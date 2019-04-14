(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Utils
open Common


let fold_sibling f acc (seq : _ Iter.t) =
  let prev = ref None in
  let acc = ref acc in
  seq (fun elt -> acc := f !acc !prev elt ; prev := Some elt)

module type S = sig

  type t
  type vertex

  module H : Hashtbl.S with type key = vertex

  val layout :
    distance:(vertex -> vertex -> float) ->
    t -> vertex -> pos H.t
end

module type TREE = sig
  type t
  module V : Hashtbl.HashedType
  val children : t -> V.t -> (V.t -> unit) -> unit
  val rev_children : t -> V.t -> (V.t -> unit) -> unit
  val rightmost_child : t -> V.t -> V.t option
  val leftmost_child : t -> V.t -> V.t option
  val is_parent : t -> parent:V.t -> child:V.t -> bool
end


(** Implementation of
    Drawing routed trees in linear time
    -- Christoph Buchheim, Michael Jünger and Sebastian Leipert

    See there for proof/details of how it works.

    Also see https://github.com/abego/treelayout for another implementation.
*)
module Make (G : TREE) = struct

  module H = Hashtbl.Make (G.V)
  let get ~default tbl x =
    try H.find tbl x
    with Not_found -> default

  let find tbl x =
    try Some (H.find tbl x)
    with Not_found -> None


  type state = {
    ancestor : G.V.t H.t ;
    thread : G.V.t H.t ;
    modtbl : float H.t ;
    prelim : float H.t ;
    change : float H.t ;
    shift : float H.t ;
    numbers : int H.t ;
    distance : G.V.t -> G.V.t -> float ;
    g : G.t ;
  }

  let get_mod s v = get ~default:0. s.modtbl v
  let get_ancestor s v = get ~default:v s.ancestor v
  let get_prelim s v = get ~default:0. s.prelim v
  let incr tbl v x =
    H.add tbl v @@
    get ~default:0. tbl v +. x

  let next_right s v =
    match G.rightmost_child s.g v with
    | Some w -> Some w
    | None -> find s.thread v

  let next_left s v =
    match G.leftmost_child s.g v with
    | Some w -> Some w
    | None -> find s.thread v

  let number ~s ~parent v =
    match find s.numbers v with
    | Some i -> i
    | None ->
      let f i v = H.add s.numbers v (i+1) in
      Iter.iteri f @@ G.children s.g parent ;
      H.find s.numbers v

  let move_subtree ~s ~parent wm wp shift =
    let subtrees = float @@ number ~s ~parent wp - number ~s ~parent wm in
    incr s.change wp @@ ~-. shift /. subtrees ;
    incr s.shift  wp @@ shift ;
    incr s.change wm @@ shift /. subtrees ;
    incr s.prelim wp @@ shift ;
    incr s.modtbl wp @@ shift ;
    ()

  let execute_shifts ~s v =
    let shift = ref 0. in
    let change = ref 0. in
    let f w =
      incr s.prelim w !shift ;
      incr s.modtbl w !shift ;
      change := !change +. get ~default:0. s.change w ;
      shift := !shift +. get ~default:0. s.shift w +. !change ;
      ()
    in
    Iter.iter f (G.rev_children s.g v) ;
    ()

  let ancestor ~s ~defaultAncestor ~parent vim =
    let child = get_ancestor s vim in
    if G.is_parent s.g ~parent ~child then child
    else defaultAncestor

  let apportion ~s ~parent ~sibling ~defaultAncestor v =
    match sibling with
    | None -> defaultAncestor
    | Some w ->
      let vip = ref v and vop = ref v
      and vim = ref w in
      let vom = ref @@ Opt.get @@ G.leftmost_child s.g parent in
      let sip = ref @@ get_mod s !vip and sop = ref @@ get_mod s !vop
      and sim = ref @@ get_mod s !vim and som = ref @@ get_mod s !vom in
      let rec aux () = match next_right s !vim, next_left s !vip with
        | Some vim_, Some vip_ ->
          vim := vim_ ;
          vip := vip_ ;
          vom := Opt.get @@ next_left s !vom ;
          vop := Opt.get @@ next_right s !vop ;
          H.add s.ancestor !vop v ;
          let shift =
            (get_prelim s !vim +. !sim)
            -. (get_prelim s !vip +. !sip)
            +. s.distance !vim !vip
          in
          if shift > 0. then begin
            move_subtree ~s ~parent
              (ancestor ~s ~defaultAncestor ~parent !vim) v shift ;
            sip := !sip +. shift ;
            sop := !sop +. shift ;
          end ;
          sip := !sip +. get_mod s !vip ;
          sop := !sop +. get_mod s !vop ;
          som := !som +. get_mod s !vom ;
          sim := !sim +. get_mod s !vim ;
          aux ()
        | _ -> ()
      in
      aux () ;

      begin match next_right s !vim, next_right s !vop with
        | Some vim_, None ->
          H.add s.thread !vop vim_ ;
          H.add s.modtbl !vop @@ get_mod s !vop +. !sim -. !sop ;
        | _ -> ()
      end ;

      begin match next_left s !vip, next_left s !vom with
        | Some vip_, None ->
          H.add s.thread !vom vip_ ;
          H.add s.modtbl !vom @@ get_mod s !vom +. !sip -. !som ;
          v
        | _ -> defaultAncestor
      end

  let rec first_walk ~s ~sibling v =
    match G.leftmost_child s.g v with
    | None -> begin match sibling with
        | None -> ()
        | Some w ->
          H.add s.prelim v @@ get_prelim s w +. s.distance v w
      end
    | Some vl -> begin
        let vr = Opt.get @@ G.rightmost_child s.g v in
        fold_sibling
          (fun defaultAncestor sibling w ->
             first_walk ~s ~sibling w ;
             apportion ~s ~parent:v
               ~defaultAncestor
               ~sibling w)
          vl (G.children s.g v) ;
        execute_shifts ~s v ;
        let midpoint =
          ((get_prelim s vl) +. (get_prelim s vr)) /. 2.
        in
        match sibling with
        | Some w ->
          H.add s.prelim v @@ get_prelim s w +. s.distance v w ;
          H.add s.modtbl v @@ get_prelim s v -. midpoint ;
        | None ->
          H.add s.prelim v midpoint ;
      end

  let rec second_walk s result level v m =
    let x = get_prelim s v +. m in
    let y = float level in
    H.add result v {x;y} ;
    let f w = second_walk s result (level+1) w (m +. get_mod s v) in
    Iter.iter f (G.children s.g v) ;
    ()

  let layout ~distance g r =
    let modtbl = H.create 17 in
    let thread = H.create 17 in
    let ancestor = H.create 17 in
    let prelim = H.create 17 in
    let change = H.create 17 in
    let shift = H.create 17 in
    let result = H.create 17 in
    let numbers = H.create 17 in
    let s =
      { prelim ; modtbl ; thread ; ancestor ; change ; shift ; numbers ;
        distance ; g ; }
    in
    first_walk ~s ~sibling:None r ;
    second_walk s result 0 r (-. (H.find prelim r)) ;
    result

end

module type I = Hashtbl.HashedType
module type VIEW = sig
  type t
  val children : t -> t array
end

let array_rev_iter f a =
  let last = Array.length a - 1 in
  for i = last downto 0 do f a.(i) done
module MakeTree (V : I) (T : VIEW with type t = V.t)  = struct

  type t = unit
  module V = V

  let children () a k = Array.iter k @@ T.children a

  let rev_children () a k = array_rev_iter k @@ T.children a

  let leftmost_child () a =
    let a = T.children a in
    let l = Array.length a in
    if l = 0 then None else Some a.(0)

  let rightmost_child () a =
    let a = T.children a in
    let l = Array.length a in
    if l = 0 then None else Some a.(l - 1)

  let is_parent () ~parent ~child =
    Array.exists (V.equal child) @@ T.children parent
end

let layout
    (type a)
    ?(m:(module I with type t = a) option)
    ~children
    : distance:_ -> _ -> _ 
  =
  let (module I) = match m with
    | Some m -> m
    | None ->
      (module (struct type t = a let equal = (=) let hash = Hashtbl.hash end))
  in
  let module X = struct
    type t = a
    let children = children
  end
  in
  let module T = MakeTree(I)(X) in
  let module L = Make(T) in
  fun ~distance t ->
    let h = L.layout ~distance () t in
    L.H.find h
