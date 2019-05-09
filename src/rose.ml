
type 'a tree =
    Node of 'a * 'a tree array
let children (Node (_, a)) = a

module type I = Hashtbl.HashedType 
module Lift (I : I) = struct
  type t = I.t tree
  let equal (Node (v1,_)) (Node (v2,_)) = I.equal v1 v2
  let hash (Node (v,_)) = I.hash v
end

let layered_lookup
    (type a)
    ?(m:(module I with type t = a) option)
    =
  let (module I) = match m with
    | Some m -> m
    | None ->
      (module (struct type t = a let equal = (=) let hash = Hashtbl.hash end))
  in
  let module T = Lift(I) in
  Layered.layout
    ~m:(module T) ~children

let rec decorate f (Node (i,a) as t) =
  Node ((i, f t), Array.map (decorate f) a)
let layered ?m =
  let f = layered_lookup ?m in
  fun ~distance t ->
    let lookup = f ~distance t in
    decorate lookup t

let treemap_iter ?sub ~area =
  let children (Node (_, a)) k = Array.iter k a in
  Treemaps.layout ?sub
    ~children ~area

let rec decorate f (Node (i,a)) =
  Node ((i, f i), Array.map (decorate f) a)
let treemap 
    (type a)
    ?(m:(module I with type t = a) option)
    =
  let (module I) = match m with
    | Some m -> m
    | None ->
      (module (struct type t = a let equal = (=) let hash = Hashtbl.hash end))
  in
  let module H = Hashtbl.Make(I) in
  let hash_of_iter k =
    let h = H.create 17 in
    k (fun (Node (v, _), r) -> H.add h v r) ;
    h
  in
  fun ?sub ~area r t -> 
    let h = hash_of_iter @@ treemap_iter ?sub ~area r t in
    decorate (H.find h) t
  


(*
 * Copyright (c) 2019 Gabriel Radanne <drupyog@zoho.com>
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
