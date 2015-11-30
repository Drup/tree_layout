type info = {label : int ; w : float ; h : float }

type tree = Node of tree * info * tree | Leaf of info

module T = struct

  type t = unit
  module V = struct
    type t = tree
    let equal = (=)
    let hash = Hashtbl.hash
  end

  let children () v k = match v with
    | Leaf _ -> ()
    | Node (t1, _, t2) -> k t1 ; k t2

  let rev_children () v k = match v with
    | Leaf _ -> ()
    | Node (t1, _, t2) -> k t2 ; k t1

  let leftmost_child () = function
    | Leaf _ -> None
    | Node (t, _, _) -> Some t

  let rightmost_child () = function
    | Leaf _ -> None
    | Node (_, _, t) -> Some t

  let is_parent () ~parent ~child = match parent with
    | Leaf _ -> false
    | Node (t1, _, t2) -> V.equal t1 child || V.equal t2 child

end

module L = Tree_layout.Make(T)

module Gen = struct

  let label label =
    { label ; w = 0.5 +. Random.float 1. ; h = 0.8 }

  let node, leaf =
    let r = ref 0 in
    let node t1 t2 = incr r ; Node (t1, label !r, t2) in
    let leaf () = incr r ; Leaf (label !r) in
    node, leaf

  let rec make n =
    if n <= 1 then leaf ()
    else
      let i = 1 + Random.int (n-1) in
      node (make i) (make (n-i))

end

let width (Leaf info | Node (_,info,_)) = info.w
let distance l1 l2 = width l1 /. 2. +. 0.2 +. width l2 /. 2.

module Output = struct
  open Svg
  open Tree_layout

  let line p1 p2 =
    M.(path ~a:[
        a_d @@ Printf.sprintf {|M%f,%f L%f,%f|} p1.x p1.y p2.x p2.y ;
        a_stroke (`Color ("black", None)) ; a_strokewidth (0.06, None) ;
        a_fill `None ;
      ] [])

  let rect info {x;y} =
    M.[
      rect ~a:[
        a_x (x -. info.w/.2., None) ; a_y (y -. info.h/.2., None) ;
        a_width (info.w, None) ; a_height (info.h, None) ;
        a_strokewidth (0.03, None) ;
        a_style "fill:#4860E8;stroke:black"
      ][] ;
      text ~a:[
        a_x_list [x, None] ; a_y_list [y, None] ;
        a_text_anchor `Middle ; a_fontsize "0.4" ;
      ] [pcdata @@ string_of_int info.label] ;
    ]

  let rec svg_shapes h t =
    let pos = L.H.find h t in
    match t with
    | Leaf info -> rect info pos
    | Node (t1, info, t2) ->
      svg_shapes h t1 @
        svg_shapes h t2 @
        rect info pos

  let rec svg_lines h t =
    let pos = L.H.find h t in
    match t with
    | Leaf _ -> []
    | Node (t1, _, t2) ->
      line pos (L.H.find h t1) ::
        line pos (L.H.find h t2) ::
        svg_lines h t1 @
        svg_lines h t2

  let doc seed h t =
    let pos, size = L.boundaries ~margins:{x=1.;y=1.} h in
    M.(svg ~a:[
        a_width (1200., Some `Px) ; a_height (700., Some `Px) ;
        a_viewbox (pos.x, pos.y, size.x, size.y) ;
      ] (
        title (pcdata @@ Printf.sprintf "Tree layout -- Seed: %i" seed)::
        svg_lines h t @ svg_shapes h t
      ))
end

let () =
  Random.self_init () ;
  let seed = Random.int (1 lsl 29) in
  Random.init seed ;

  let tree = Gen.make 50 in
  let h = L.layered ~distance () tree in
  let file = open_out Sys.argv.(1) in
  let doc = Output.doc seed h tree in
  Svg.P.print ~output:(output_string file) doc ;
  close_out file
