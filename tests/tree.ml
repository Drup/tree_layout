
type tree = Node of tree * int * tree | Leaf of int

module T = struct

  type t = unit
  module V = struct
    type t = tree
    let equal = (=)
    let hash = Hashtbl.hash
  end

  let succ () v k = match v with
    | Leaf _ -> ()
    | Node (t1, _, t2) -> k t1 ; k t2

  let rev_succ () v k = match v with
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

include Tree_layout.Raw.Make(T)
open Svg

let line (x1,y1) (x2,y2) =
  M.(path ~a:[
      a_d @@ Printf.sprintf {|M%f,%i L%f,%i|} x1 y1 x2 y2 ;
      a_stroke (`Color ("black", None)) ; a_strokewidth (0.03, None) ;
      a_fill `None ;
    ] [])

let circle i (x,y) =
  M.[
    circle ~a:[a_cx (x, None) ; a_cy (float y, None) ; a_r (0.2, None)][] ;
    text ~a:[
      a_x_list [x, None] ; a_y_list [float y, None] ;
      a_text_anchor `Middle ; a_fontsize "0.2" ;
      a_fill (`Color ("white", None)) ;
    ] [pcdata @@ string_of_int i] ;
  ]

let node, leaf =
  let r = ref 0 in
  let node t1 t2 = incr r ; Node (t1, !r, t2) in
  let leaf () = incr r ; Leaf !r in
  node, leaf

let rec make n =
  if n <= 2 then leaf ()
  else
    let i = 1 + Random.int ((n-1)/2) in
    node (make i) (make (n-i-1))

let distance _ _ = 1.
let tree = Random.self_init () ; make 30
let h = tree_layout ~distance () tree

let rec to_svg t =
  let pos = H.find h t in
  match t with
  | Leaf i -> circle i pos
  | Node (t1, i, t2) ->
    line pos (H.find h t1) ::
    line pos (H.find h t2) ::
    circle i pos @
    to_svg t1 @
    to_svg t2

let () =
  let file = open_out Sys.argv.(1) in
  let doc =
    M.(svg ~a:[
        a_width (700., Some `Px) ; a_height (700., Some `Px) ;
        a_viewbox (-3., -1., 10., 10.) ;
      ]) (to_svg tree)
  in
  P.print ~output:(output_string file) doc
