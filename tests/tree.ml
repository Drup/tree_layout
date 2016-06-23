type info = {label : int ; w : float ; h : float }

type tree = Node of info * tree array | Leaf of info

let array_rev_iter f a =
  let last = Array.length a - 1 in
  for i = last downto 0 do f a.(i) done

module T = struct

  type t = unit
  module V = struct
    type t = tree
    let equal = (=)
    let hash = Hashtbl.hash
  end

  let children () v k = match v with
    | Leaf _ -> ()
    | Node (_, a) -> Array.iter k a

  let rev_children () v k = match v with
    | Leaf _ -> ()
    | Node (_, a) -> array_rev_iter k a

  let leftmost_child () = function
    | Leaf _ -> None
    | Node (_, a) -> Some a.(0)

  let rightmost_child () = function
    | Leaf _ -> None
    | Node (_, a) -> Some a.(Array.length a - 1)

  let is_parent () ~parent ~child = match parent with
    | Leaf _ -> false
    | Node (_, a) -> Array.exists (V.equal child) a

end

module L = Tree_layout.Make(T)

module Gen = struct

  let label label =
    { label ; w = 0.5 +. Random.float 1. ; h = 0.8 }

  let node, leaf =
    let r = ref 0 in
    let node a = incr r ; Node (label !r, a) in
    let leaf () = incr r ; Leaf (label !r) in
    node, leaf

  let rec make n =
    if n <= 1 then leaf ()
    else
      let l = 1 + Random.int (min 4 (n-1)) in
      let a = Array.init l (fun _ -> make ((n-1)/l)) in
      node a

end

let width (Leaf info | Node (info,_)) = info.w
let distance l1 l2 = width l1 /. 2. +. 0.2 +. width l2 /. 2.

module Output = struct
  module M = Tyxml.Svg
  open Tree_layout

  let line p1 p2 =
    M.(path ~a:[
        a_d @@ Printf.sprintf {|M%f,%f L%f,%f|} p1.x p1.y p2.x p2.y ;
        a_stroke (`Color ("black", None)) ; a_stroke_width (0.06, None) ;
        a_fill `None ;
      ] [])

  let rect info {x;y} =
    M.[
      rect ~a:[
        a_x (x -. info.w/.2., None) ; a_y (y -. info.h/.2., None) ;
        a_width (info.w, None) ; a_height (info.h, None) ;
        a_stroke_width (0.03, None) ;
        a_style "fill:#4860E8;stroke:black"
      ][] ;
      text ~a:[
        a_x_list [x, None] ; a_y_list [y, None] ;
        a_text_anchor `Middle ; a_font_size "0.4" ;
      ] [pcdata @@ string_of_int info.label] ;
    ]

  let list_map_array f a = List.map f @@ Array.to_list a
  let list_flatmap_array f a =
    List.concat @@ list_map_array f a

  let rec svg_shapes h t =
    let pos = L.H.find h t in
    match t with
    | Leaf info -> rect info pos
    | Node (info, a) ->
      list_flatmap_array (svg_shapes h) a @ rect info pos

  let rec svg_lines h t =
    let pos = L.H.find h t in
    match t with
    | Leaf _ -> []
    | Node (_, a) ->
      list_map_array (fun x -> line pos @@ L.H.find h x) a
      @ list_flatmap_array (svg_lines h) a

  let doc seed h t =
    let pos, size = L.boundaries ~margins:{x=1.;y=1.} h in
    M.(svg ~a:[
        a_width (1200., Some `Px) ; a_height (700., Some `Px) ;
        a_viewBox (pos.x, pos.y, size.x, size.y) ;
      ] (
        title (pcdata @@ Printf.sprintf "Tree layout -- Seed: %i" seed)::
        svg_lines h t @ svg_shapes h t
      ))
end

let () =
  Random.self_init () ;
  let seed = Random.int (1 lsl 29) in
  Random.init seed ;
  Printf.printf "Seed : %i\n" seed ;

  let tree = Gen.make 100 in
  let h = L.layered ~distance () tree in
  let file = Format.formatter_of_out_channel @@ open_out Sys.argv.(1) in
  let doc = Output.doc seed h tree in
  Format.fprintf file "%a@." (Tyxml.Svg.pp ()) doc
