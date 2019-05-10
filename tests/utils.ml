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

module T = Tree_layout

type info = {label : int ; width : float ; height : float }

module Info = struct
  type t = info T.tree
  let equal (T.Node (a,_)) (T.Node (b, _)) = a.label = b.label
  let hash (T.Node (a,_)) = Hashtbl.hash a.label
end

module Gen = struct

  let label label =
    { label ; width = 0.5 +. Random.float 1.5 ; height = 0.8 }

  let node, leaf =
    let r = ref 0 in
    let node a = incr r ; T.Node (label !r, a) in
    let leaf () = incr r ; T.Node (label !r, [||]) in
    node, leaf

  let rec make_split ~split total =
    if split <= 1 || total <= 1 then [total]
    else
      let i = 1 + Random.int (total-1) in
      i :: make_split ~split (total - i)
  
  let rec make n =
    if n <= 1 then leaf ()
    else
      let split = max 1 (Random.int n) in
      let splits = make_split ~split (n-1) in
      let a = List.map make splits  in
      node (Array.of_list a)

end

let gen = Gen.make

module Output = struct
  module M = Tyxml.Svg
  open Tree_layout.Common

  let line p1 p2 =
    M.(path ~a:[
        a_d @@ Printf.sprintf {|M%f,%f L%f,%f|} p1.x p1.y p2.x p2.y ;
        a_stroke (`Color ("black", None)) ; a_stroke_width (0.06, None) ;
        a_fill `None ;
      ] [])

  let rect ?label { p ; w ; h } =
    M.[
      rect ~a:[
        a_x (p.x, None) ; a_y (p.y, None) ;
        a_width (w, None) ; a_height (h, None) ;
        a_stroke_width (0.03, None) ;
        a_style
          (if label = None then "fill:transparent;stroke:black"
           else "fill:#4860E8;stroke:black")
      ][]
    ] @
    match label with
    | None -> []
    | Some label ->
      M.[text ~a:[
          a_x_list [p.x +. w/.2., None] ; a_y_list [p.y+.h/.2., None] ;
          a_text_anchor `Middle; a_dy_list [0.4, Some `Em];
          a_font_size "0.4" ;
        ] [txt @@ string_of_int label] ;
        ]
  let rect_of_info info {x;y} =
    rect ~label:info.label
      { p = { x = x -. info.width/.2. ; y = y -. info.height/.2. } ;
        w = info.width ;
        h = info.height ;
      }

  let list_map_array f a = List.map f @@ Array.to_list a
  let list_flatmap_array f a =
    List.concat @@ list_map_array f a

  let rec svg_shapes t =
    match t with
    | T.Node ((info, pos), [||]) -> rect_of_info info pos
    | Node ((info, pos), a) ->
      list_flatmap_array (svg_shapes) a @ rect_of_info info pos

  let rec svg_lines t =
    match t with
    | T.Node (_, [||]) -> []
    | Node ((_,pos), a) ->
      let aux (T.Node ((_,pos'),_)) = line pos pos' in
      list_map_array aux a
      @ list_flatmap_array (svg_lines) a

  let viewbox_of_rect { p ; w ; h } = M.a_viewBox (p.x, p.y, w, h)

  let rec iter_tree f (T.Node (x,a)) = f @@ snd x ; Array.iter (iter_tree f) a
  let tree seed t =
    let r =
      boundaries ~margins:{x=1.;y=1.} @@ fun k -> iter_tree k t
    in
    M.(svg ~a:[
        a_width (1200., Some `Px) ; a_height (700., Some `Px) ;
        viewbox_of_rect r ;
      ] (
        title (txt @@ Printf.sprintf "Tree layout -- Seed: %i" seed)::
        svg_lines t @
        svg_shapes t
      ))

  let rec svg_rects t =
    match t with
    | T.Node ((info,r), [||]) -> rect ~label:info.label r
    | Node ((_info,r), a) ->
      list_flatmap_array (svg_rects) a @ rect r

  let treemap seed r t =
    M.(svg ~a:[
        a_width (1200., Some `Px) ; a_height (700., Some `Px) ;
        viewbox_of_rect r ;
      ] (
        title (txt @@ Printf.sprintf "Tree layout -- Seed: %i" seed)::
        (List.flatten @@ Iter.to_list @@ Iter.map svg_rects t)
      ))
    
end
