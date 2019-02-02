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

open Rose
module H = Hashtbl.Make(Rose.V)
module O = Rose.Output (H)
let rec area = function
  | Leaf x -> x.width *. x.height *. 2.
  | Node (_, a) ->
    Sequence.sumf @@ Sequence.map area @@ Sequence.of_array a
      
let cmp x y = compare (area x) (area y)
let children t =
  Sequence.sort ~cmp @@ children () t

let layout t =
  let h = H.create 17 in
  let k (x, p) = H.add h x p in
  let a = area t in
  let rect : Tree_layout.Common.rectangle =
    { p = { x = 0. ; y = 0. } ; w = sqrt a; h = sqrt a }
  in
  Tree_layout.Treemaps.layout
    ~area
    ~children
    rect
    t
    k ;
  h

let () =
  let out =
    if Array.length Sys.argv >= 2 then
      Sys.argv.(1)
    else
      "/tmp/treemap.svg"
  in
  
  Random.self_init () ;
  let seed = Random.int (1 lsl 29) in
  Random.init seed ;
  Printf.printf "Seed : %i\n" seed ;

  let tree = Rose.gen 100 in
  let h = layout tree in
  let file = Format.formatter_of_out_channel @@ open_out out in
  let doc = O.treemap seed h tree in
  Format.fprintf file "%a@." (Tyxml.Svg.pp ()) doc
