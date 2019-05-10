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
module T = Tree_layout

let rec area = function
  | T.Node (x,[||]) -> x.width *. x.height *. 2.
  | T.Node (_,a) ->
    (Iter.sumf @@ Iter.map area @@ Iter.of_array a)
let rect_of_tree t : Tree_layout.Common.rectangle =
  let a = area t in
  { p = { x = 0. ; y = 0. } ; w = sqrt a; h = sqrt a }

let layout t =
  let rect = rect_of_tree t in
  T.treemap
    ~m:(module Utils.Info)
    ~area
    rect
    (Iter.singleton t)

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

  let tree = gen 100 in
  let h = layout tree in
  let file = Format.formatter_of_out_channel @@ open_out out in
  let doc = Output.treemap seed (rect_of_tree tree) h in
  Format.fprintf file "%a@." (Tyxml.Svg.pp ()) doc
