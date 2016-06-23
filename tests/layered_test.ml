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
module L = Tree_layout.Layered.Make(Rose)
module O = Rose.Output (L.H)
let width (Leaf info | Node (info,_)) = info.w
let distance l1 l2 = width l1 /. 2. +. 0.2 +. width l2 /. 2.

let () =
  Random.self_init () ;
  let seed = Random.int (1 lsl 29) in
  Random.init seed ;
  Printf.printf "Seed : %i\n" seed ;

  let tree = Rose.gen 100 in
  let h = L.layout ~distance () tree in
  let file = Format.formatter_of_out_channel @@ open_out Sys.argv.(1) in
  let doc = O.doc seed h tree in
  Format.fprintf file "%a@." (Tyxml.Svg.pp ()) doc
