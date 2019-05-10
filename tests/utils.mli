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

(** Simple rose trees that fullfill most signatures with the right complexity. *)

open Tree_layout

type info = {label : int ; width : float ; height : float }

module Info : Hashtbl.HashedType with type t = info tree

(** Randomly generate a tree with [n] nodes. *)
val gen : int -> info tree

module Output : sig
  val tree : int -> (info * Common.pos) tree -> Tyxml.Svg.doc
  val treemap :
    int -> 
    Common.rectangle ->
    (info * Common.rectangle) tree Iter.t ->
    Tyxml.Svg.doc
end
