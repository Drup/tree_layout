(** Squarified Treemaps

    Treemaps represent trees as nested rectangles. A subtree is a larger
    rectangle that contains all its children. Squarified treemaps
    are treemaps which attempts to keep rectangles as square as possible.
    Functions in this module expect a function to compute the area of a node, and
    return sequences of pair of nodes and rectangles.

    {%html: <a href="https://drup.github.io/tree_layout/treemap.svg"><img style="margin : 0 auto; display: block; max-width:60%" src="https://drup.github.io/tree_layout/treemap.svg" /></a> %}
*)


open Common

val layout : 
  area:('a -> float) ->
  children:('a -> 'a Sequence.t) ->
  rectangle -> 'a -> ('a * rectangle) Sequence.t
(** [layout ~area ~children rect tree] computes a squarified treemap of [tree]
    and return a sequence of subtrees and their positions.

    @param area Return the area of a subtree.
    @param children Return all the subtrees of a tree.
*)

val squarify :
  area:('a -> float) ->
  rectangle -> 'a Sequence.t -> ('a * rectangle) Sequence.t
(** [squarify ~area rect l] takes a sequence of elements [l] and 
    decorate them with positions.

    @param area Return the area of a subtree.
*)

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
