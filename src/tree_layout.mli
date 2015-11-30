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

(** Algorithms to layout trees in a pretty manner. *)

(** A position (or a size). *)
type pos = { x : float ; y : float }

(** The output signature for a certain layout engine. *)
module type S = sig

  type t
  (** A tree *)

  type vertex
  (** A vertex of the tree. *)

  module H : Hashtbl.S with type key = vertex

  (** [tree_layout ~distance g v] returns the layered layout for
      the tree [g] rooted in [v]. Layered layout are such that
      vertices with the same depth have the same vertical coordinate.

      [distance v1 v2] should return the horizontal distance
      between [v1] and [v2] placed at the same depth.

      The returned hash table contains one binding per
      accessible vertex in [g].

      @see <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.16.8757>
      Improving Walker's Algorithm to Run in Linear Time
  *)
  val layered :
    distance:(vertex -> vertex -> float) ->
    t -> vertex -> pos H.t

  (** [boundaries ~margins positions] returns a pair [(pos, size)]
      defining a rectangle containing the positions in [positions].

      The option argument [margins] add a margin around the rectangle.
  *)
  val boundaries :
    ?margins:pos -> pos H.t -> pos * pos
end

(** The input signature for {!Make} *)
module type TREE = sig
  type t
  module V : Hashtbl.HashedType
  val children : t -> V.t -> (V.t -> unit) -> unit
  val rev_children : t -> V.t -> (V.t -> unit) -> unit
  val rightmost_child : t -> V.t -> V.t option
  val leftmost_child : t -> V.t -> V.t option
  val is_parent : t -> parent:V.t -> child:V.t -> bool
end

(** Define layout engines.

    If the operations in {!TREE} are O(1), the layout functions are O(n).
*)
module Make (G : TREE) :
  S with type t := G.t
     and type vertex := G.V.t
