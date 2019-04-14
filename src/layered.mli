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


(** Layered trees

    A layered tree is a tree that is organized by layers: the horizontal position
    of a node is fixed depending on its depth in the tree, 
    regardless of its height.

    {%html: <a href="https://drup.github.io/tree_layout/layered_tree.svg"><img style="margin : 0 auto; display: block; max-width:90%" src="https://drup.github.io/tree_layout/layered_tree.svg" /></a> %}


{[
(* Given a nice tree, ... *)
let tree : Tree.t = ...

(* and a distance function. *)
let distance v1 v2 = ...

(* Get positions ! *)
let positions =
  Tree_layout.Layered.layout
    ~children:Tree.children
    ~distance
    tree
]}
*)

open Common

(** [layout ~children ~distance g v] returns the layered layout for
    the tree [g] rooted in [v]. Layered layout are such that
    vertices with the same depth have the same vertical coordinate.
    The layout is returned as a lookup functions from trees to positions.

    This algorithm is in linear time if [children] is constant time.
    Use {!Make} for a more flexible implementation.

    [distance v1 v2] should return the horizontal distance
    between [v1] and [v2] placed at the same depth.

    @param m An hashing specification for the tree type. If not provided, polymorphic comparison and hashing are used.
    @param children Return all the subtrees of a tree.

    @see <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.16.8757>
    Improving Walker's Algorithm to Run in Linear Time
*)
val layout: 
  ?m:(module Hashtbl.HashedType with type t = 'a) ->
  children:('a -> 'a array) ->
  distance:('a -> 'a -> float) -> 'a -> ('a -> Common.pos)

(** {1 Functorized API} *)

(** The output signature for the layered layout engine. *)
module type S = sig

  type t
  (** A tree *)

  type vertex
  (** A vertex of the tree. *)

  module H : Hashtbl.S with type key = vertex

  val layout :
    distance:(vertex -> vertex -> float) ->
    t -> vertex -> pos H.t
    (** Same as {!Tree_layout.Layered.layout}, but with the specified
        implementation. 

        @return An Hashtable which contains one binding per
        accessible vertex in [g].
    *)
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

(** Full implementation. 
    If the operations in {!TREE} are O(1), the layout functions is O(n).
*)
module Make (G : TREE) :
  S with type t := G.t
     and type vertex := G.V.t
