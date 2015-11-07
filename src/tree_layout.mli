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

module type S = sig

  type graph
  type vertex

  module H : Hashtbl.S with type key = vertex

  val tree_layout :
    distance:(vertex -> vertex -> float) ->
    graph -> vertex -> (float * int) H.t
end

module Raw : sig

  module type TREE = sig
    type t
    module V : Hashtbl.HashedType
    val succ : t -> V.t -> (V.t -> unit) -> unit
    val rev_succ : t -> V.t -> (V.t -> unit) -> unit
    val rightmost_child : t -> V.t -> V.t option
    val leftmost_child : t -> V.t -> V.t option
    val is_parent : t -> parent:V.t -> child:V.t -> bool
  end

  module Make (G : TREE) :
    S with type graph := G.t
       and type vertex := G.V.t

end
