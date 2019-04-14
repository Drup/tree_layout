(** 
This module is intended to be an easy-to-use entry point of the library.
Users who would like to use their own tree data-structures can consult the
various other modules and instantiate their own layout functions.
*)

type 'a tree =
    Node of 'a * 'a tree array

(** Layered trees

    See {!Tree_layout.Layered} for details.
*)
val layered : 
  ?m:(module Hashtbl.HashedType with type t = 'a) ->
  distance:('a tree -> 'a tree -> float) ->
  'a tree ->
  ('a * Common.pos) tree

(** Treemaps 

    See {!Tree_layout.Treemaps} for details.
*)
val treemap :
  ?m:(module Hashtbl.HashedType with type t = 'a) ->
  area:('a tree -> float) ->
  Common.rectangle -> 'a tree ->
  ('a * Common.rectangle) tree
