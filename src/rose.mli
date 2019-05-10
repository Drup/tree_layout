(** 
This module is intended to be an easy-to-use entry point of the library.
Users who would like to use their own tree data-structures can consult the
various other modules and instantiate their own layout functions.
*)

open Common

type 'a tree =
    Node of 'a * 'a tree array
(** A n-ary tree. 

    For correct usage in the next functions, 
    the labels of the nodes should always contain
    a unique identifier.
*)

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
  ?sub:(rectangle -> rectangle) ->
  area:('a tree -> float) ->
  Common.rectangle -> 'a tree Iter.t ->
  ('a * Common.rectangle) tree Iter.t
