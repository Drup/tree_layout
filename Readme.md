# Tree_layout [![Build Status](https://travis-ci.org/Drup/tree_layout.svg?branch=master)](https://travis-ci.org/Drup/tree_layout) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]



![A tree](https://drup.github.io/tree_layout/layered_tree.svg)

Algorithms to layout trees in a pretty manner.

```ocaml
open Tree_layout

(* Given a well groomed tree module, ... *)
module Tree : Layered.TREE = ...

(* a tree, ... *)
let tree : Tree.t = ...

(* and a distance function. *)
let distance v1 v2 = ...

(* Get positions ! *)
let positions = Layered.Make(Tree).layout ~distance tree root
```

Currently, only layered layouts are implemented. The [algorithm][] is linear in the size of the tree

[algorithm]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.16.8757

## Install

```
opam install tree_layout
```

[doc]: https://drup.github.io/tree_layout/dev/Tree_layout.html


