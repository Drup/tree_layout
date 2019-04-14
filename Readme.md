# Tree_layout [![Build Status](https://travis-ci.org/Drup/tree_layout.svg?branch=master)](https://travis-ci.org/Drup/tree_layout) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]



![A tree](https://drup.github.io/tree_layout/layered_tree.svg)

Algorithms to layout trees in a pretty manner.
Currently support layered trees and treemaps.
See the [documentation][doc] for details.

```ocaml
(* Given a well groomed tree module, ... *)
module Tree = ...

(* a tree, ... *)
let tree : Tree.t = ...

(* and a distance function. *)
let distance v1 v2 = ...

(* Get positions ! *)
let positions =
  let module L = Tree_layout.Layered.Make(Tree) in
  L.layout ~distance tree root
```

## Install

```
opam install tree_layout
```

[doc]: https://drup.github.io/tree_layout/dev/


