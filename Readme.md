# Tree_layout [![Build Status](https://travis-ci.org/Drup/tree_layout.svg?branch=master)](https://travis-ci.org/Drup/tree_layout) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]

![A tree](https://drup.github.io/tree_layout/layered_tree.svg)

Algorithms to layout trees in a pretty manner.

Currently support layered trees and treemaps.
See the [documentation][doc] for details.
An easy-to-use [rose tree][rose] API is provided by default:

```ocaml
(* Given a tree, ... *)
let tree : _ Tree_layout.tree = ...

(* and a distance function. *)
let distance v1 v2 = ...

(* Get annotated tree ! *)
let annotated_tree =
  Tree_layout.layered ~distance tree
```

## Install

```
opam install tree_layout
```

[doc]: https://drup.github.io/tree_layout/dev/
[rose]: https://en.wikipedia.org/wiki/Rose_tree
