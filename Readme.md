# Tree_layout

![A tree](https://drup.github.io/tree_layout/layered_tree.svg)

Algorithms to layout trees in a pretty manner.

```ocaml
(* Given a well groomed tree module, ... *)
module Tree : Tree_layout.TREE = ...

(* a tree, ... *)
let tree : Tree.t = ...

(* and a distance function. *)
let distance v1 v2 = ...

(* Get positions ! *)
let positions = Tree_layout.Make(Tree).layered ~distance tree root
```

Currently, only layered layouts are implemented. The [algorithm][] is linear in the size of the tree

[algorithm]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.16.8757

## Install

with opam: `opam install tree_layout`

Otherwise: `./configure && make && make install`
