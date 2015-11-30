
type tree =
  | Node of tree * int * tree
  | Leaf of int
let label (Leaf i | Node (_,i,_)) = i

module T = struct

  type t = unit
  module V = struct
    type t = tree
    let equal t1 t2 = label t1 = label t2
    let hash t = Hashtbl.hash (label t)
  end

  let children () v k = match v with
    | Leaf _ -> ()
    | Node (t1, _, t2) -> k t1 ; k t2

  let rev_children () v k = match v with
    | Leaf _ -> ()
    | Node (t1, _, t2) -> k t2 ; k t1

  let leftmost_child () = function
    | Leaf _ -> None
    | Node (t, _, _) -> Some t

  let rightmost_child () = function
    | Leaf _ -> None
    | Node (_, _, t) -> Some t

  let is_parent () ~parent ~child = match parent with
    | Leaf _ -> false
    | Node (t1, _, t2) -> V.equal t1 child || V.equal t2 child

end

module L = Tree_layout.Make(T)

let distance _ _ = 1.
let node, leaf =
  let r = ref 0 in
  let node t1 t2 = incr r ; Node (t1, !r, t2) in
  let leaf () = incr r ; Leaf !r in
  node, leaf

let rec make n =
  if n <= 2 then leaf ()
  else
    let i = 1 + Random.int (n-1) in
    node (make i) (make (n-i))

let benchs =
  Random.self_init () ;
  List.map
    (fun i -> (string_of_int i, L.layered ~distance (), make i))
    [ 1000 ; 5000 ; 10000 ; 20000 ; 30000 ; 50000 ]

open Benchmark
let () =
  let res = throughputN ~repeat:3 3 benchs in
  print_newline();
  tabulate res
