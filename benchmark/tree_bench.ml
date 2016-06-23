module L = Tree_layout.Layered.Make(Rose)

let distance _ _ = 1.

let benchs =
  Random.self_init () ;
  List.map
    (fun i -> (string_of_int i, L.layout ~distance (), Rose.gen i))
    [ 1000 ; 5000 ; 10000 ; 20000 ; 30000 ]

open Benchmark
let () =
  let res = throughputN ~repeat:3 4 benchs in
  print_newline();
  tabulate res
