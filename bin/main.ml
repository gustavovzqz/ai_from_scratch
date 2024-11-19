open Ai_from_scratch
open Graph

let g =
  Graph.init
    [ "Arad"
    ; "Bucharest"
    ; "Craiova"
    ; "Dobreta"
    ; "Eforie"
    ; "Fagaras"
    ; "Giurgiu"
    ; "Hirsova"
    ; "Iasi"
    ; "Lugoj"
    ; "Mehadia"
    ; "Neamt"
    ; "Oradea"
    ; "Pitesti"
    ; "Rimnicu Vilcea"
    ; "Sibiu"
    ; "Timisoara"
    ; "Urziceni"
    ; "Vaslui"
    ; "Zerind"
    ]
;;

let heuristics =
  [| 366.0
   ; 0.0
   ; 160.0
   ; 242.0
   ; 161.0
   ; 176.0
   ; 77.0
   ; 151.0
   ; 226.0
   ; 244.0
   ; 241.0
   ; 234.0
   ; 380.0
   ; 100.0
   ; 193.0
   ; 253.0
   ; 329.0
   ; 80.0
   ; 199.0
   ; 374.0
  |]
;;

let () =
  Graph.add_edge ("Arad", "Zerind") 75.0 g;
  Graph.add_edge ("Zerind", "Oradea") 71.0 g;
  Graph.add_edge ("Oradea", "Sibiu") 151.0 g;
  Graph.add_edge ("Sibiu", "Arad") 140.0 g;
  Graph.add_edge ("Arad", "Timisoara") 118.0 g;
  Graph.add_edge ("Timisoara", "Lugoj") 111.0 g;
  Graph.add_edge ("Lugoj", "Mehadia") 70.0 g;
  Graph.add_edge ("Mehadia", "Dobreta") 75.0 g;
  Graph.add_edge ("Dobreta", "Craiova") 120.0 g;
  Graph.add_edge ("Craiova", "Rimnicu Vilcea") 146.0 g;
  Graph.add_edge ("Rimnicu Vilcea", "Sibiu") 80.0 g;
  Graph.add_edge ("Sibiu", "Fagaras") 99.0 g;
  Graph.add_edge ("Fagaras", "Bucharest") 211.0 g;
  Graph.add_edge ("Bucharest", "Pitesti") 101.0 g;
  Graph.add_edge ("Pitesti", "Rimnicu Vilcea") 97.0 g;
  Graph.add_edge ("Craiova", "Pitesti") 138.0 g;
  Graph.add_edge ("Pitesti", "Bucharest") 101.0 g;
  Graph.add_edge ("Bucharest", "Giurgiu") 90.0 g;
  Graph.add_edge ("Bucharest", "Urziceni") 85.0 g;
  Graph.add_edge ("Urziceni", "Hirsova") 98.0 g;
  Graph.add_edge ("Hirsova", "Eforie") 86.0 g;
  Graph.add_edge ("Urziceni", "Vaslui") 142.0 g;
  Graph.add_edge ("Vaslui", "Iasi") 92.0 g;
  Graph.add_edge ("Iasi", "Neamt") 87.0 g;
  let h node = heuristics.(node.id) in
  let path1 = Search.dijkstra g g.graph.(0).node g.graph.(1).node in
  let path2 = Search.gbfs g g.graph.(0).node g.graph.(1).node h in
  let path3 = Search.a_star g g.graph.(0).node g.graph.(1).node h in
  List.iter (Printf.printf "%s ") path1;
  print_endline "";
  List.iter (Printf.printf "%s ") path2;
  print_endline "";
  List.iter (Printf.printf "%s ") path3;
  print_endline ""
;;
