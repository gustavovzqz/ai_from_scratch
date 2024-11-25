open Ai_from_scratch

let () =
  let g = Graph.init_by_file "graph.txt" in
  let path = Search.dijkstra g (Graph.get_node 0 g) (Graph.get_node 32786 g) in
  List.iter (fun i -> Printf.printf "%d " i) path;
  print_endline ""
;;
