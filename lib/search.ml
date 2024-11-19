module Heap = Pairing_heap
open Graph

let cmp (cost1, _) (cost2, _) =
  if cost1 < cost2 then -1 else if cost1 > cost2 then 1 else 0
;;

let reconstruct_path predecessors goal graph =
  let rec aux current_id path =
    match current_id with
    | None -> path
    | Some v ->
      let node = graph.(v).node.data in
      aux predecessors.(v) (node :: path)
  in
  aux (Some goal.id) []
;;

let best_first_search (graph_ : 'a Graph.t) start goal priority_function =
  let graph = graph_.graph in
  let graph_size = Array.length graph in
  let pq = Heap.create ~cmp () in
  let costs = Array.make graph_size infinity
  and predecessors = Array.make graph_size None
  and visited = Array.make graph_size false
  and keys = Array.make graph_size None
  and not_reached = ref true in
  keys.(start.id)
  <- Some (Heap.add_removable pq (priority_function start 0., start));
  costs.(start.id) <- 0.;
  while (not (Heap.is_empty pq)) && !not_reached do
    let _, current_node = Heap.pop_exn pq in
    let current_node_id = current_node.id in
    if visited.(current_node_id)
    then ()
    else (
      visited.(current_node_id) <- true;
      if current_node = goal
      then not_reached := false
      else
        List.iter
          (fun { neigh_id; weight } ->
            let current_cost = costs.(current_node_id) in
            let tentative_cost = current_cost +. weight in
            let neigh = graph.(neigh_id).node in
            if tentative_cost < costs.(neigh_id)
            then (
              predecessors.(neigh_id) <- Some current_node_id;
              costs.(neigh_id) <- tentative_cost;
              match keys.(neigh_id) with
              | None ->
                keys.(neigh_id)
                <- Some
                     (Heap.add_removable
                        pq
                        (current_cost +. priority_function neigh weight, neigh))
              | Some k ->
                keys.(neigh_id)
                <- Some
                     (Heap.update
                        pq
                        k
                        (current_cost +. priority_function neigh weight, neigh))))
          graph.(current_node_id).adj_list)
  done;
  reconstruct_path predecessors goal graph
;;

let a_star graph start goal heuristics =
  let priority_function node weight = heuristics node +. weight in
  best_first_search graph start goal priority_function
;;

let gbfs graph start goal heuristics =
  (* gbfs ignores the weight *)
  let priority_function node _weight = heuristics node +. 0. in
  best_first_search graph start goal priority_function
;;

let dijkstra graph start goal =
  (* dijkstra ignores the heuristics *)
  let priority_function _node weight = weight in
  best_first_search graph start goal priority_function
;;
