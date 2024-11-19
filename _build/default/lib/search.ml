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

let a_star (graph_ : 'a Graph.t) start goal h =
  let graph = graph_.graph in
  let graph_size = Array.length graph in
  let pq = Heap.create ~min_size:graph_size ~cmp () in
  let costs = Array.make graph_size infinity
  and predecessors = Array.make graph_size None
  and visited = Array.make graph_size false
  and keys = Array.make graph_size None
  and not_reached = ref true in
  keys.(start.id) <- Some (Heap.add_removable pq (h start, start));
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
            let tentative_cost = costs.(current_node_id) +. weight in
            let neigh = graph.(neigh_id).node in
            if tentative_cost < costs.(neigh_id)
            then (
              predecessors.(neigh_id) <- Some current_node_id;
              costs.(neigh_id) <- tentative_cost;
              match keys.(neigh_id) with
              | None ->
                keys.(neigh_id)
                <- Some
                     (Heap.add_removable pq (tentative_cost +. h neigh, neigh))
              | Some k ->
                keys.(neigh_id)
                <- Some (Heap.update pq k (tentative_cost +. h neigh, neigh))))
          graph.(current_node_id).adj_list)
  done;
  reconstruct_path predecessors goal graph
;;
