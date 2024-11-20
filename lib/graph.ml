type 'a node =
  { id : int
  ; data : 'a
  }

type edge =
  { neigh_id : int
  ; weight : float
  }

type 'a adj_list =
  { node : 'a node
  ; mutable adj_list : edge list
  }

type 'a t =
  { graph : 'a adj_list array
  ; name_map : ('a, int) Hashtbl.t
  }

(* TODO: Check for duplicates *)
let init (info_list : 'a list) : 'a t =
  let info_array = Array.of_list info_list in
  let info_size = Array.length info_array in
  let graph =
    Array.init info_size (fun i ->
      { node = { id = i; data = info_array.(i) }; adj_list = [] })
  in
  let name_map = Hashtbl.create info_size in
  List.iteri (fun i data -> Hashtbl.add name_map data i) info_list;
  { graph; name_map }
;;

(* TODO: Parse functions to get a graph by a file *)

let get_index (node_data : 'a) (graph_struct : 'a t) : int =
  try Hashtbl.find graph_struct.name_map node_data with
  | Not_found -> failwith "Node not found"
;;

let get_node (node_data : 'a) (graph_struct : 'a t) : 'a node =
  try
    let id = Hashtbl.find graph_struct.name_map node_data in
    graph_struct.graph.(id).node
  with
  | Not_found -> failwith "Node not found"
;;

let add_edge_directed (source_data, dest_data) weight (graph_struct : 'a t) =
  try
    let source_index = Hashtbl.find graph_struct.name_map source_data in
    let dest_index = Hashtbl.find graph_struct.name_map dest_data in
    let source_node = graph_struct.graph.(source_index) in
    let edge = { neigh_id = dest_index; weight } in
    source_node.adj_list <- edge :: source_node.adj_list
  with
  | Not_found -> failwith "Node not found"
;;

let add_edge (source_data, dest_data) weight (graph_struct : 'a t) =
  add_edge_directed (source_data, dest_data) weight graph_struct;
  add_edge_directed (dest_data, source_data) weight graph_struct
;;
