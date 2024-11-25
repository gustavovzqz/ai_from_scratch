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

let init_int_len size =
  let graph =
    Array.init size (fun i -> { node = { id = i; data = i }; adj_list = [] })
  in
  let name_map = Hashtbl.create size in
  let rec loop i cap =
    if i = cap
    then ()
    else (
      Hashtbl.add name_map i i;
      loop (i + 1) cap)
  in
  loop 0 (size - 1);
  { graph; name_map }
;;

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

let parse_line string =
  let string_list = String.split_on_char ' ' string in
  match string_list with
  | h1 :: h2 :: h3 :: _ ->
    int_of_string h1, int_of_string h2, float_of_string h3
  | _ -> assert false
;;

let init_by_file path =
  let file = open_in path in
  let rec read_file list =
    try
      let line = input_line file in
      let input_value = parse_line line in
      read_file (input_value :: list)
    with
    | End_of_file -> list
  in
  let values = read_file [] in
  let g = init_int_len (List.length values) in
  let rec add_edges edge_list =
    match edge_list with
    | (source, dest, weight) :: t ->
      add_edge_directed (source, dest) weight g;
      add_edges t
    | _ -> ()
  in
  add_edges values;
  g
;;
