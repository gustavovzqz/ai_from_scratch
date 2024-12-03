type margin = int * int [@@deriving show]

(* canibais, missionarios *)

type boat_pos =
  | Left
  | Right
[@@deriving show]

type problem =
  { boat_side : boat_pos
  ; left_margin : margin
  ; right_margin : margin
  }
[@@deriving show]

let update_margin problem canib miss =
  match problem.boat_side with
  | Left ->
    let canib_left, miss_left = problem.left_margin in
    let canib_right, miss_right = problem.right_margin in
    let new_left_margin = canib_left - canib, miss_left - miss
    and new_right_margin = canib_right + canib, miss_right + miss in
    { boat_side = Right
    ; left_margin = new_left_margin
    ; right_margin = new_right_margin
    }
  | Right ->
    let canib_left, miss_left = problem.left_margin in
    let canib_right, miss_right = problem.right_margin in
    let new_left_margin = canib_left + canib, miss_left + miss
    and new_right_margin = canib_right - canib, miss_right - miss in
    { boat_side = Left
    ; left_margin = new_left_margin
    ; right_margin = new_right_margin
    }
;;

let filter_pair (canib, miss) =
  match canib, miss with
  | _, _ when canib < 0 || miss < 0 -> false
  | _, 0 -> true
  | _, _ -> miss >= canib
;;

let filter_valid problem =
  filter_pair problem.left_margin && filter_pair problem.right_margin
;;


let expand_problem problem =
  let update_margin = update_margin problem in
  let p1 = update_margin 0 1
  and p2 = update_margin 0 2
  and p3 = update_margin 1 1
  and p4 = update_margin 1 0
  and p5 = update_margin 2 0 in
  let problem_list = [ p1; p2; p3; p4; p5 ] in
  List.iter
    (fun p ->
      Printf.printf
        "Verificando estado: %s -> %b\n"
        (show_problem p)
        (filter_valid p))
    problem_list;
  List.filter filter_valid problem_list
;;

let rec find_solution states visited =
  match states with
  | [] -> print_endline "Sem solução encontrada!"
| (h, _path) :: t when List.exists (fun s  -> 
    s.left_margin = h.left_margin &&
    s.right_margin = h.right_margin &&
    s.boat_side = h.boat_side) visited -> find_solution t visited
  | (h, path) :: t ->
    Printf.printf "Chamando para %s\n" (show_problem h);
    if h.left_margin = (0, 0)
    then (
      print_endline "Encontrado!, exibindo caminho:";
      List.iter
        (fun s -> Printf.printf "%s\n" (show_problem s))
        (List.rev (h :: path)))
    else (
      let new_states = List.map (fun s -> s, h :: path) (expand_problem h) in
      find_solution (t @ new_states) (h :: visited))
;;

let () =
  let problem = { boat_side = Left; left_margin = 4, 4; right_margin = 0, 0 } in
  find_solution [ problem, [] ] []
;;
