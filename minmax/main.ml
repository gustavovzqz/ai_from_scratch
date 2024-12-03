type player =
  | Min
  | Max
[@@deriving show]

type 'a state =
  { state : 'a
  ; player : player
  }
[@@deriving show]

type 'a game =
  { is_terminal : 'a state -> bool
  ; utility : 'a state -> float
  ; actions : 'a state -> 'a state list
  }
[@@deriving show]

let _print_state fmt (a, b, c) =
  Format.fprintf fmt "(a: %d, b: %d, c: %d)" a b c
;;

let min_of l =
  let rec iter_min list min =
    match list with
    | [] -> min
    | h :: t when h = min -> iter_min t h
    | _ :: t -> iter_min t min
  in
  iter_min l infinity
;;

let max_of l =
  let rec iter_max list max =
    match list with
    | [] -> max
    | h :: t when h > max -> iter_max t h
    | _ :: t -> iter_max t max
  in
  iter_max l (-.infinity)
;;

let rec minimax g s =
  if g.is_terminal s
  then g.utility s
  else (
    match s.player with
    | Min -> min_of (List.map (minimax g) (g.actions s))
    | Max -> max_of (List.map (minimax g) (g.actions s)))
;;

let () =
  let filter_moves =
    List.filter (fun { state; player = _ } ->
      let _a, b, _c = state in
      b >= 0)
  in
  let transition { state; player } =
    let a, b, c = state in
    match player with
    | Max ->
      filter_moves
        (List.init 3 (fun i ->
           let i = i + 1 in
           { state = a + i, b - i, c; player = Min }))
    | Min ->
      filter_moves
        (List.init 3 (fun i ->
           let i = i + 1 in
           { state = a, b - i, c + i; player = Max }))
  in
  let is_terminal_state { state; player = _ } =
    let _a, b, _c = state in
    b = 0
  in
  let utility_s { state = _; player } =
    match player with
    | Min -> -1.
    | Max -> 1.
  in
  let initial_state_1 = { state = 0, 9, 0; player = Min } in
  let initial_state_2 = { state = 0, 5, 0; player = Min } in
  let initial_state_3 = { state = 0, 4, 0; player = Min } in
  let initial_state_4 = { state = 0, 3, 0; player = Max } in
  let game =
    { is_terminal = is_terminal_state
    ; utility = utility_s
    ; actions = transition
    }
  in
  Printf.printf "Resultado: %f\n" (minimax game initial_state_1);
  Printf.printf "Resultado: %f\n" (minimax game initial_state_2);
  Printf.printf "Resultado: %f\n" (minimax game initial_state_3);
  Printf.printf "Resultado: %f\n" (minimax game initial_state_4)
;;
