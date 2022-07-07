module Movement = struct
  (** This Movement module is used when deriving children states from a given
      state. Each function takes a pair of coordinates (x, y) and a max value.
      Based on the coordinates, each function will either return Some (x, y)
      position or None. *)
  let move_left (x, y) _max_x =
    if x = 0 then
      None
    else
      Some (x - 1, y)

  let move_right (x, y) max_x =
    if x = max_x - 1 then
      None
    else
      Some (x + 1, y)

  let move_up (x, y) _max_y =
    if y = 0 then
      None
    else
      Some (x, y - 1)

  let move_down (x, y) max_y =
    if y = max_y - 1 then
      None
    else
      Some (x, y + 1)

  let movements = [ move_up; move_right; move_down; move_left ]
end

(** [matrix_get p x y] returns the element of array [p] at coordinates [x] [y]*)
let matrix_get p x y =
  let line = Immut_array.get p y in
  Immut_array.get line x

(** [matrix_set p x y v] returns a new puzzle with a value [v] set at
    coordinates [x] [y] *)
let matrix_set p x y v =
  let line = Immut_array.get p y in
  let line = Immut_array.set line x v in
  Immut_array.set p y line

(** [permut p (x1, y1) (x2, y2)] takes a puzzle and two positions and return a
    new puzzle with the tile values at each positions permuted *)
let permut p (x1, y1) (x2, y2) =
  let v1 = matrix_get p x1 y1 in
  let v2 = matrix_get p x2 y2 in
  let p = matrix_set p x2 y2 v1 in
  matrix_set p x1 y1 v2

(** [get_blank puzzle] returns a pair of coordinates (x, y) corresponding to the
    position of the blank ([0]) tile in the puzzle passed as parameter*)
let get_blank puzzle =
  let pos = ref None in
  Immut_array.iteri
    (fun j line ->
      Immut_array.iteri
        (fun i value -> if value = 0 then pos := Some (i, j))
        line )
    puzzle;
  match !pos with
  | None -> assert false
  | Some pos -> pos

(** [get_children size state] returns a list of possible states to expand. This
    function locates the blank tile, then maps a list of valid movements based
    on that information and applies these movements to the current state of the
    puzzle and returns a list of states. This function is unaware of the states
    already visited by the algorithm, they must be filtered after the call to
    this function in order NOT cycle. *)
let get_children size state =
  let pos = get_blank state in
  let possible_movements =
    List.filter_map (fun f -> f pos size) Movement.movements
  in
  let change_current_state_with given_movement =
    let state = permut state pos given_movement in
    state
  in
  List.map change_current_state_with possible_movements

(** [solved] is a boolean flag used to assert if the algorithm has reached a
    valid state at some point in its traversal *)
let solved = ref false

(** [is_solved current goal] temporary function used to assert if a current
    state is equivalent to the goal state established early in the program. This
    function will be modified as the program improves. Its necessity isn't quite
    proven yet *)
let is_solved current =
  let last = ref 0 in
  try
    Immut_array.iter2
      (fun n ->
        if n < !last then raise Exit;
        last := n )
      current;
    true
  with
  | Exit -> false

module Saucisse = Utils.Saucisse

(** [astar seen_states size state score] A* function. Sets up environment and
    data structure and recursively expands all puzzle states until end is
    reached. It tracks the depth of the algorithm, the total amount of
    opened_states considered during the search. *)
let astar seen_states size state score =
  let score g n = score n - g in
  let map = Saucisse.empty in
  let map = Saucisse.push (state, 0, None) (score 0 state) map in

  let rec expand total_opened max map acc =
    match Saucisse.take_max map with
    | None -> assert false
    | Some (((state, depth, parent) as s), _, map2) ->
      if is_solved state then begin
        solved := true;
        Hashtbl.replace seen_states state parent;
        (s :: acc, total_opened, depth, max)
      end else (
        Hashtbl.replace seen_states state parent;

        let children = get_children size state in
        let children =
          List.filter
            (fun state -> not @@ Hashtbl.mem seen_states state)
            children
        in
        let map =
          List.fold_left
            (fun map child ->
              let chair = (child, depth + 1, Some state) in
              Saucisse.push chair (score (depth + 1) child) map )
            map2 children
        in

        let total = total_opened + List.length children in
        let max =
          if total > max then
            total
          else
            max
        in
        expand total max map (s :: acc)
      )
  in
  let trace_back_path_to_goal path =
    let child, _depth, parent = List.hd path in
    let rec trace_back acc child opt_parent_of_child =
      match opt_parent_of_child with
      | None -> child :: acc
      | Some parent ->
        let parent_of_parent = Hashtbl.find seen_states parent in
        trace_back (child :: acc) parent parent_of_parent
    in
    trace_back [] child parent
  in
  let reversed_expansion_path, total_opened, depth, max = expand 0 0 map [] in
  let actual_path = trace_back_path_to_goal reversed_expansion_path in
  let rec print_path = function
    | [] -> ()
    | [ state ] -> Pp.final state total_opened depth max
    | state :: r ->
      Pp.state state;
      print_path r
  in
  print_path actual_path

(** [solve_with heuristic_f size state] main astar solver function. Takes an
    heuristic, size and initial state. *)
let solve_with heuristic size initial_state =
  let seen_states = Hashtbl.create 512 in
  let solving = Format.sprintf "Solving... " in
  let be_patient =
    Format.sprintf "This could take a moment, please be patient...@."
  in
  Pp.colour_wrap Format.std_formatter (7, solving);
  Heuristics.select heuristic;
  Pp.colour_wrap Format.std_formatter (6, be_patient);
  let heuristic_f = !Heuristics.given in
  astar seen_states size initial_state heuristic_f
