open Types

module Movement = struct

let move_left (x, y) _max_index =
  if x = 0 then None else Some (x - 1, y)

let move_right (x, y) max_index =
  if x = max_index then None else Some (x + 1, y)

let move_up (x, y) _max_index =
  if y = 0 then None else Some (x, y - 1)

let move_down (x, y) max_index =
  if y = max_index then None else Some (x, y + 1)

let movements = [move_up; move_down; move_left; move_right]

end

(** [map_tiles puzzle] turns the [int Immut_array.t Immut_array.t] puzzle representation into a [Types.tile list] allowing*)
let map_tiles puzzle =
  let t_arr =
    Immut_array.mapi
      (fun y row ->
          Immut_array.mapi
            (fun x v -> {value = v; pos = x, y})
            row)
      puzzle
  in
  Immut_array.map Immut_array.to_list t_arr
  |> Immut_array.to_list
  |> List.flatten

let pop l = List.hd l

let get_children state =
  let blank_tile =
    List.hd @@ List.filter
      (fun tile -> tile.value = 0)
      state.tiles
  in
  let movements =
    List.map (fun f -> f blank_tile.pos (state.size - 1)) Movement.movements
  in
  let possible_movements =
    List.filter Option.is_none movements
  in possible_movements
    (* WIP
     *
     * apply list of possible movements to current state to get puzzle of children
     * then convert to puzzles to state types
     * Run heuristic, order by cost
     * return children
     * *)

let expand state _hfunc =
  let _children = get_children state in
  ()
  (* WIP
   * Get children,
   * close current state
   * add children to priority cue, IE, openedstates cue
   * *)

let astar hfunc initial_state =
  if initial_state.h_cost = 0 then
    [initial_state]
  else
    let state = initial_state in
    let _opened = expand state hfunc in
    []

let solve_with heuristic_f ((size, puzzle) as map)=
  let goal = Generate.goal map in
  let goal = map_tiles goal in
  let tiles = map_tiles puzzle in
  let heuristic_f = heuristic_f goal in
  let initial_state =
    {
      puzzle;
      size;
      tiles;
      g_cost = 0;
      h_cost = heuristic_f tiles;
    }
  in astar heuristic_f initial_state
