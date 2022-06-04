module Movement = struct
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

  let movements = [ move_up; move_down; move_left; move_right ]
end

let matrix_get p x y =
  let line = Immut_array.get p y in
  Immut_array.get line x

let matrix_set p x y v =
  let line = Immut_array.get p y in
  let line = Immut_array.set line x v in
  Immut_array.set p y line

let permut p (x1, y1) (x2, y2) =
  let v1 = matrix_get p x1 y1 in
  let v2 = matrix_get p x2 y2 in
  let p = matrix_set p x2 y2 v1 in
  matrix_set p x1 y1 v2

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

let solved = ref false

let rec solve_brutforce seen_states size goal state =
  if Hashtbl.mem seen_states state then
    ()
  else begin
    Hashtbl.add seen_states state ();
    ( if state = goal then (
      solved := true;
      Format.printf "YES HAPPY ME:@\n%a@\n" Pp.map state
    ) else
      let children = get_children size state in
      List.iter
        (fun child ->
          if not !solved then solve_brutforce seen_states size goal child )
        children );
    if !solved then Format.printf "%a@\n@\n" Pp.map state
  end

let astar seen_states size state goal _hfunc =
  let _children = get_children size state in
  solve_brutforce seen_states size goal state

let solve_with heuristic_f size state =
  let seen_states = Hashtbl.create 512 in
  let goal = Generate.goal size in
  let hfunc = heuristic_f size in
  astar seen_states size state goal hfunc
