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

  let movements = [ move_up; move_down; move_left; move_right ]
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

(** [solve_brutforce seen_states size goal state] is self explanatory. This
    function Stack Overflows for puzzles of size 4 onward. This function was
    useful in order to grasp the problem first hand *)
let rec solve_brutforce seen_states size goal state =
  (* Things this function does NOT do:
   *
   * keep track of depth
   * run heuristic on current state
   * order children by heuristic cost
   * correctly check g(n) + h(n)
   *
   * *)
  if Hashtbl.mem seen_states state then
    ()
  else begin
    Hashtbl.add seen_states state ();
    ( if state = goal then begin
      solved := true;
      Format.printf "The BRUTE is done:@\n%a@\n------@\n" Pp.map state
    end else
      let children = get_children size state in
      List.iter
        (fun child ->
          if not !solved then solve_brutforce seen_states size goal child )
        children );
    (* This will print states in reverse order of resolution,
     * since its located after the recursive call *)
    if !solved then Format.printf "%a@\n@\n" Pp.map state
  end

(*
 *
 * TODO
 * - Properly handle Hashtbl storing with g value.
 * - Properly remember chosen states for later PP
 *
 * Astar aims at reducing f(n) when f(n) = g(n) + h(n)
 * n = current node
 * g = depth
 * h = heuristic cost estimation
 *
 * Meaning that, when choosing next state to expand,
 * we must remember all nodes visited (with their corresponding depth)
 * and choose in regards to all we have seen before.
 *
 * Storing in the hashtabl ONLY states that we have CHOSEN in our path
 *
 * *)
let astar seen_states size state goal hfunc =
  let rec expand state g =
    if Hashtbl.mem seen_states state then
      ()
    else begin
      Hashtbl.add seen_states state ();
      ( if state = goal then begin
        solved := true;
        Format.printf "half assed code say What ?@\n%a@\n------What ?@\n" Pp.map
          state
      end else
        let unfiltered_children = get_children size state in
        let children =
          List.sort (fun child1 child2 ->
              compare (hfunc child1 + g) (hfunc child2 + g) )
          @@ List.filter_map
               (fun child ->
                 if Hashtbl.mem seen_states child then
                   None
                 else
                   Some child )
               unfiltered_children
        in
        List.iter
          (fun child -> if not !solved then expand child (g + 1))
          children );
      (* This will print states in reverse order of resolution,
       * since its located after the recursive call *)
      if !solved then Format.printf "%a@\n@\n" Pp.map state
    end
  in
  expand state 0

(** [solve_with heuristic_f size state] main astar solver function. Takes an
    heuristic, size and initial state. *)
let solve_with heuristic_f size state =
  let seen_states = Hashtbl.create 512 in
  let goal = Generate.goal size in
  let hfunc = heuristic_f size in
  astar seen_states size state goal hfunc
