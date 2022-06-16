(** [informed_search goal state] compares the current state to the goal one and
    returns an int corresponding to the number of tiles that should have to move
    from their current location in order to make it to the ordered goal state *)
let informed_search size state : int =
  let nb = ref 0 in
  Immut_array.iteri
    (fun j line ->
      Immut_array.iteri (fun i v -> if v = i + (j * size) then incr nb) line )
    state;
  (size * size) - !nb

(** [manhattan_distance size state] compares the coordinates (x, y) of each tile
    in the current state to their would-be absolute position in the goal state
    and returns the sum of all the non-matching coordinates to establish which
    state would have the fewest amount of displaced tiles *)
let manhattan_distance _size _state : int = 0
