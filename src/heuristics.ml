(** [informed_search goal state] compares the current state to the goal one and
    returns an int corresponding to the number of tiles in [state] that would
    differ from their expected position in [goal] *)
let informed_search size state : int =
  let nb = ref 0 in
  Immut_array.iteri
    (fun j line ->
      Immut_array.iteri
        (fun i v -> if v = i + (j * size) then nb := !nb + 1)
        line )
    state;
  !nb

let manhattan_distance _size _state : int = 0
