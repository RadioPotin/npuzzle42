(** [informed_search goal state] compares the current state to the goal one and
    returns an int corresponding to the number of tiles that should have to move
    from their current location in order to make it to the ordered goal state *)
let informed_search state : int =
  let score = ref 0 in
  let size = Immut_array.length state in
  Immut_array.iteri2 (fun j i v -> if v = i + (j * size) then incr score) state;
  !score

(** [manhattan_distance size state] compares the coordinates (x, y) of each tile
    in the current state to their would-be absolute position in the goal state
    and returns the sum of all the non-matching coordinates to establish which
    state would have the fewest amount of displaced tiles *)
let manhattan_distance state : int =
  let score = ref 0 in
  let size = Immut_array.length state in
  let normal_coordinates v = (v / size, v mod size) in
  Immut_array.iteri2
    (fun j i v ->
      let nj, ni = normal_coordinates v in
      let manj, mani = (abs (j - nj), abs (i - ni)) in
      score := !score + manj + mani )
    state;
  - !score

let nb_of_inversions puzzle : int =
  let values = Utils.map_to_lists puzzle in
  -(snd @@ Utils.get_inversions values)

let given = ref manhattan_distance

let select = function
  | "man"
  | "manhattan"
  | "Man"
  | "Manhattan"
  | "manhattan_distance"
  | "Manhattan_distance" ->
    let s = Format.sprintf "Using Manhattan_distance!@." in
    Pp.colour_wrap Format.std_formatter (7, s);
    given := manhattan_distance
  | "inf"
  | "informed"
  | "inf_search"
  | "informed_search" ->
    let s = Format.sprintf "Using informed_search!@." in
    Pp.colour_wrap Format.std_formatter (7, s);
    given := informed_search
  | "inv"
  | "inversions"
  | "Inversions"
  | "inversion_nb"
  | "Inversions_nb" ->
    let s = Format.sprintf "Using number of inversions!@." in
    Pp.colour_wrap Format.std_formatter (7, s);
    given := nb_of_inversions
  | _s -> Format.printf "Unknown heuristic, check usage for more information."
