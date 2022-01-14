let map_to_lists puzzle =
  Immut_array.map Immut_array.to_list puzzle |> Immut_array.to_list

let display_inversions fmt inversions nb =
  Format.fprintf fmt "INVERSIONS:@.%a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
       (fun fmt i -> Format.fprintf fmt "%d" i) )
    inversions;
  Format.printf "Nb of inversions in initial grid is: %d@." nb

let is_solvable fmt (size, puzzle) =

  let is_even nb = nb mod 2 = 0 in
  let rec map_inversions acc = function
    | [] -> List.rev acc
    | value :: r ->
      map_inversions
        ( List.fold_left
            (fun acc v ->
              if (not (v = 0)) && value > v then
                acc + 1
              else
                acc )
            0 r
        :: acc )
        r
  in

  (* Functions used *)
  let blank_location_from_bottom f map = f @@ List.rev map in
  let is_on_even_row map =
    match
      List.filteri
        (fun i row -> List.exists (fun v -> v = 0) row && is_even i)
        map
    with
    | [] -> false
    | _l -> true
  in

  let inversions_nb = List.fold_left ( + ) 0 in
  let values = map_to_lists puzzle in
  let inversions = map_inversions [] (List.flatten values) in
  let inv_nb = inversions_nb inversions in

  if size mod 2 <> 0 then (
    (* if size is odd, then puzzle instance is solvable if number of inversions is even in the input state *)
    display_inversions fmt inversions inv_nb;
    inv_nb mod 2 = 0
  ) else (
    (* if size is even, puzzle instance is solvable if :
     * - the blank in on an EVEN row counting from the bottom AND number of inversions is ODD
     *
     * OR
     *
     * - the blank is on an ODD row counting from the bottom AND number of inversions is EVEN
     * source: https://www.geeksforgeeks.org/check-instance-15-puzzle-solvable/
     * *)
    display_inversions fmt inversions inv_nb;
    (blank_location_from_bottom is_on_even_row values && (not @@ is_even inv_nb))
    || (not @@ blank_location_from_bottom is_on_even_row values)
       && is_even inv_nb
  )
