let map_to_lists puzzle =
  Immut_array.map Immut_array.to_list puzzle |> Immut_array.to_list

let display_inversions fmt inversions =
  Format.fprintf fmt "INVERSIONS:@.%a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
       (fun fmt i -> Format.fprintf fmt "%d" i) )
    inversions

let is_solvable (size, puzzle) =
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
  let inversions_nb = List.fold_left ( + ) 0 in
  let values = List.flatten @@ map_to_lists puzzle in

  (* if size if odd, then puzzle instance is solvable if number of inversions is even in the input state *)
  if size mod 2 <> 0 then (
    let inversions = map_inversions [] values in
    display_inversions Format.std_formatter inversions;

    let nb = inversions_nb inversions in
    Format.printf "Nb of inversions in initial grid is: %d@." nb;
    nb mod 2 = 0
  ) else
    (* if size is even, puzzle instance is solvable if :
     * - the blank in on an EVEN row counting from the bottom AND number of inversions is ODD
     *
     * OR
     *
     * - the blank is on an ODD row counting from the bottom AND number of inversions is EVEN
     * source: https://www.geeksforgeeks.org/check-instance-15-puzzle-solvable/
     * *)
    false
