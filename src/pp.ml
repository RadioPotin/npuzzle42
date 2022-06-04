let file fmt puzzle_file =
  Format.fprintf fmt "FILE:----@.%s@.----------@.@." puzzle_file

let pp_puzzle fmt puzzle =
  Format.fprintf fmt "%a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt row ->
         Format.fprintf fmt "%a"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
              (fun fmt n -> Format.fprintf fmt "%d" n) )
           row ) )
    puzzle

let display_inversions fmt inversions_l nb =
  Format.fprintf fmt "INVERSIONS:@.%a@."
    (Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
        (fun fmt i -> Format.fprintf fmt "%d" i) ))
    inversions_l;
  Format.printf "Nb of inversions in grid is: %d@." nb

let pp_print_array ?(pp_sep = Format.pp_print_cut) pp_v ppf a =
  let len = Immut_array.length a in
  if len > 0 then begin
    pp_v ppf (Immut_array.get a 0);
    for i = 1 to len - 1 do
      pp_sep ppf ();
      pp_v ppf (Immut_array.get a i)
    done
  end

let pp_line fmt line =
    pp_print_array ~pp_sep:(fun fmt () -> Format.fprintf fmt " - ")
    Format.pp_print_int fmt line


let map fmt puzzle =
  pp_print_array ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_line fmt puzzle
