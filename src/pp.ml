
let pp_file fmt puzzle_file =
    Format.fprintf fmt "FILE:----@.%s@.----------@." puzzle_file

  let map fmt (size, puzzle) =
    Format.fprintf fmt "SIZE: < %d >@.%a@." size
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
            (fun fmt n -> Format.pp_print_int fmt n)
         )) puzzle
