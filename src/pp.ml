let file fmt puzzle_file =
  Format.fprintf fmt "FILE:----@.%s@.----------@." puzzle_file

let pp_puzzle fmt puzzle =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (fun fmt row ->
         Format.fprintf fmt "%a"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
              (fun fmt n -> Format.fprintf fmt "%d" n) )
           row ) )
    puzzle

let map fmt (size, puzzle) =
  let puzzle = Utils.map_to_lists puzzle in
  Format.fprintf fmt "< %d >@.%a@." size pp_puzzle puzzle
