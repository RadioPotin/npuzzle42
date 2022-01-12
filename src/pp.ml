
let file fmt puzzle_file =
  Format.fprintf fmt "FILE:----@.%s@.----------@." puzzle_file

let pp_puzzle fmt puzzle =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
        (fun fmt row ->
          Format.fprintf fmt "%a"
            (Format.pp_print_list
              ~pp_sep:(fun fmt ()-> Format.fprintf fmt "-")
              (fun fmt n -> Format.fprintf fmt "%d" n)
            ) row
        )
    ) puzzle

let map fmt (size, puzzle) =
  let l = Immut_array.map Immut_array.to_list puzzle in
  let puzzle = Immut_array.to_list l in
  Format.fprintf fmt "< %d >@.%a@." size pp_puzzle puzzle
