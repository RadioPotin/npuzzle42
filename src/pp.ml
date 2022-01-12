
let file fmt puzzle_file =
  Format.fprintf fmt "FILE:----@.%s@.----------@." puzzle_file

let maparray fmt (map:int Parray.t Parray.t) =
 Parray.iter
   ( fun int_arr ->
     let len = Parray.length int_arr in
     if len > 0 then
       begin
       Format.pp_print_int fmt (Parray.get int_arr 0);
       for i = 1 to len - 1 do
         Format.fprintf fmt "-";
         Format.pp_print_int fmt (Parray.get int_arr i)
       done
               end
     ) map

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
  Format.fprintf fmt "< %d >@.%a@." size pp_puzzle puzzle
