let tablet =
  [| "" (* no clour *)
   ; "\027[0m" (* reset *)
   ; "\027[31m" (* red *)
   ; "\027[32m" (* green *)
   ; "\027[33m" (* yellow *)
   ; "\027[34m" (* blue *)
   ; "\027[35m" (* magenta *)
   ; "\027[36m" (* cyan *)
   ; "\027[37m" (* white *)
  |]

let colour_selector fmt colour = Format.fprintf fmt "%s" tablet.(colour)

let file fmt puzzle_file =
  Format.fprintf fmt "FILE:----@.%s@.----------@.@." puzzle_file

let colour = ref false

let with_colour () = colour := true

let colour_prefix fmt value =
  if !colour then
    if value = 0 then
      colour_selector fmt 2
    else
      colour_selector fmt 3
  else
    colour_selector fmt 0

let colour_suffix fmt () =
  if !colour then
    colour_selector fmt 1
  else
    colour_selector fmt 0

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
          (fun fmt i -> Format.fprintf fmt "%d" i) ) )
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

let pp_line fmt puzzle =
  let len = Immut_array.length puzzle in
  let width =
    if len <= 3 then
      1
    else if len <= 9 then
      2
    else
      3
  in
  pp_print_array
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " - ")
    (fun fmt value ->
      let t = Format.dprintf "%0*d" width value in
      Format.fprintf fmt "%a%t%a" colour_prefix value t colour_suffix () )
    fmt puzzle

let map fmt puzzle =
  pp_print_array
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_line fmt puzzle

let state state = Format.fprintf Format.std_formatter "%a@\n@\n" map state

let final state total_opened depth =
  Format.fprintf Format.std_formatter
    "Goal reached!@\n\
     %a@\n\
     Total number of opened states: %d@\n\
     Number of moves it took us   : %d@\n"
    map state total_opened depth
