(** [tablet] is an array of colours to use for the colouring of output. *)
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

let heurislist fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    (fun fmt hfunc -> Format.fprintf fmt "%s" hfunc)
    fmt l

(** [file fmt puzzle_file puzzle_name] Prints the name and contents of the map
    file passed as a parameter to the program. *)
let file fmt puzzle_file puzzle_name =
  Format.fprintf fmt "Reading map from '%s'@\n@\nMAP:@\n%s@\n@\n" puzzle_name
    puzzle_file

(** [colour] boolean reference for colour flag in cli. *)
let colour = ref false

(** [with_colour ()] raises [colour] flag. *)
let with_colour () = colour := true

(** [colour_prefix fmt colour] function that prints colour control sequences in
    the shell using the [tablet] array if necessary. *)
let colour_prefix fmt color =
  if !colour then
    Format.fprintf fmt "%s" tablet.(color)
  else
    ()

(** [colour_suffix fmt ()] closes a colour control sequence or nothing if
    [colour] flag isn't raised. *)
let colour_suffix fmt () =
  if !colour then
    colour_prefix fmt 1
  else
    colour_prefix fmt 0

(** [colour_value fmt value] function used specifically for pretty-printing the
    map at each level of the search. Takes colours the tiles by differentiating
    the 0 value and the others, or nothing if [colour] flag isn't set. *)
let colour_value fmt value =
  if !colour then
    if value = 0 then
      colour_prefix fmt 2
    else
      colour_prefix fmt 3
  else
    colour_prefix fmt 0

(** [colour_wrap fmt (colour, string)] wraps calls to [colour_prefix] to
    pretty-print the string passed as argument with the colour passed as
    argument in the tuple parameter. *)
let colour_wrap fmt (colour, s) =
  Format.fprintf fmt "%a%s%a" colour_prefix colour s colour_suffix ()

let not_solvable () =
  let s = Format.sprintf "ERROR: Your map is unsolvable! >:C" in
  Format.fprintf Format.std_formatter "%a@\n" colour_wrap (1, s);
  exit 1

(** [pp_puzzle fmt puzzle] prints a map. *)
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

(** [display_inversions fmt inversions_l nb] Function mainly used for debugging.
    This allows the display of pre-calculated inversions found in a map. *)
let display_inversions fmt inversions_l nb =
  Format.fprintf fmt "INVERSIONS:@.%a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
          (fun fmt i -> Format.fprintf fmt "%d" i) ) )
    inversions_l;
  Format.printf "Nb of inversions in grid is: %d@." nb

(** [pp_puzzle fmt puzzle] prints an array. *)
let pp_print_array ?(pp_sep = Format.pp_print_cut) pp_v ppf a =
  let len = Immut_array.length a in
  if len > 0 then begin
    pp_v ppf (Immut_array.get a 0);
    for i = 1 to len - 1 do
      pp_sep ppf ();
      pp_v ppf (Immut_array.get a i)
    done
  end

(** [pp_line fmt puzzle] prints an array and pads the width of each tile output
    for pretty-printing. Checks the value of each tile for colouring if
    necessary. *)
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
      Format.fprintf fmt "%a%t%a" colour_value value t colour_suffix () )
    fmt puzzle

(** [map fmt puzzle] prints arrays of arrays and pads the width of each tile
    output for pretty-printing. Checks the value of each tile for colouring if
    necessary. *)
let map fmt puzzle =
  pp_print_array
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_line fmt puzzle

(** [state state] debugging function to print each state expanded during the
    search. *)
let state state = Format.fprintf Format.std_formatter "%a@\n@\n" map state

(** [final state total_opened depth] Pretty prints the result of the search with
    all relevant information. *)
let final state total_opened depth max expansion_path_length =
  let goal_reached = Format.sprintf "Goal reached!@." in
  let total_number =
    Format.sprintf "Total number of states selected from the opened set:"
  in
  let steps = Format.sprintf "Number of moves it took us:" in
  let maximum_states =
    Format.sprintf "Max number of states allocated in memory:"
  in
  let expansion_path = Format.sprintf "Search explored a path of length:" in
  Format.fprintf Format.std_formatter
    "%a@\n%a@\n%a %#d@\n%a %#d@\n%a %#d@\n%a %#d@\n" map state colour_wrap
    (5, goal_reached) colour_wrap (4, steps) depth colour_wrap
    (4, expansion_path) expansion_path_length colour_wrap (4, maximum_states)
    max colour_wrap (4, total_number) total_opened
