module Cli = struct
  (** [heurislist fmt l] Used to display the list of available heuristic
      functions. *)
  let heurislist fmt l =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      (fun fmt hfunc -> Format.fprintf fmt "%s" hfunc)
      fmt l

  (** [usage ()] Displays full usage of the program. *)
  let usage () =
    Format.fprintf Format.std_formatter
      {|NPUZZLE(1)                      NPuzzle Manual                      NPUZZLE(1)

NAME
       NPuzzle - This small program is a solver for NPuzzles. It implements
       the A* algorithm and three different heuristics to go along with it.

SYNOPSIS
       NPuzzle [OPTIONS]

OPTIONS
       --colour, -c
           Print colourful output for readability

       --heuristic, -hfunc
           One of the available heuristic functions used for A* algorithm

       --file, -f
           Specify a path to an NPuzzle map file. Look into the `puzzles` directory for information.

       --help, -h
           Some help on available options for the CLI of NPuzzle

       --usage, -u
           See this message

EXIT STATUS
       NPuzzle exits with the following status:

       0   on success.

       1   on failure.

BUGS
       Email bug reports to <laradiopotin@gmail.com>.
|}

  (** [help ()] Displays some help for the CLI of the program. *)
  let help () =
    Format.printf
      {|Some help for the CLI:
    - colour option is a flag, evaluates to true or false if found or not respectively.

    - heuristic function option evaluates to one of the following functions:
        { %a }.
      Defaults to Manhattan Distance.
      Parsing is generous with naming.
      These functions are used during the search to establish the most optimal node to expand in the search tree.

    - file option is a file that describes an NPUZZLE map, map can be unsolvable, and programm will say if so.
    For more information about formatting NPUZZLE maps, look into the `puzzles` directory.
|}
      heurislist
      [ "Manhattan Distance"; "Informed Search"; "Inversions Count" ]
end

(** Module Saucisse is the meat and bones of the datastructure used for the
    search *)
module Saucisse : sig
  (** ['a t] abstract type for map *)
  type 'a t

  (** [empty] returns an empty map. *)
  val empty : 'a t

  (** [push 'a key map] Pushes to the map a binding of ('a, key) and returns an
      updated map. *)
  val push : 'a -> int -> 'a t -> 'a t

  (** [take_max map] returns an [('a * key * updated_map) Option.t] value. *)
  val take_max : 'a t -> ('a * int * 'a t) option

  (** [pp f fmt map] printer for the datastructure. Takes a printer, a formatter
      and a map. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  module T = Map.Make (Int)

  type 'a t = 'a list T.t

  let empty = T.empty

  let push v k map =
    let new_list =
      match T.find_opt k map with
      | None -> [ v ]
      | Some l -> v :: l
    in
    T.add k new_list map

  let take_max map =
    match T.max_binding_opt map with
    | None -> None
    | Some (k, v) -> (
      match v with
      | [] -> assert false
      | x :: r ->
        let map =
          match r with
          | [] -> T.remove k map
          | _l -> T.add k r map
        in
        Some (x, k, map) )

  let pp f fmt v =
    let v = T.bindings v in
    Format.fprintf fmt "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (k, l) ->
           Format.fprintf fmt "{KEY:%d _ %a}" k
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt " - ")
                f )
             l ) )
      v
end

(** [no_duplicate_values lines] verifies that there are no duplicated tile in
    the puzzle *)
let no_duplicate_values lines =
  let tbl = Hashtbl.create 64 in
  List.for_all
    (fun line ->
      List.for_all
        (fun v ->
          if Hashtbl.mem tbl v then
            false
          else (
            Hashtbl.add tbl v ();
            true
          ) )
        line )
    lines

let has_blank_char lines = List.exists (List.exists (fun v -> v = 0)) lines

let map_to_lists puzzle =
  Immut_array.map Immut_array.to_list puzzle |> Immut_array.to_list

let map_of_lists puzzle =
  Immut_array.of_list (List.map Immut_array.of_list puzzle)

(** [map_maker s] reads the maps from [s], splits it and recovers the puzzle
    while making some sanity assertions on the nature of the puzzle *)
let map_maker s : Types.npuzzle =
  let lines = String.split_on_char '\n' s in
  let lines =
    List.fold_left
      (fun lines line ->
        let line = String.trim line in
        if not @@ String.contains line '#' then
          line :: lines
        else if String.get line 0 = '#' then
          lines
        else
          let new_line = List.hd @@ String.split_on_char '#' line in
          if String.trim new_line = "" then
            lines
          else
            new_line :: lines )
      [] lines
  in
  let lines = List.rev lines in
  let lines =
    List.map
      (fun line ->
        let words = String.split_on_char ' ' (String.trim line) in
        List.map int_of_string words )
      lines
  in
  let size, lines =
    match lines with
    | [] -> failwith "empty"
    | size :: lines -> (List.hd size, lines)
  in
  assert (List.length lines = size);
  assert (List.for_all (fun line -> List.length line = size) lines);
  assert (no_duplicate_values lines);
  assert (has_blank_char lines);
  (size, map_of_lists lines)

(** [get_inversions values] returns a tuple (inversions:int list *
    inversions_nb:int). this is used when figuring out the solvability of the
    puzzle. It operates on a flattened list of int values converted from
    [Types.t] *)
let get_inversions values =
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
  let inversions_count = List.fold_left ( + ) 0 in
  let inversions = map_inversions [] (List.flatten values) in
  let inversions_nb = inversions_count inversions in
  (inversions, inversions_nb)

(** [is_solvable (size, puzzle)] boolean function used to establish solvability
    of the map, be it read from a file, or the automatically generated one *)
let is_solvable (size, puzzle) =
  let is_even nb = nb mod 2 = 0 in

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
  let values = map_to_lists puzzle in
  let _inversions_l, inversions_nb = get_inversions values in

  if size mod 2 <> 0 then
    (* if size is odd and the number of inversions is even in the input state, then puzzle instance is solvable if*)
    inversions_nb mod 2 = 0
  else
    (* if size is even, puzzle instance is solvable if :
     * - the blank in on an EVEN row counting from the bottom AND number of inversions is ODD
     *
     * OR
     *
     * - the blank is on an ODD row counting from the bottom AND number of inversions is EVEN
     * source: https://www.geeksforgeeks.org/check-instance-15-puzzle-solvable/
     * *)
    blank_location_from_bottom is_on_even_row values
    && (not @@ is_even inversions_nb)
    || (not @@ blank_location_from_bottom is_on_even_row values)
       && is_even inversions_nb

(** [read_puzzle_file filename] opens a file given as argument to the program
    and returns it as a a list of strings *)
let read_puzzle_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with
  | End_of_file ->
    close_in chan;
    List.rev !lines
