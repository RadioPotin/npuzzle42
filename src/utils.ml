let map_to_lists puzzle =
  Immut_array.map Immut_array.to_list puzzle |> Immut_array.to_list

let map_of_lists puzzle =
  Immut_array.of_list (List.map Immut_array.of_list puzzle)

module Saucisse : sig
  type 'a t

  val empty : 'a t

  val push : 'a -> int -> 'a t -> 'a t

  val take_max : 'a t -> ('a * int * 'a t) option

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
    puzzle *)
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
