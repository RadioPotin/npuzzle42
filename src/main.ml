open Npuzzle
open Cmdliner

let colour =
  let doc = "Print colourful output for readability" in
  Arg.(value & flag & info [ "colour"; "c" ] ~docv:"colour" ~doc)

let input =
  let doc = "File with NPuzzle definition." in
  Arg.(value & pos 0 string "" & info [] ~docv:"NPuzzle file" ~doc)

let usage =
  let doc =
    "This small program is a solver for NPuzzles. It implements the A* \
     algorithm and three different heuristics to go along with it."
  in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <laradiopotin@gmail.com>." ]
  in
  Term.info "NPuzzle" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

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

(** [verify_solvability fmt puzzle] place holder function used for assertion of
    solvability of the given puzzle *)
let verify_solvability _fmt puzzle =
  if Utils.is_solvable puzzle then
    ()
  else
    exit 1

let solve fmt size map =
  verify_solvability fmt (size, map);
  Format.fprintf fmt "Solving:@\n";
  Astar.solve_with Heuristics.informed_search size map

(** [main input] entry point to the program, if input is [""] then a valid map
    is automatically generated. Said map cannot exceed the size hardcoded in
    [Generate] module nor can it be equal to 0 or 1 *)
let main input colour =
  let fmt = Format.std_formatter in
  match input with
  | "saucisse" ->
    let p = [| [| 1; 0 |]; [| 2; 3 |] |] in
    Format.printf "%d@\n" (Heuristics.informed_search p);
    ()
  | "" ->
    Format.printf "GENERATING SOLVABLE MAP@.";
    let size, puzzle = Generate.gen () in
    if colour then Pp.with_colour ();
    solve fmt size puzzle
  | input -> (
    let puzzle_file = String.concat "\n" (read_puzzle_file input) in

    try
      Pp.file fmt puzzle_file;
      let size, map = Utils.map_maker puzzle_file in
      if colour then Pp.with_colour ();
      solve fmt size map
    with
    | Types.Syntax_error s ->
      Format.eprintf "ERROR: %s@." s;
      exit 1
    | Types.Format_error s ->
      Format.eprintf "ERROR: %s@." s;
      exit 1 )

let npuzzle = Term.(const main $ input $ colour)

let () = Term.exit @@ Term.eval (npuzzle, usage)
