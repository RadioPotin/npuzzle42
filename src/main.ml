open Npuzzle

let solve size map heuristic =
  if Utils.is_solvable (size, map) then
    Astar.solve_with heuristic size map
  else
    Pp.not_solvable ()

(** [main input] entry point to the program, if input is [""] then a valid map
    is automatically generated. Said map cannot exceed the size hardcoded in
    [Generate] module nor can it be equal to 0 or 1 *)
let main input heuristic =
  let fmt = Format.std_formatter in
  match input with
  | None ->
    let size, puzzle = Generate.gen () in
    let s = Format.sprintf "GENERATING SOLVABLE MAP OF SIZE %d@." size in
    Pp.colour_wrap fmt (4, s);
    solve size puzzle heuristic
  | Some input -> (
    let puzzle_file = String.concat "\n" (Utils.read_puzzle_file input) in

    try
      Pp.file fmt puzzle_file input;
      let size, map = Utils.map_maker puzzle_file in
      solve size map heuristic
    with
    | Types.Syntax_error s ->
      Format.eprintf "SYNTAX ERROR: %s@." s;
      exit 1
    | Types.Format_error s ->
      Format.eprintf "FORMAT ERROR: %s@." s;
      exit 1 )

let () =
  let heuristic = ref "Manhattan" in
  let input = ref None in
  let help = Utils.Cli.help in
  let usage = Utils.Cli.usage in
  let check_file s =
    if Sys.file_exists s then
      input := Some s
    else begin
      Pp.with_colour ();
      Pp.colour_wrap Format.std_formatter (2, "No such file or directory.")
    end
  in
  let finish_with f =
    f ();
    exit 1
  in

  let cli =
    [ ("--help", Arg.Unit (fun () -> finish_with help), "")
    ; ("-help", Arg.Unit (fun () -> finish_with help), "")
    ; ("-h", Arg.Unit (fun () -> finish_with help), "")
    ; ("--usage", Arg.Unit (fun () -> finish_with usage), "")
    ; ("-u", Arg.Unit (fun () -> finish_with usage), "")
    ; ("--colour", Arg.Unit (fun () -> Pp.with_colour ()), "")
    ; ("-c", Arg.Unit (fun () -> Pp.with_colour ()), "")
    ; ("--heuristic", Arg.String (fun s -> heuristic := s), "")
    ; ("-hfunc", Arg.String (fun s -> heuristic := s), "")
    ; ("--file", Arg.String check_file, "")
    ; ("-f", Arg.String check_file, "")
    ]
  in
  Arg.parse cli
    (fun s ->
      help ();
      failwith @@ Format.sprintf "unknown arg `%s`" s )
    "";

  let input = !input in
  let heuristic = !heuristic in
  main input heuristic
