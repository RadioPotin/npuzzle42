open Npuzzle
open Cmdliner

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

let maker s : int * Ast.t=
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
  List.iter (fun line -> Format.printf "line = `%s`@." line) lines;
  let lines =
    List.map
      (fun line ->
        let words = String.split_on_char ' ' (String.trim line) in
        List.iter (fun w -> Format.printf "word = `%s`@." w) words;
        List.map int_of_string words )
      lines
  in
  let size, lines =
    match lines with
    | [] -> failwith "empty"
    | size :: lines -> (List.hd size, lines)
  in
  assert(List.length lines = size);
  assert(List.for_all (fun line -> List.length line = size) lines);
  (size, Immut_array.of_list (List.map Immut_array.of_list lines))

let main = function
  | "" -> Format.printf "GENERATING NEW MAP@.";
  let map = Generate.gen () in Pp.map Format.std_formatter map
  | input ->

  let fmt = Format.std_formatter in
  let puzzle_file = String.concat "\n" (read_puzzle_file input) in
  try
    Pp.file fmt puzzle_file;
    let map =
      maker puzzle_file
    in Pp.map fmt map

  with
  | Ast.Syntax_error s ->
    Format.eprintf "ERROR: %s@." s;
    exit 1
  | Ast.Format_error s ->
    Format.eprintf "ERROR: %s@." s;
    exit 1

let npuzzle = Term.(const main $ input)

let () = Term.exit @@ Term.eval (npuzzle, usage)
