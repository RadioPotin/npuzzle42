open Npuzzle
open Cmdliner

let input =
  let doc = "File with NPuzzle definition." in
  Arg.(value & pos 0 string "" & info [] ~docv:"NPuzzle file" ~doc)

let usage =
  let doc =
    "This small program is a solver for NPuzzles. It implements the A* algorithm and three different heuristics to go along with it."
  in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <laradiopotin@gmail.com>." ]
  in
  Term.info "NPuzzle" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let read_puzzle_file filename =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines

let main input =
  let puzzle_file = String.concat "\n" (read_puzzle_file input) in
  Format.printf "FILE:----@.%s@.----------@." puzzle_file;
  let puzzle_buf = Lexing.from_string puzzle_file in
  try
    let header, size, puzzle, footer = Parser.npuzzle Lexer.token puzzle_buf in
    Format.fprintf Format.std_formatter
      "%a@.%a@.%a@.%a@."
      (fun fmt -> function
          | None -> ()
          | Some comments ->
            (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
          (fun fmt comment -> Format.fprintf fmt "%s" comment
          )) fmt comments
      ) header
      (fun fmt  -> function
          | size, Some comment -> Format.fprintf fmt "%d%s" size comment
          | size, None -> Format.fprintf fmt "%d" size
      )  size
      (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
          (fun fmt -> function
            | row, None ->
              (Format.fprintf fmt "%a" (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt "_")
                (fun fmt value -> Format.fprintf fmt "%d" value)
                ) row )
            | row, Some comment->
              Format.fprintf fmt "%a%s"
                (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt "-")
                (fun fmt value -> Format.fprintf fmt "%d" value)
                ) row comment )
      ) puzzle
      (fun fmt -> function
          | None -> ()
          | Some comments ->
            (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.")
          (fun fmt comment -> Format.fprintf fmt "%s" comment
          )) fmt comments
      ) footer

  with
  | Ast.Syntax_error s -> Format.eprintf "ERROR: %s@." s; exit 1
  | Ast.Format_error s -> Format.eprintf "ERROR: %s@." s; exit 1
  | Parser.Error -> Format.eprintf "PARSER ERROR"; exit 1

let npuzzle = Term.(const main $ input)

let () = Term.exit @@ Term.eval (npuzzle, usage)
