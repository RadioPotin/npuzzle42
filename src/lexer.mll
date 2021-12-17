{
open Parser
}

let linebreak = ['\n' '\r']
let whitespace = [' ' '\t']
let com_id = ['#'] [' ' '\t']? [' ' '\t' '0'-'9' '#' 'a'-'z' 'A'-'Z' '?']+
let int_id = ['-']? ['0'-'9']+


rule token = parse
  | whitespace { token lexbuf }
  | linebreak {token lexbuf}
  | com_id as id {COM id}
  | int_id as id {INT (int_of_string id)}
  | eof { EOF }
  | _ { raise (Ast.Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
