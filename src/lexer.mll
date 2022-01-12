{
open Parser
}

let linebreak = ['\n' '\r']
let whitespace = [' ' '\t']
let com_id = ['#'] [' ' '\t']*? ([^ '\n' '\r']*)
let int_id = ['-']? ['0'-'9']+

rule token = parse
  | whitespace { token lexbuf }
  | com_id { token lexbuf }
  | linebreak { NEWLINE }
  | int_id as id {INT (int_of_string id)}
  | eof { EOF }
  | _ { raise (Ast.Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
