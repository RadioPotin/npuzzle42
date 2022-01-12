
%token<int> INT
%token EOF
%token NEWLINE

%start <Ast.npuzzle> npuzzle


%left NEWLINE

%%

let npuzzle :=
  | _ = list(NEWLINE); size = INT; nonempty_list(NEWLINE); l = separated_nonempty_list(nonempty_list(NEWLINE), list(INT)); EOF; {(size, l)}
