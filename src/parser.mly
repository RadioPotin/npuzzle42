
%token<int> INT
%token<string> COM
%token EOF

%start <Ast.npuzzle> npuzzle

%%

let comment :=
    | comment = COM ;
  { comment }

let size :=
    | n = INT ; option(comment);
  { n }

let tile :=
    | n = INT; option(comment);
  { n }

let tiles :=
    | {[]}
  | n = tile ; ~ = tiles ;
  { n::tiles }

let puzzle :=
  | ~ = tiles ; {[tiles]}

let npuzzle :=
    |  ~ = size; ~ = puzzle; EOF;
  { size, puzzle }
    | option(list(comment)); ~ = size; ~ = puzzle; option(list(comment)); EOF;
  { size, puzzle }
