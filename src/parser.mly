
%token<int> INT
%token<string> COM
%token EOF


%start <Ast.npuzzle> npuzzle

%%
let comment :=
    | comment = COM ;
  { comment }

let size :=
    | n = INT ; {(n, None)}
    | n = INT ; com = comment ; {(n, Some com)}

let tile :=
    | value = INT ; {value}

let tiles :=
    | values = list(tile); {values}

let rows :=
    | values = tiles; {values}

let puzzle :=
    |r = rows; com = comment; {r, Some com}
    | r = rows; {r, None}

let npuzzle :=
    | header = option(list(comment)); n = size; p = puzzle; footer = option(list(comment)); EOF;
  { header, n, [p], footer }
