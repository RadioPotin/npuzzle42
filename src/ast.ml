exception Syntax_error of string
exception Format_error of string

type comment = string

type free_text = comment list option

type size = int * comment option

type tile = int

type tiles = tile list

type rows = tiles * comment option

type puzzle = rows list

type npuzzle = free_text * size * puzzle * free_text
