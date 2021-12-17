exception Syntax_error of string

exception Format_error of string

type size = int

type tile = int

type tiles = tile list

type puzzle = tiles list

type npuzzle = size * puzzle
