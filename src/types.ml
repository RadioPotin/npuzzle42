exception Syntax_error of string

exception Format_error of string

type t = int Immut_array.t Immut_array.t

type position = int * int

type tile =
  {
    value : int;
    pos : position
  }

type state =
  {
    puzzle : t;
    tiles : tile list;
    size : int;
    g_cost : int;
    h_cost : int
  }

type npuzzle = int * t
