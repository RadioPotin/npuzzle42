# Npuzzle [![Actions Status](https://github.com/RadioPotin/npuzzle42/workflows/build/badge.svg)](https://github.com/RadioPotin/npuzzle42/actions) [![coverage percentage](https://raw.githubusercontent.com/RadioPotin/npuzzle42/gh-pages/coverage/badge.svg)](https://RadioPotin.github.io/npuzzle42/coverage/)

[Subject PDF File](https://linx.zapashcanon.fr/8i0usstm.pdf)

# Building
```
dune build @all
```
# Testing
```
dune runtest
```
This will run all tests contained in the folder `test`.

(Which means NONE for now)

# Usage

```shell-session
NPUZZLE(1)                      NPuzzle Manual                      NPUZZLE(1)

NAME
       NPuzzle - This small program is a solver for NPuzzles. It implements
       the A* algorithm and three different heuristics to go along with it.

SYNOPSIS
       NPuzzle [OPTIONS]

OPTIONS
       --colour, -c
           Print colourful output for readability

       --heuristic, -hfunc
           One of the available heuristic functions used for A* algorithm

       --file, -f
           Specify a path to an NPuzzle map file. Look into the `puzzles`
           directory for information.

       --help, -h
           Some help on available options for the CLI of NPuzzle

       --usage, -u
           See this message

EXIT STATUS
       NPuzzle exits with the following status:

       0   on success.

       1   on failure.

BUGS
       Email bug reports to <laradiopotin@gmail.com>.
```

##  Running in Docker

The repo has a Dockerfile with everything necessary for you to just run.

**Building image**
```shell-session
docker build -t <user>:<imagename> .
```

Make sure to replace build `name` and `user` accordingly.

**Interactive mode**
```shell-session
docker run --rm -it --entrypoint bash <user>:<name>
```

**Running cli inside docker**

Either:

1. Wrap commands in `opam exec`
```shell-session
$ opam exec -- dune exec -- src/main.exe  
```

2. `eval $(opam env)` to have all required binaries in your path
```shell-session
$ eval $(opam env)
$ dune exec -- src/main.exe 
```

