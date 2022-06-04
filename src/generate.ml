let () = Random.self_init ()

let rec size () =
  match Random.int 6 with
  | 0
  | 1 ->
    size ()
  | n -> n

let rec gen () =
  let tbl = Hashtbl.create 64 in
  let size = size () in
  let rec aux () =
    let i = Random.int (size * size) in
    if Hashtbl.mem tbl i then
      aux ()
    else begin
      Hashtbl.add tbl i ();
      i
    end
  in
  let map =
    ( size
    , Immut_array.init size (fun _i -> Immut_array.init size (fun _i -> aux ()))
    )
  in
  if Utils.is_solvable map then
    map
  else
    gen ()

let goal size =
  Immut_array.init size
      (fun j ->
        Immut_array.init size
          (fun i -> i + j * size ))
