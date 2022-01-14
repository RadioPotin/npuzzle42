let () = Random.self_init ()

let rec size () =
  match Random.int 4 with
  | 0
  | 1 ->
    size ()
  | n -> n

let gen () =
  let tbl = Hashtbl.create 124 in
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
  ( size
  , Immut_array.init size (fun _i -> Immut_array.init size (fun _i -> aux ()))
  )
