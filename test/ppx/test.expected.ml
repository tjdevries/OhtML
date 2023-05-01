module User =
  struct
    type t = {
      id: int ;
      name: string }[@@deriving model]
    include struct let _ = fun (_ : t) -> ()
                   let read _ = 10
                   let _ = read end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
