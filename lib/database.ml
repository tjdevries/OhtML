module type DB = Caqti_lwt.CONNECTION

type t = (module DB)
type error = Database_error of string

let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return
;;
