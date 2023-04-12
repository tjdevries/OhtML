open Base
(* https://github.com/roddyyaga/ppx_rapper *)

module T = Caqti_type

type exhibit =
  { id : int
  ; content : string
  }

type error = Database_error of string

(* Helper method to map Caqti errors to our own error type. 
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return
;;

let migrate =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->. T.unit)
      {|
      CREATE TABLE exhibits (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          content VARCHAR
        )
      |}
  in
  fun (module Db : Db.DB) -> Db.exec query () |> or_error
;;

let rollback =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->. T.unit) "DROP TABLE exhibits"
  in
  fun (module Db : Db.DB) -> Db.exec query () |> or_error
;;

(* Stub these out for now. *)
let add =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit) "INSERT INTO exhibits (content) VALUES ($1)"
  in
  fun content (module Db : Db.DB) -> Db.exec query content |> or_error
;;

let remove =
  let query =
    let open Caqti_request.Infix in
    (T.int ->. T.unit) "DELETE FROM exhibits WHERE id = $1"
  in
  fun id (module Db : Db.DB) -> Db.exec query id |> or_error
;;

let get =
  let query =
    let open Caqti_request.Infix in
    (T.int ->* T.(tup2 int string))
      "SELECT id, content FROM exhibits WHERE id = $1"
  in
  fun id (module Db : Db.DB) ->
    let%lwt x = Db.collect_list query id |> or_error in
    match x with
    | Ok [ (id, content) ] -> Ok { id; content } |> Lwt.return
    | _ -> Error (Database_error "Could not find exhibit") |> Lwt.return
;;

let get_all =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup2 int string)) "SELECT id, content FROM exhibits "
  in
  fun (module Db : Db.DB) ->
    let%lwt x = Db.collect_list query () |> or_error in
    match x with
    | Ok rows ->
      Ok (List.map rows ~f:(fun (id, content) -> { id; content })) |> Lwt.return
    | _ -> Error (Database_error "Could not find exhibit") |> Lwt.return
;;

let get_link id = Caml.Format.sprintf "/exhibit/%d" id
let delete_link ex = Caml.Format.sprintf "/exhibit/%d" ex.id
