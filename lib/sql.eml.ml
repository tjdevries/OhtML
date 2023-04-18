open Base

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

let list_comments =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup2 int string)) "SELECT id, text FROM comment"
  in
  fun (module Db : DB) ->
    let%lwt comments_or_error = Db.collect_list query () in
    Caqti_lwt.or_fail comments_or_error
;;

let add_comment =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit) "INSERT INTO comment (text) VALUES ($1)"
  in
  fun text (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query text in
    (* Db.with_transaction (fun () -> Caqti_lwt.or_fail unit_or_error) *)
    Caqti_lwt.or_fail unit_or_error
;;

let exec_strs (module Db : DB) stmts =
  Lwt_list.iter_s
    (fun stmt ->
      let query =
        let open Caqti_request.Infix in
        (T.unit ->. T.unit) stmt
      in
      let%lwt unit_or_error = Db.exec query () in
      Caqti_lwt.or_fail unit_or_error)
    stmts
;;

let migrate (module Db : DB) =
  let unwrap x =
    match x with
    | Ok x -> x
    | _ -> failwith "nope"
  in
  Fpath.v "./tables/"
  |> Bos.OS.Dir.contents
  |> unwrap
  |> List.sort ~compare:(fun a b ->
       String.compare (Fpath.to_string a) (Fpath.to_string b))
  |> List.map ~f:(fun migration -> Bos.OS.File.read migration |> unwrap)
  |> exec_strs (module Db)
;;

let populate (module Db : DB) =
  let queries =
    [ {| INSERT INTO exhibits (content) VALUES
          ('This is an example exhibit'),
          ('A middle exhibit'),
          ('This is another one') |}
    ; {| INSERT INTO comments (content, exhibit_id) VALUES
          ('Wow, first try?', 1),
          ('Opti is a great teacher', 1) |}
    ]
  in
  exec_strs (module Db) queries
;;
