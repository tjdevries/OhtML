open Base

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

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
    [ {| INSERT INTO users (name, password) VALUES
          ('teej_dv', '12345'),
          ('opti', 'hunter2'),
          ('nightshadedude', 'ilovepiq') |}
    ; {| INSERT INTO exhibits (user_id, content) VALUES
          (1, 'This is an example exhibit'),
          (1, 'A middle exhibit'),
          (2, 'This is another one') |}
    ; {| INSERT INTO comments (content, exhibit_id) VALUES
          ('Wow, first try?', 1),
          ('Opti is a great teacher', 1) |}
    ]
  in
  exec_strs (module Db) queries
;;
