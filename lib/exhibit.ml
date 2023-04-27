open Base
module T = Caqti_type

type exhibit =
  { id : int
  ; content : string
  }

(* Helper method to map Caqti errors to our own error type. 
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error = Database.or_error

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
  fun (module Db : Database.DB) -> Db.exec query () |> or_error
;;

let rollback =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->. T.unit) "DROP TABLE exhibits"
  in
  fun (module Db : Database.DB) -> Db.exec query () |> or_error
;;

(* Stub these out for now. *)
let add =
  let query =
    [%rapper
      get_one
        {| INSERT INTO exhibits (content) VALUES (%string{content}) RETURNING @int{id} |}]
  in
  fun content db -> query ~content db |> or_error
;;

let remove =
  let query =
    [%rapper execute {| DELETE FROM exhibits WHERE id = %int{id} |}]
  in
  fun id db -> query ~id db |> or_error
;;

let get =
  let query =
    [%rapper
      get_opt
        {| SELECT @int{id}, @string{content} FROM exhibits WHERE id = %int{id} |}
        record_out]
  in
  fun id db -> query ~id db |> or_error
;;

let get_all =
  let query =
    [%rapper
      get_many {| SELECT @int{id}, @string{content} FROM exhibits |} record_out]
  in
  fun db -> query () db |> or_error
;;

let route = "/exhibit"

let route_for_id ?mode id =
  match mode with
  | Some `List -> "/exhibit/list/" ^ Int.to_string id
  | _ -> "/exhibit/" ^ Int.to_string id
;;

let route_for ex = route_for_id ex.id

let get_comments =
  let query =
    let open Caqti_request.Infix in
    (T.int ->* T.(string))
      {| SELECT comments.content from comments
            INNER JOIN exhibits on exhibits.id = comments.exhibit_id
            WHERE exhibits.id = $1 |}
  in
  fun id (module Db : Database.DB) ->
    let%lwt x = Db.collect_list query id |> or_error in
    x |> Lwt.return
;;
