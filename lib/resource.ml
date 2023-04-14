open Lwt_result.Syntax

module type RESOURCE = sig
  type t
  type data

  val create_query : data -> Database.t -> (int, Caqti_error.t) Lwt_result.t

  val read_query
    :  id:int
    -> Database.t
    -> (t option, Caqti_error.t) Lwt_result.t

  val format : t -> [ `Div ] Tyxml_html.elt
end

module Resource (T : RESOURCE) = struct
  type t = T.t
  type data = T.data

  let read (id : int) (db : Database.t) = T.read_query ~id db

  let create (data : T.data) db =
    let* id = T.create_query data db in
    read id db |> Lwt_result.map Option.get
  ;;

  let format t = T.format t |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())

  let route_get request =
    let id = Dream.param request "id" in
    let%lwt resource = Dream.sql request (read (int_of_string id)) in
    match resource with
    | Ok (Some resource) -> Dream.html @@ format resource
    | _ -> Dream.empty `Not_Found
  ;;
end

module UserImpl = struct
  type t =
    { id : int
    ; name : string
    }

  type data = { name : string }

  let read_query =
    [%rapper
      get_opt
        {| SELECT @int{id}, @string{name}
             FROM resources
             WHERE id = %int{id} |}
        record_out]
  ;;

  let create_query =
    [%rapper
      get_one
        {|
          INSERT INTO users (name)
          VALUES (%string{name})
          RETURNING @int{id}
        |}
        record_in]
  ;;

  let format { id; name } =
    let open Tyxml_html in
    div [ h1 [ txt name ]; p [ txt (Printf.sprintf "ID: %d" id) ] ]
  ;;
end

module User = Resource (UserImpl)
