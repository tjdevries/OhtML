open Base
module T = Caqti_type

type exhibit =
  { id : int
  ; user_id : int
  ; image_id : int option
  ; content : string
  }

(* Helper method to map Caqti errors to our own error type. 
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error = Database.or_error

(* Stub these out for now. *)
let add =
  let query =
    [%rapper
      get_one
        {| INSERT INTO exhibits (user_id, image_id, content)
           VALUES (%int{user_id}, %int?{image_id}, %string{content})
           RETURNING @int{id} |}]
  in
  fun ~content ~user_id ~image_id db ->
    query ~content ~user_id ~image_id db |> or_error
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
        {| SELECT @int{id}, @int{user_id}, @int?{image_id}, @string{content}
           FROM exhibits WHERE id = %int{id} |}
        record_out]
  in
  fun id db -> query ~id db |> or_error
;;

let get_all =
  let query =
    [%rapper
      get_many
        {| SELECT @int{id}, @int{user_id}, @int?{image_id}, @string{content}
           FROM exhibits |}
        record_out]
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
    [%rapper
      get_many
        {| SELECT @string{comments.content} from comments
            INNER JOIN exhibits on exhibits.id = comments.exhibit_id
            WHERE exhibits.id = %int{id} |}]
  in
  fun id db ->
    let%lwt x = query ~id db |> or_error in
    x |> Lwt.return
;;
