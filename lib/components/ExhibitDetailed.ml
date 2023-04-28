let unwrap x =
  match x with
  | Ok x -> x
  | _ -> failwith "nope"
;;

let format_exhibit
  header
  (user : Ohtml.Resource.User.t)
  (exhibit : Ohtml.Exhibit.exhibit)
  (cont : string list)
  =
  let e_header = header in
  let open Tyxml.Html in
  let comments = List.map ~f:(fun c -> tr [ td [ txt c ] ]) cont in
  let image_tag =
    match exhibit.image_id with
    | Some image_id -> img ~src:("/images/" ^ Int.to_string image_id) ~alt:"" ()
    | None -> div []
  in
  let e_body =
    body
      [ div
          ~a:[ a_id ("exhibit-" ^ Int.to_string exhibit.id) ]
          [ txt exhibit.content
          ; txt " ("
          ; txt user.name
          ; txt ")"
          ; image_tag
          ; table ~thead:(thead [ tr [ th [ txt "comments" ] ] ]) comments
          ]
      ]
  in
  html e_header e_body |> Fmt.str "%a" (Tyxml.Html.pp_elt ())
;;

let handle header (request : Dream.request) =
  let id = Dream.param request "id" in
  let%lwt exhibit = Dream.sql request (Ohtml.Exhibit.get (Int.of_string id)) in
  match exhibit with
  | Ok exhibit ->
    let exhibit = Option.value_exn exhibit in
    let%lwt user =
      Dream.sql request (Ohtml.Resource.User.read exhibit.user_id)
    in
    let user =
      match user with
      | Ok user -> user
      | Error e ->
        Fmt.pr "@.%a@." Caqti_error.pp e;
        failwith "OH NO"
    in
    let user = user |> Option.value_exn in
    let%lwt comments =
      Dream.sql request (Ohtml.Exhibit.get_comments exhibit.id)
    in
    (match comments with
     | Ok comments -> Dream.html @@ format_exhibit header user exhibit comments
     | Error (Database_error s) ->
       Fmt.pr "failed... :'( %s@." s;
       Dream.empty `Not_Found)
  | _ -> Dream.empty `Not_Found
;;
