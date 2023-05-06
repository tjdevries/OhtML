(* open Models *)

let _content_id = "post-content"
let _image_id = "post-image"

let form ~target =
  let open Tyxml.Html in
  div
    ~a:[ a_id "exhibit-form" ]
    [ form
        ~a:
          [ Hx.post "/exhibit/"
          ; Hx.target target
          ; Hx.swap BeforeEnd (* ; Hx.boost true *)
          ; a_enctype "multipart/form-data"
          ]
        [ div [ input ~a:[ a_name _content_id ] () ]
        ; div
            [ label ~a:[ a_label_for _image_id ] [ txt "image goes here: " ]
            ; input ~a:[ a_input_type `File; a_name _image_id ] ()
            ]
        ; input ~a:[ a_input_type `Submit; a_value "Create Exhibit" ] ()
        ]
    ]
;;

module FormHelper = struct
  type 'a t = (string * 'a) list

  let find_data t key =
    List.find_map t ~f:(fun (header, data) ->
      if String.(header = key) then Some data else None)
  ;;
end

let upload_form request (form_data : Dream.multipart_form) =
  let content =
    match FormHelper.find_data form_data _content_id with
    | Some [ (_, content) ] -> Some content
    | _ -> None
  in
  let%lwt image_data =
    match FormHelper.find_data form_data _image_id with
    | Some image_data when List.length image_data = 1 ->
      (*  TODO: Not sure if we should discard the file name here... but i don't really care *)
      let _, image_data = List.hd_exn image_data in
      let%lwt image = Dream.sql request Models.Image.(create ~data:image_data) in
      (match image with
       | Ok image -> Some image |> Lwt.return
       | _ -> None |> Lwt.return)
    | _ -> None |> Lwt.return
  in
  match content, image_data with
  | Some content, image_data -> Ok (content, image_data) |> Lwt.return
  | _ -> Error `MissingContent |> Lwt.return
;;
