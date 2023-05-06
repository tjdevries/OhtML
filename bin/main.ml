open Base
module Exhibit = Models.Exhibit

let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt

let unwrap x =
  match x with
  | Ok x -> x
  | _ -> failwith "unwrap failed!"
;;

let make_image link_ =
  let open Tyxml.Html in
  img ~src:link_ ~alt:"" ()
;;

let make_swapper route content t =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "sample-transition" ] ]
    [ h1 [ txt "hey begin: "; txt content ]
    ; button
        ~a:[ Hx.get route; Hx.swap ~transition:true OuterHTML; Hx.target (Closest "div") ]
        [ txt t ]
    ]
;;

let default_header title_text =
  let open Tyxml.Html in
  head
    (title (txt title_text))
    [ link ~rel:[ `Stylesheet ] ~href:"/static/home.css" ()
      (* ; script ~a:[ a_mime_type "module"; a_src "https://cdn.skypack.dev/twind/shim" ] (txt "") *)
    ; script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.0" ] (txt "")
    ]
;;

let index _ who =
  let open Tyxml.Html in
  html
    (default_header "OhtML")
    (body
       [ h1 [ txt "Good morning, "; txt who; txt "!" ]
       ; div ~a:[ a_id "counter"; a_class [ "bg-gray-200" ] ] [ txt "0" ]
       ; h2 [ txt "not affiliated with rust foundation, btw" ]
       ; button ~a:[ Hx.post "/increment" ] [ txt "Increment" ]
       ; make_swapper "/transition" "swapped content" "swap it!"
       ; a ~a:[ a_href "/exhibits" ] [ txt "exhibits" ]
       ; img ~src:"" ~alt:"" ()
       ; div
         @@ Components.ImageUpload.make_form
              ~id:"image"
              ~route:"/upload"
              ~target:(Previous "img")
       ])
;;

let login _ =
  let open Tyxml.Html in
  let make_input ~name ~text ~input_type =
    div
      [ label ~a:[ a_label_for name ] [ txt text ]
      ; input ~a:[ a_id name; a_name name; a_input_type input_type ] ()
      ]
  in
  html
    (default_header "Login")
    (body
       [ h1 [ txt "Login here" ]
       ; form
           ~a:[ a_action "/login"; a_method `Post ]
           [ div
               ~a:[ a_id "twitchchat" ]
               [ make_input ~name:"user_id" ~text:"user_id:" ~input_type:`Text
               ; make_input ~name:"name" ~text:"Username:" ~input_type:`Text
               ; make_input ~name:"password" ~text:"Password:" ~input_type:`Password
               ; button ~a:[ a_button_type `Submit ] [ txt "Submit" ]
               ]
           ]
       ])
;;

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html

let format_exhibits user_id request =
  let%lwt user =
    match user_id with
    | Some user_id -> Dream.sql request (Models.User.read ~id:user_id)
    | None -> None |> Lwt_result.return
  in
  match user with
  | Ok user ->
    (match%lwt Dream.sql request (Exhibit.read_all ()) with
     | Ok exhibits ->
       Components.ExhibitList.page user (default_header "Exhibits") exhibits
     | _ -> failwith "No Exhibists Something")
  | Error _ -> failwith "Error Something"
;;

let counter = ref 0

let get_user request =
  let user_id = Dream.session_field request "user" in
  Fmt.pr "get user: %s@." (Option.value ~default:"None" user_id);
  match user_id with
  | None ->
    let%lwt () = Dream.invalidate_session request in
    None |> Lwt.return
  | Some user_id -> Some (Int.of_string user_id) |> Lwt.return
;;

let set_user request user_id =
  let%lwt () = Dream.invalidate_session request in
  let%lwt () = Dream.set_session_field request "user" (Int.to_string user_id) in
  Fmt.pr "set user: %d@." user_id |> Lwt.return
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.memory_sessions
  @@ Dream.sql_pool "sqlite3:/home/tjdevries/tmp/ohtml.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           Dream.html
           @@ html_to_string
           @@ index request (Fmt.str "Twitch Chat: %s" "teej"))
       ; Dream.get "/static/**" (Dream.static "./static")
       ; Dream.get "/login" (fun request -> Dream.html @@ html_to_string (login request))
       ; Dream.post "/login" (fun request ->
           let find_data t key =
             List.find_map t ~f:(fun (header, data) ->
               if String.(header = key) then Some data else None)
           in
           let%lwt form_result = Dream.form ~csrf:false request in
           match form_result with
           | `Ok form_data ->
             let user_id = find_data form_data "user_id" in
             (match user_id with
              | Some user_id ->
                let%lwt () = set_user request (Int.of_string user_id) in
                Dream.redirect request "/"
              | None -> failwith "dawg")
           | _ -> failwith "Login Something")
       ; Dream.post "/increment" (fun _ ->
           Int.incr counter;
           Dream.html ("yo, posted:" ^ Int.to_string !counter))
       ; Dream.get "/count" (fun _ ->
           Dream.html ("yo, count is:" ^ Int.to_string !counter))
       ; Dream.get "/exhibits" (fun request ->
           let%lwt user_id = get_user request in
           format_exhibits user_id request)
       ; Dream.post "/exhibit/" (fun request ->
           (*  TODO: Come back to this and explore let%map and let%bind more *)
           (* let open Result.Let_syntax in *)
           (* let open Lwt_result.Let_syntax in *)
           let%lwt user_id = get_user request in
           match user_id with
           | Some user_id ->
             let%lwt form_result = Dream.multipart ~csrf:false request in
             (match form_result with
              | `Ok data ->
                let%lwt decoded = Components.ExhibitPost.upload_form request data in
                let content, image =
                  match decoded with
                  | Ok (content, image) -> content, image
                  | Error _ -> assert false
                in
                let image_id = Option.map image ~f:(fun x -> x.id) in
                (match%lwt
                   Dream.sql request (Models.Exhibit.create ~user_id ~content ~image_id)
                 with
                 | Ok exhibit ->
                   Dream.response
                     (elt_to_string @@ Components.ExhibitList.table_row None exhibit)
                   |> Lwt.return
                 | _ -> assert false)
              | _ -> Dream.empty `Bad_Request)
           | None -> Dream.empty `Unauthorized)
       ; Dream.get "/exhibit/list/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit = Dream.sql request (Exhibit.read ~id:(Int.of_string id)) in
           match exhibit with
           | Ok exhibit ->
             Dream.response
               (elt_to_string
                @@ Components.ExhibitList.table_row None (Option.value_exn exhibit))
             |> Lwt.return
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/:id"
         @@ Components.ExhibitDetailed.handle (default_header "exhibits")
       ; Dream.delete "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit = Dream.sql request (Exhibit.delete ~id:(Int.of_string id)) in
           match exhibit with
           | Ok _ -> Dream.html "deleted"
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/user/:id" Models.User.handle_get
       ; Dream.get "/replace" (fun _ ->
           Dream.html
             (make_swapper "/transition" "swapped content" "swap it!"
              |> Fmt.str "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.get "/transition" (fun _ ->
           Dream.html
             (make_swapper "/replace" "replace this" "replace it!"
              |> Fmt.str "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.post "/upload" (fun request ->
           let rec receive files =
             match%lwt Dream.upload request with
             | None -> files |> Lwt.return
             | Some (_, _, _) ->
               let rec concat_part contents =
                 match%lwt Dream.upload_part request with
                 | None -> receive (contents :: files)
                 | Some chunk -> concat_part (contents ^ chunk)
               in
               concat_part ""
           in
           let%lwt uploaded = receive [] in
           let%lwt image =
             Dream.sql request Models.Image.(create ~data:(List.hd_exn uploaded))
           in
           let image = image |> unwrap in
           Dream.html (elt_to_string (make_image ("/images/" ^ Int.to_string image.id))))
       ; Dream.get "/images/:id" (fun request ->
           let id = Dream.param request "id" in
           let id = Int.of_string id in
           let%lwt image = Dream.sql request (Models.Image.read ~id) in
           let image = image |> unwrap in
           let image = Option.value_exn image in
           Dream.response ~headers:[ "Content-Type", "image/jpeg" ] image.data
           |> Lwt.return)
       ; Dream_livereload.route ()
         (* ; Dream.get "/exhibit/" (fun _ -> Dream.html "POSTED YO") *)
       ]
;;
