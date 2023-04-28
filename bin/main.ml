let elt_to_string elt = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

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
        ~a:
          [ Hx.get route
          ; Hx.swap ~transition:true OuterHTML
          ; Hx.target (Closest "div")
          ]
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
  let _ = [ "Good morning"; who; "!" ] |> List.map txt in
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
           [ div
               ~a:[ a_id "twitchchat" ]
               [ make_input ~name:"name" ~text:"Username:" ~input_type:`Text
               ; make_input
                   ~name:"password"
                   ~text:"Password:"
                   ~input_type:`Password
               ]
           ]
       ])
;;

let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html

let format_exhibits request =
  match%lwt Dream.sql request Ohtml.Exhibit.get_all with
  | Ok exhibits ->
    let open Tyxml.Html in
    let header = Components.ExhibitList.table_head in
    let exhibits = List.map Components.ExhibitList.table_row exhibits in
    let exhibits =
      div [ table ~thead:header exhibits ]
      :: [ Components.ExhibitPost.form ~target:(Previous "tbody") ]
    in
    html (default_header "Exhibits") (body exhibits)
    |> html_to_string
    |> Dream.html
  | _ -> failwith "Something"
;;

let counter = ref 0

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool "sqlite3:/home/tjdevries/tmp/ohtml.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           let%lwt username =
             match Dream.session_field request "user" with
             | None ->
               let%lwt () = Dream.invalidate_session request in
               let%lwt () = Dream.set_session_field request "user" "teej_dv" in
               "alice" |> Lwt.return
             | Some username -> username |> Lwt.return
           in
           Dream.html
           @@ html_to_string
           @@ index request (Fmt.str "Twitch Chat: %s" username))
       ; Dream.get "/static/**" (Dream.static "./static")
       ; Dream.get "/login" (fun request ->
           Dream.html @@ html_to_string (login request))
       ; Dream.post "/increment" (fun _ ->
           incr counter;
           Dream.html ("yo, posted:" ^ string_of_int !counter))
       ; Dream.get "/count" (fun _ ->
           Dream.html ("yo, count is:" ^ string_of_int !counter))
       ; Dream.get "/exhibits" (fun request -> format_exhibits request)
       ; Dream.post "/exhibit/" (fun request ->
           match%lwt Dream.form ~csrf:false request with
           | `Ok [ ("content", content) ] ->
             let%lwt id =
               Dream.sql
                 request
                 (Ohtml.Exhibit.add ~content ~user_id:1 ~image_id:None)
             in
             let id = Result.get_ok id in
             Dream.redirect request (Ohtml.Exhibit.route_for_id ~mode:`List id)
           | _ -> Dream.empty `Bad_Request)
       ; Dream.get "/exhibit/list/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Ohtml.Exhibit.get (int_of_string id))
           in
           match exhibit with
           | Ok exhibit ->
             Dream.response
               (elt_to_string
                @@ Components.ExhibitList.table_row (Option.get exhibit))
             |> Lwt.return
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/:id"
         @@ Components.ExhibitDetailed.handle (default_header "exhibits")
       ; Dream.delete "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Ohtml.Exhibit.remove (int_of_string id))
           in
           match exhibit with
           | Ok _ -> Dream.html "deleted"
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/user/:id" Ohtml.Resource.User.route_get
       ; Dream.get "/replace" (fun _ ->
           Dream.html
             (make_swapper "/transition" "swapped content" "swap it!"
              |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.get "/transition" (fun _ ->
           Dream.html
             (make_swapper "/replace" "replace this" "replace it!"
              |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.post "/upload" (fun request ->
           Fmt.pr "uploading\n%!";
           let rec receive files =
             match%lwt Dream.upload request with
             | None -> files |> Lwt.return
             | Some (name, filename, _) ->
               Fmt.pr
                 "name(%s) filename(%s)\n"
                 (Option.value ~default:"<default>" name)
                 (Option.value ~default:"<default>" filename);
               let rec concat_part contents =
                 match%lwt Dream.upload_part request with
                 | None -> receive (contents :: files)
                 | Some chunk -> concat_part (contents ^ chunk)
               in
               concat_part ""
           in
           let%lwt uploaded = receive [] in
           Fmt.pr "uploaded len: %d\n" (List.length uploaded);
           Fmt.pr "first len: %d\n" (String.length (List.hd uploaded));
           let%lwt image =
             Dream.sql
               request
               Ohtml.Resource.Image.(create { data = List.hd uploaded })
           in
           let image = image |> unwrap in
           Dream.html
             (elt_to_string (make_image ("/images/" ^ Int.to_string image.id))))
       ; Dream.get "/images/:id" (fun request ->
           let id = Dream.param request "id" in
           let id = int_of_string id in
           let%lwt image = Dream.sql request (Ohtml.Resource.Image.read id) in
           let image = image |> unwrap in
           let image = Option.get image in
           Dream.response ~headers:[ "Content-Type", "image/jpeg" ] image.data
           |> Lwt.return)
       ; Dream_livereload.route ()
         (* ; Dream.get "/exhibit/" (fun _ -> Dream.html "POSTED YO") *)
       ]
;;
