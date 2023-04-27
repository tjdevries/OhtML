let elt_to_string elt = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

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
       ])
;;

let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html

module ExhibitHTML = struct
  open Tyxml.Html

  let make_form ~target =
    form
      ~a:
        [ Hx.post "/exhibit/"
        ; Hx.target target
        ; Hx.swap BeforeEnd (* ; Hx.boost true *)
        ]
      [ input ~a:[ a_name "content" ] (); button [ txt "submit" ] ]
  ;;
end

let get_one_exhibit (ex : Ohtml.Exhibit.exhibit) =
  let open Tyxml.Html in
  (* ; txt ex.content *)
  tr
    [ td [ txt @@ string_of_int ex.id ]
    ; td [ txt ex.content ]
    ; td [ a ~a:[ a_href (Ohtml.Exhibit.route_for ex) ] [ txt "get" ] ]
    ; td
        [ button
            ~a:
              [ Hx.delete (Ohtml.Exhibit.route_for ex)
              ; Hx.swap OuterHTML
              ; Hx.target (Closest "button")
              ]
            [ txt "delete" ]
        ]
    ]
;;

let format_exhibits request =
  match%lwt Dream.sql request Ohtml.Exhibit.get_all with
  | Ok exhibits ->
    let open Tyxml.Html in
    let header = Components.ExhibitList.header in
    let exhibits = List.map Components.ExhibitList.row exhibits in
    let exhibits =
      div [ table ~thead:header exhibits ]
      :: [ ExhibitHTML.make_form ~target:(Previous "tbody") ]
    in
    html (default_header "Exhibits") (body exhibits)
    |> html_to_string
    |> Dream.html
  | _ -> failwith "Something"
;;

let counter = ref 0

let format_exhibit (exhibit : Ohtml.Exhibit.exhibit) (cont : string list) =
  let open Tyxml.Html in
  let comments = List.map (fun c -> tr [ td [ txt c ] ]) cont in
  let e_header = default_header "OhtML" in
  let e_body =
    body
      [ div
          ~a:[ a_id ("exhibit-" ^ string_of_int exhibit.id) ]
          [ txt exhibit.content
          ; table ~thead:(thead [ tr [ th [ txt "comments" ] ] ]) comments
          ]
      ]
  in
  html e_header e_body |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
;;

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
       ; Dream.post "/increment" (fun _ ->
           incr counter;
           Dream.html ("yo, posted:" ^ string_of_int !counter))
       ; Dream.get "/count" (fun _ ->
           Dream.html ("yo, count is:" ^ string_of_int !counter))
       ; Dream.get "/exhibits" (fun request -> format_exhibits request)
       ; Dream.post "/exhibit/" (fun request ->
           match%lwt Dream.form ~csrf:false request with
           | `Ok [ ("content", content) ] ->
             let%lwt id = Dream.sql request (Ohtml.Exhibit.add content) in
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
             Fmt.pr "%s@."
             @@ elt_to_string
             @@ get_one_exhibit (Option.get exhibit);
             Dream.response
               (elt_to_string @@ get_one_exhibit (Option.get exhibit))
             |> Lwt.return
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Ohtml.Exhibit.get (int_of_string id))
           in
           match exhibit with
           | Ok exhibit ->
             let exhibit = Option.get exhibit in
             let%lwt comments =
               Dream.sql request (Ohtml.Exhibit.get_comments exhibit.id)
             in
             (match comments with
              | Ok comments -> Dream.html @@ format_exhibit exhibit comments
              | Error (Database_error s) ->
                Format.printf "failed... :'( %s@." s;
                Dream.empty `Not_Found)
           | _ -> Dream.empty `Not_Found)
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
       ; Dream_livereload.route ()
         (* ; Dream.get "/exhibit/" (fun _ -> Dream.html "POSTED YO") *)
       ]
;;
