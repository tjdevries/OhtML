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

let mk_header title_text =
  let open Tyxml.Html in
  head
    (title (txt title_text))
    [ link ~rel:[ `Stylesheet ] ~href:"/static/home.css" ()
      (* ; script ~a:[ a_mime_type "module"; a_src "https://cdn.skypack.dev/twind/shim" ] (txt "") *)
    ; script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.0" ] (txt "")
    ]
;;

let greet _ who =
  let open Tyxml.Html in
  let _ = [ "Good morning"; who; "!" ] |> List.map txt in
  html
    (mk_header "OhtML")
    (body
       [ h1 [ txt "Good morning, "; txt who; txt "!" ]
       ; div ~a:[ a_id "counter"; a_class [ "bg-gray-200" ] ] [ txt "0" ]
       ; h2 [ txt "not affiliated with rust foundation, btw" ]
       ; button ~a:[ Hx.post "/increment" ] [ txt "Increment" ]
       ; form
           ~a:[ a_action "/exhibit/"; a_method `Post ]
           [ input ~a:[ a_name "content" ] () ]
       ; make_swapper "/transition" "swapped content" "swap it!"
       ])
;;

let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html

let get_one_exhibit (ex : Ohtml.Exhibit.exhibit) =
  let open Tyxml.Html in
  (* ; txt ex.content *)
  tr
    [ td [ txt @@ string_of_int ex.id ]
    ; td [ txt ex.content ]
    ; td [ a ~a:[ a_href ("/exhibit/" ^ string_of_int ex.id) ] [ txt "get" ] ]
    ; td
        [ button
            ~a:
              [ Hx.delete (Ohtml.Exhibit.delete_link ex)
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
    let exhibits = List.map get_one_exhibit exhibits in
    let header = thead [ tr [ th [ txt "id" ]; th [ txt "content" ] ] ] in
    let exhibits = [ div [ table ~thead:header exhibits ] ] in
    html (mk_header "Exhibits") (body exhibits) |> html_to_string |> Dream.html
  | _ -> failwith "Something"
;;

let counter = ref 0

let format_exhibit (exhibit : Ohtml.Exhibit.exhibit) (cont : string list) =
  let open Tyxml.Html in
  let comments = List.map (fun c -> tr [ td [ txt c ] ]) cont in
  let e_header = mk_header "OhtML" in
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
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           Dream.html @@ html_to_string @@ greet request "Twitch Chat")
       ; Dream.get "/echo/:word" (fun request ->
           Dream.html (Dream.param request "word"))
       ; Dream.get "/static/**" (Dream.static "./static")
       ; Dream.post "/increment" (fun _ ->
           incr counter;
           Dream.html ("yo, posted:" ^ string_of_int !counter))
       ; Dream.get "/count" (fun _ ->
           Dream.html ("yo, count is:" ^ string_of_int !counter))
       ; Dream.delete "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Ohtml.Exhibit.remove (int_of_string id))
           in
           match exhibit with
           | Ok _ -> Dream.html "deleted"
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Ohtml.Exhibit.get (int_of_string id))
           in
           match exhibit with
           | Ok exhibit ->
             let%lwt comments =
               Dream.sql request (Ohtml.Exhibit.get_comments exhibit.id)
             in
             (match comments with
              | Ok comments -> Dream.html @@ format_exhibit exhibit comments
              | Error (Database_error s) ->
                Format.printf "failed... :'( %s@." s;
                Dream.empty `Not_Found)
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/" (fun _ -> Dream.html "POSTED YO")
       ; Dream.get "/exhibits" (fun request -> format_exhibits request)
       ; Dream.get "/replace" (fun _ ->
           Dream.html
             (make_swapper "/transition" "swapped content" "swap it!"
              |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.get "/transition" (fun _ ->
           Dream.html
             (make_swapper "/replace" "replace content" "replace it!"
              |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())))
       ; Dream.post "/exhibit/" (fun request ->
           Format.printf "@.INSIDE POST EXHIBIT:@.";
           match%lwt Dream.form ~csrf:false request with
           | `Ok [ ("content", content) ] ->
             Format.printf "  content: %s@." content;
             let%lwt _ = Dream.sql request (Ohtml.Exhibit.add content) in
             Dream.html @@ html_to_string @@ greet request " POSTED "
             (* Dream.redirect request "/" *)
           | `Ok something ->
             List.iter (fun (k, v) -> Format.printf "  %s: %s@." k v) something;
             Dream.html @@ html_to_string @@ greet request " POSTED "
           | _ ->
             Format.printf "  bad request: s@.";
             Dream.empty `Bad_Request)
       ; Dream.get "/user/:id" Ohtml.Resource.User.route_get
       ]
;;
