let _ = Declare.Sql.list_comments
let hx_post link = Tyxml.Html.Unsafe.string_attrib "hx-post" link
let hx_get link = Tyxml.Html.Unsafe.string_attrib "hx-get" link
let hx_swap str = Tyxml.Html.Unsafe.string_attrib "hx-swap" str
let hx_target str = Tyxml.Html.Unsafe.string_attrib "hx-target" str

let make_swapper route content t =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "sample-transition" ] ]
    [ h1 [ txt content ]
    ; button
        ~a:
          [ hx_get route
          ; hx_swap "outerHTML transition:true"
          ; hx_target "closest div"
          ]
        [ txt t ]
    ]
;;

let greet _ who =
  let open Tyxml.Html in
  let _ = [ "Good morning"; who; "!" ] |> List.map txt in
  html
    (head
       (title (txt "Greeting"))
       [ link ~rel:[ `Stylesheet ] ~href:"/static/home.css" ()
       ; script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.0" ] (txt "")
       ])
    (body
       [ h1 [ txt "Good morning, "; txt who; txt "!" ]
       ; div ~a:[ a_id "counter" ] [ txt "0" ]
       ; h2 [ txt "not affiliated with rust foundation, btw" ]
       ; button ~a:[ hx_post "/increment" ] [ txt "Increment" ]
       ; form (* Tyxml.Unsafe.data *)
           ~a:[ a_action "/exhibit/"; a_method `Post ]
           [ input ~a:[ a_name "content" ] () ]
       ; make_swapper "/transition" "swapped content" "swap it!"
       ])
;;

let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let counter = ref 0

let format_exhibit (exhibit : Declare.Exhibit.exhibit) =
  let open Tyxml.Html in
  div
    ~a:[ a_id ("exhibit-" ^ string_of_int exhibit.id) ]
    [ txt exhibit.content ]
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
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
       ; Dream.get "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Declare.Exhibit.get (int_of_string id))
           in
           match exhibit with
           | Ok exhibit -> Dream.html @@ format_exhibit exhibit
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/" (fun _ -> Dream.html "POSTED YO")
       ; Dream.get "/exhibits" (fun _ -> Dream.html "Getting exhibits")
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
             let%lwt _ = Dream.sql request (Declare.Exhibit.add content) in
             Dream.html @@ html_to_string @@ greet request " POSTED "
             (* Dream.redirect request "/" *)
           | `Ok something ->
             List.iter (fun (k, v) -> Format.printf "  %s: %s@." k v) something;
             Dream.html @@ html_to_string @@ greet request " POSTED "
           | _ ->
             Format.printf "  bad request: s@.";
             Dream.empty `Bad_Request)
       ]
;;
