let _ = Declare.Sql.list_comments

(*
The hx-target attribute allows you to target a different element for swapping than the one issuing the AJAX request. The value of this attribute can be:

a CSS query selector of the element to target
this which indicates that the element that the hx-target attribute is on is the target next <CSS selector> which will scan the DOM forward for the first element that matches the given CSS selector. (e.g. next .error will target the closest following sibling element with error class)
previous <CSS selector> which will scan the DOM backwards for the first element that matches the given CSS selector. (e.g previous .error will target the closest previous sibling with error class)
closest <CSS selector> which will find the closest parent ancestor that matches the given CSS selector. (e.g. closest table will target the closest parent table to the element)
find <CSS selector> which will find the first child descendant element that matches the given CSS selector. (e.g find tr will target the first child descendant row to the element)
*)

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

type my_int = private int

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
       ; form (* Tyxml.Unsafe.data *)
           ~a:[ a_action "/exhibit/"; a_method `Post ]
           [ input ~a:[ a_name "content" ] () ]
       ; make_swapper "/transition" "swapped content" "swap it!"
       ])
;;

let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html

let get_one_exhibit (ex : Declare.Exhibit.exhibit) =
  let open Tyxml.Html in
  div
    [ txt (string_of_int ex.id)
    ; txt ex.content
    ; button
        ~a:
          [ Hx.delete (Declare.Exhibit.delete_link ex)
          ; Hx.swap OuterHTML
          ; Hx.target (Closest "div")
          ]
        [ txt "delete" ]
    ]
;;

let format_exhibits request =
  match%lwt Dream.sql request Declare.Exhibit.get_all with
  | Ok exhibits ->
    let open Tyxml.Html in
    let exhibits = List.map get_one_exhibit exhibits in
    html (mk_header "Exhibits") (body exhibits) |> html_to_string |> Dream.html
  | _ -> failwith "Something"
;;

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
       ; Dream.delete "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Declare.Exhibit.remove (int_of_string id))
           in
           match exhibit with
           | Ok _ -> Dream.html "deleted"
           | _ -> Dream.empty `Not_Found)
       ; Dream.get "/exhibit/:id" (fun request ->
           let id = Dream.param request "id" in
           let%lwt exhibit =
             Dream.sql request (Declare.Exhibit.get (int_of_string id))
           in
           match exhibit with
           | Ok exhibit -> Dream.html @@ format_exhibit exhibit
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
             let%lwt _ = Dream.sql request (Declare.Exhibit.add content) in
             Dream.html @@ html_to_string @@ greet request " POSTED "
             (* Dream.redirect request "/" *)
           | `Ok something ->
             List.iter (fun (k, v) -> Format.printf "  %s: %s@." k v) something;
             Dream.html @@ html_to_string @@ greet request " POSTED "
           | _ ->
             Format.printf "  bad request: s@.";
             Dream.empty `Bad_Request)
       ; Dream.post "/user/:id" (fun request ->
           let module User = Declare.Resource.User in
           let id = Dream.param request "id" in
           let%lwt user = Dream.sql request (User.read (int_of_string id)) in
           match user with
           | Ok user -> Dream.html @@ User.format (user |> Option.get)
           | _ -> Dream.empty `Not_Found)
       ]
;;
