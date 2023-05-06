open Tyxml.Html
module Exhibit = Models.Exhibit

(* TODO: Not sure I like this very much *)
type t = Exhibit.t

let table_head = thead [ tr [ th [ txt "id" ]; th [ txt "user" ]; th [ txt "content" ] ] ]

let table_row (user : Models.User.t option) (ex : t) =
  tr
    [ td [ txt @@ Int.to_string ex.id ]
    ; td [ txt (ex.user_id |> Int.to_string) ]
    ; td [ txt ex.content ]
    ; td [ a ~a:[ a_href (Exhibit.route_for ex) ] [ txt "get" ] ]
    ; (match user with
       | Some user when user.id = ex.user_id ->
         td
           [ button
               ~a:
                 [ Hx.delete (Exhibit.route_for ex)
                 ; Hx.swap OuterHTML
                 ; Hx.target (Closest "button")
                 ]
               [ txt "delete" ]
           ]
       | _ -> td [ txt "" ])
    ]
;;

let page user page_header exhibits =
  let open Tyxml.Html in
  let exhibits = List.map exhibits ~f:(table_row user) in
  let exhibits =
    div [ table ~thead:table_head exhibits ]
    ::
    (match user with
     | Some _ -> [ ExhibitPost.form ~target:(Previous "tbody") ]
     | None -> [])
  in
  html page_header (body exhibits) |> Fmt.str "%a" (Tyxml.Html.pp ()) |> Dream.html
;;
