open Tyxml.Html

(* TODO: Not sure I like this very much *)
type t = Ohtml.Exhibit.exhibit

let table_head =
  thead [ tr [ th [ txt "id" ]; th [ txt "user" ]; th [ txt "content" ] ] ]
;;

let table_row (ex : t) =
  tr
    [ td [ txt @@ Int.to_string ex.id ]
    ; td [ txt (ex.user_id |> Int.to_string) ]
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
