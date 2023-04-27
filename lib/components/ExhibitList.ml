open Tyxml.Html

let header = thead [ tr [ th [ txt "id" ]; th [ txt "content" ] ] ]

let row (ex : Ohtml.Exhibit.exhibit) =
  tr
    [ td [ txt @@ Int.to_string ex.id ]
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
