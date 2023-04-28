let make_form ~id ~route ~target =
  let open Tyxml.Html in
  [ form
      ~a:
        [ a_id id
        ; Hx.post route
        ; Hx.swap OuterHTML
        ; Hx.target target
        ; a_enctype "multipart/form-data"
        ]
      [ label ~a:[ a_label_for id ] [ txt "image goes here: " ]
      ; input ~a:[ a_input_type `File; a_name "image" ] ()
      ; input ~a:[ a_input_type `Submit; a_value "Upload" ] ()
      ]
  ]
;;
