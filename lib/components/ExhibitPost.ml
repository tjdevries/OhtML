let form ~target =
  let open Tyxml.Html in
  form
    ~a:
      [ Hx.post "/exhibit/"
      ; Hx.target target
      ; Hx.swap BeforeEnd (* ; Hx.boost true *)
      ]
    [ input ~a:[ a_name "content" ] (); button [ txt "submit" ] ]
;;
