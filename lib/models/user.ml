module UserID = struct
  type t = int

  let t =
    let encode (t : t) : (int, string) result = Ok t in
    let decode (t : int) : (t, string) result = Ok t in
    Caqti_type.(custom ~encode ~decode int)
  ;;

  let mk (t : int) : t = t
end

module _ : Rapper.CUSTOM = UserID

type id = UserID.t

type t =
  { id : UserID.t
  ; name : string
  }
[@@deriving model ~table_name:"users"]

(* All of these are generated from my deriving macro that I've been working on *)
let _ = create
let _ = create_record
let _ = read
let _ = read_all

let format { id; name } =
  let open Tyxml_html in
  div [ h1 [ txt name ]; p [ txt (Printf.sprintf "ID: %d" id) ] ]
  |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
;;

let handle_get request =
  let id = Dream.param request "id" in
  let%lwt resource = Dream.sql request (read ~id:(int_of_string id)) in
  match resource with
  | Ok (Some resource) -> Dream.html @@ format resource
  | _ -> Dream.empty `Not_Found
;;
