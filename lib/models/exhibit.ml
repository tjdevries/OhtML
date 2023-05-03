module ExhibitID = struct
  type t = int

  let t =
    let encode (t : t) : (int, string) result = Ok t in
    let decode (t : int) : (t, string) result = Ok t in
    Caqti_type.(custom ~encode ~decode int)
  ;;

  let mk (t : int) : t = t
end

module _ : Rapper.CUSTOM = ExhibitID
module UserID = User.UserID

type t =
  { id : ExhibitID.t
  ; user_id : int
  ; image_id : int option
  ; content : string
  }
[@@deriving model ~table_name:"exhibits"]

let delete = [%rapper execute "DELETE FROM exhibits WHERE id = %int{id}"]
let route = "/exhibit"

let route_for_id ?mode id =
  match mode with
  | Some `List -> "/exhibit/list/" ^ Int.to_string id
  | _ -> "/exhibit/" ^ Int.to_string id
;;

let route_for ex = route_for_id ex.id

let get_comments =
  [%rapper
    get_many
      {| SELECT @string{comments.content} from comments
            INNER JOIN exhibits on exhibits.id = comments.exhibit_id
            WHERE exhibits.id = %int{id} |}]
;;
