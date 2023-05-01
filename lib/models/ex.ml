module User = struct
  type t =
    { id : int
    ; name : string
    ; user_id : int
    }
  [@@deriving model ~table_name:"users"]
end

let do_something db =
  let%lwt _ = User.create ~name:"hello" ~user_id:10 db in
  let%lwt user = User.read ~id:1 db in
  match user with
  | Ok (Some user) -> user.name |> Lwt.return
  | _ -> "something" |> Lwt.return
;;
