module User = struct
  type t =
    { id : int
    ; name : string
    ; image_id : int [@foreign images]
    }
  [@@deriving model ~table_name:"users"]
end

let do_something db =
  let%lwt _ = User.create ~name:"hello" ~image_id:10 db in
  let%lwt _ = User.create_record { name = "something"; image_id = 5 } db in
  let%lwt _ = User.read_all () db in
  let%lwt _ = User.get_by_image_id ~image_id:5 db in
  let%lwt user = User.read ~id:1 db in
  match user with
  | Ok (Some user) -> user.name |> Lwt.return
  | _ -> "something" |> Lwt.return
;;
