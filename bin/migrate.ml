let () = print_endline "Migrate"

let unwrap m =
  match%lwt m with
  | Ok a -> a |> Lwt.return
  | Error e -> failwith @@ Caqti_error.show e
;;

let main () =
  (* Kill existing db *)
  (try Unix.unlink "db.sqlite" with
   | _ -> ());
  (* Migrate new db *)
  let connection = Uri.of_string "sqlite3:db.sqlite" in
  let%lwt connection = Caqti_lwt.connect connection |> unwrap in
  let%lwt _ = Declare.Sql.migrate connection in
  Declare.Sql.populate connection
;;

(* Lwt_main.run connection *)

let _ = Lwt_main.run @@ main ()
