type exhibit =
  { id : int
  ; content : string
  }

type error = Database_error of string

(* Migrations-related helper functions. *)
val migrate : Db.t -> (unit, error) result Lwt.t
val rollback : Db.t -> (unit, error) result Lwt.t

(* Core functions *)
val add : string -> Db.t -> (unit, error) result Lwt.t
val get : int -> Db.t -> (exhibit, error) result Lwt.t
val get_all : Db.t -> (exhibit list, error) result Lwt.t
val remove : int -> Db.t -> (unit, error) result Lwt.t

(* url functions *)
val get_link : int -> string
val delete_link : exhibit -> string

(* val get_all : unit -> (todo list, error) result Lwt.t *)
(* val clear : unit -> (unit, error) result Lwt.t *)
