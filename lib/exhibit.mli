type exhibit =
  { id : int
  ; content : string
  }

(* Migrations-related helper functions. *)
val migrate : Database.t -> (unit, Database.error) result Lwt.t
val rollback : Database.t -> (unit, Database.error) result Lwt.t

(* Core functions *)
val add : string -> Database.t -> (unit, Database.error) result Lwt.t
val get : int -> Database.t -> (exhibit, Database.error) result Lwt.t
val get_all : Database.t -> (exhibit list, Database.error) result Lwt.t
val remove : int -> Database.t -> (unit, Database.error) result Lwt.t

(* url functions *)
val get_link : int -> string
val delete_link : exhibit -> string

(* val get_all : unit -> (todo list, error) result Lwt.t *)
(* val clear : unit -> (unit, error) result Lwt.t *)
