module type DB = Caqti_lwt.CONNECTION

type t = (module DB)
