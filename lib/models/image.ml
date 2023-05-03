module ImageID = struct
  type t = int

  let t =
    let encode (t : t) : (int, string) result = Ok t in
    let decode (t : int) : (t, string) result = Ok t in
    Caqti_type.(custom ~encode ~decode int)
  ;;

  let mk (t : int) : t = t
end

module _ : Rapper.CUSTOM = ImageID

type t =
  { id : ImageID.t
  ; data : string
  }
[@@deriving model ~table_name:"images"]
