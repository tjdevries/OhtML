module User = struct
  type t =
    { id : int
    ; name : string
    }
  [@@deriving model]
end
