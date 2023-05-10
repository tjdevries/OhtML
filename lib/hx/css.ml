module Selector = struct
  type selector =
    | Raw of string
    | Type of string
    | Class of string
    | Id of string
    | Attribute of string

  type t =
    | This
    | Raw of string
    | Closest of selector
end

(* TODO: Can use sig thing when I'm done?... *)
module Namespace = struct
  type t = string

  let create (x : string) : t = x
  let universal : t = create "*"
end

module Element = struct
  type t = string
end

module Simple = struct
  type t = Type of Namespace.t option * Element.t

  let s_type elt = Type (None, elt)
end
