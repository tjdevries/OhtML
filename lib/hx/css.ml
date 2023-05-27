(** Get the name of some HTML element. <div> -> div *)
let get_name elt =
  let elt = Tyxml.Html.toelt elt in
  let doc = Tyxml.Xml.content elt in
  match doc with
  | Tyxml_xml.Leaf (name, _) -> name
  | Tyxml_xml.Node (name, _, _) -> name
  | _ -> failwith "I don't think this is possible from html..."
;;

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
  let none = create ""
  let universal = create "*"
end

module Element = struct
  type t = string

  let element elt =
    let name = get_name elt in
    { attribute = name; value = None }
  ;;
end

module AttributeSelector = struct
  type value =
    { operation : [ `Equal | `OneOf | `Exactly | `Prefix | `Suffix | `Contains ]
    ; namespace : Namespace.t option
    ; value : [ `Identifier of string | `String of string ]
    }

  type t =
    { attribute : string
    ; value : value option
    }
end

module Simple = struct
  type t =
    | Type of Namespace.t option * Element.t
    | Universal
    | Attribute of AttributeSelector.t

  let s_type elt = Type (None, elt)
  let s_universal = Universal
end
