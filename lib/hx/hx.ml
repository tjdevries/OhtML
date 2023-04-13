(** Typesafe htmlx wrappers for OCaml. Emits attributes to be used with TyXML *)

(** hx-get wrapper. TODO: Should use different link type *)
let get link = Tyxml.Html.Unsafe.string_attrib "hx-get" link

(** hx-post wrapper. TODO: Shoudl use different link type *)
let post link = Tyxml.Html.Unsafe.string_attrib "hx-post" link

(** hx-delete wrapper. TODO: Should use different link type *)
let delete str = Tyxml.Html.Unsafe.string_attrib "hx-delete" str

module TargetType = struct
  type t =
    | This
    | Css of string
    | Closest of string
    | Find of string
    | Previous of string

  let to_string = function
    | This -> "this"
    | Css s -> s
    | Closest s -> "closest " ^ s
    | Find s -> "find " ^ s
    | Previous s -> "previous " ^ s
  ;;
end

(** Typesafe hx-target wrapper *)
let target t =
  Tyxml.Html.Unsafe.string_attrib "hx-target" (TargetType.to_string t)
;;

module SwapType = struct
  type modifiers = { transition : bool }

  type attr =
    | InnerHTML
        (** The default, replace the inner html of the target element *)
    | OuterHTML
        (** Replace the entire target element with the response *)
    | BeforeBegin
        (** Insert the response before the target element *)
    | AfterBegin
        (** Insert the response before the first child of the target element *)
    | BeforeEnd
        (** Insert the response after the last child of the target element *)
    | AfterEnd
        (** Insert the response after the target element *)
    | Delete
        (** Deletes the target element regardless of the response *)
    | None
        (** Does not append content from response (out of band items will still be processed). *)

  type t =
    { attr : attr
    ; modifiers : modifiers option
    }

  let to_attr t =
    let attr =
      match t.attr with
      | InnerHTML -> "innerHTML"
      | OuterHTML -> "outerHTML"
      | BeforeBegin -> "beforebegin"
      | AfterBegin -> "afterbegin"
      | BeforeEnd -> "beforeend"
      | AfterEnd -> "afterend"
      | Delete -> "delete"
      | None -> "none"
    in
    let modifiers =
      match t.modifiers with
      | Some { transition = true } -> " transition"
      | _ -> ""
    in
    Tyxml.Html.Unsafe.string_attrib "hx-swap" (modifiers ^ attr)
  ;;
end

(** Typesafe hx-swap wrapper *)
let swap ?transition attr =
  let open SwapType in
  to_attr
    { attr
    ; modifiers = Option.map transition ~f:(fun transition -> { transition })
    }
;;
