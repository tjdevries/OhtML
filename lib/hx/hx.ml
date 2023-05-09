open Base

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
let target t = Tyxml.Html.Unsafe.string_attrib "hx-target" (TargetType.to_string t)

(** hx-select. Select the content to swap into the page *)
let select sel = Tyxml.Html.Unsafe.string_attrib "hx-select" sel

let select_oob sel =
  Tyxml.Html.Unsafe.string_attrib "hx-select" (String.concat ~sep:"," sel)
;;

module SwapType = struct
  module Modifiers = struct
    type data =
      { transition : bool option
      ; swap : string option
      }

    type t = data option

    let create ~transition ~swap =
      match transition, swap with
      | None, None -> None
      | _, _ -> Some { transition; swap }
    ;;

    let to_string = function
      | None -> ""
      | Some data ->
        [ Some "" (* Empty string to start, to prefix with space *)
        ; Option.map data.transition ~f:(Fmt.str "transition:%b")
        ; Option.map data.swap ~f:(Fmt.str "swap:%s")
        ]
        |> List.filter_opt
        |> String.concat ~sep:" "
    ;;
    (* Option.map *)
  end

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
    ; modifiers : Modifiers.t
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
    let modifiers = Modifiers.to_string t.modifiers in
    Tyxml.Html.Unsafe.string_attrib "hx-swap" (attr ^ modifiers)
  ;;
end

(** Typesafe hx-swap wrapper *)
let swap ?transition ?swap attr =
  let open SwapType in
  to_attr { attr; modifiers = Modifiers.create ~transition ~swap }
;;

(* Misc Attributes *)
let boost status = Tyxml.Html.Unsafe.string_attrib "hx-boost" (Bool.to_string status)

let push_url status =
  Tyxml.Html.Unsafe.string_attrib "hx-push-url" (Bool.to_string status)
;;

(* TODO: hx-on. Would be very cool to use melange and encode the function name into some script:
          https://htmx.org/attributes/hx-on/ *)
