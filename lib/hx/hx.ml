(*  TODO: hx-trigger response
          You can send a response header with basically an event name
          and then respond to that from within the rest of the body.
          So, you could auto refresh / re-update some table with a new HTTP
          request, but decoupled from the rest of the project. 

          Smart thing to do would be to make some type/module that has events
          and use that from within your own stuff to make sure you're only emiting
          events that can be handled. Something to think about.

          https://hypermedia.systems/book/deep-htmx/ -> Server Generated Events *)

(* 
   HX-Location
    Causes a client-side redirection to a new location

  HX-Push-Url
    Pushes a new URL into the location bar

  HX-Refresh
    Refreshes the current page

  HX-Retarget
    Allows you to specify a new target to swap the response content into on the client side *)

(* <meta name="htmx-config" content='{"defaultSwapStyle":"outerHTML"}'> *)

open Base

(** Typesafe htmlx wrappers for OCaml. Emits attributes to be used with TyXML *)

(* TODO: Would be cool to break this down a bit more (class, id, etc.).
         Down with strings! *)
type css_selector = string

(** hx-get wrapper. TODO: Should use different link type *)
let get link = Tyxml.Html.Unsafe.string_attrib "hx-get" link

(** hx-post wrapper. TODO: Shoudl use different link type *)
let post link = Tyxml.Html.Unsafe.string_attrib "hx-post" link

(** hx-delete wrapper. TODO: Should use different link type *)
let delete str = Tyxml.Html.Unsafe.string_attrib "hx-delete" str

module TargetType = struct
  type t =
    | This
    | Css of css_selector
    | Closest of css_selector
    | Find of css_selector
    | Previous of css_selector

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
    type scrolldir =
      | Top
      | Bottom

    type scrolling = scrolldir * string option

    let scroll_to_string prefix scroll =
      let map_dir = function
        | Top -> "top"
        | Bottom -> "bottom"
      in
      match scroll with
      | dir, Some modifier -> Fmt.str "%s:%s:%s" prefix modifier (map_dir dir)
      | dir, None -> Fmt.str "%s:%s" prefix (map_dir dir)
    ;;

    type data =
      { transition : bool option
      ; swap : string option
      ; settle : string option
      ; scroll : scrolling option
      ; show : scrolling option
      ; focus_scroll : bool option
      }

    type t = data option

    let create ~transition ~swap ~settle ~scroll ~show ~focus_scroll =
      match transition, swap with
      | None, None -> None
      | _, _ -> Some { transition; swap; settle; scroll; show; focus_scroll }
    ;;

    let to_string = function
      | None -> ""
      | Some data ->
        [ Some "" (* Empty string to start, to prefix with space *)
        ; Option.map data.transition ~f:(Fmt.str "transition:%b")
        ; Option.map data.swap ~f:(Fmt.str "swap:%s")
        ; Option.map data.settle ~f:(Fmt.str "settle:%s")
        ; Option.map data.scroll ~f:(scroll_to_string "scroll")
        ; Option.map data.show ~f:(scroll_to_string "show")
        ; Option.map data.focus_scroll ~f:(Fmt.str "focus-scroll:%b")
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
let swap ?transition ?swap ?settle ?scroll ?show ?focus_scroll attr =
  let open SwapType in
  to_attr
    { attr
    ; modifiers = Modifiers.create ~transition ~swap ~settle ~scroll ~show ~focus_scroll
    }
;;

module TriggerType = struct
  type trigger =
    | Event of string
    | Every of string

  type modifier =
    | Once
    | Changed
    | Delay of string
    | Throttle of string
    | Target of string
    | From of [ `Document | `Window | `Closest of string | `Find of string ]
    | Consume
    | Queue of [ `First | `Last | `All | `None ]
    | Load
        (** Trigged on load (useful for lazy-loading) *)
    | Revealed
        (** Triggered when an element is scrolled into the viewport (useful for lazy-loading).
            If you are using `overflow` you should `intersect once` instead of Revealed *)
    | Intersect of [ `Default | `Root of string | `Threshold of float ]

  type t = trigger list * modifier list

  let init ?(modifiers = []) triggers = triggers, modifiers
end

(* TODO: hx-swap-oob? I don't think I understand. Need to look at some more examples. *)

(* Misc Attributes *)
let boost status = Tyxml.Html.Unsafe.string_attrib "hx-boost" (Bool.to_string status)

let push_url status =
  Tyxml.Html.Unsafe.string_attrib "hx-push-url" (Bool.to_string status)
;;

let indicator elt =
  let _ = elt in
  assert false
;;

(* TODO: hx-on. Would be very cool to use melange and encode the function name into some script:
          https://htmx.org/attributes/hx-on/ *)

module Headers = struct
  (* don't expose *)
  let htmx_truthy_header header req =
    Dream.headers req header
    |> List.find ~f:(fun header -> String.(header = "true"))
    |> Option.is_some
  ;;

  let is_htmx req = htmx_truthy_header "HX-Request" req
  let is_boosted req = htmx_truthy_header "HX-Boosted" req
  let is_history_restore req = htmx_truthy_header "HX-History-Restore-Request" req

  (** This will contain the user response to an hx-prompt *)
  let get_prompt req = Dream.headers req "HX-Prompt" |> List.hd

  (** This value will be the id of the target element, if it exists *)
  let get_target_id req = Dream.headers req "HX-Target" |> List.hd

  (** This value will be the id of the triggered element, if it exists *)
  let get_trigger_id req = Dream.headers req "HX-Trigger" |> List.hd

  (** This value will be the name of the triggered element, if it exists *)
  let get_trigger_name req = Dream.headers req "HX-Trigger-Name" |> List.hd
end
