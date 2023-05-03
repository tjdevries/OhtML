open Ppxlib
open Ast_helper
open Ast_builder.Default
open Base

let core_type_is_option ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> true
  | _ -> false
;;

let label_to_output id_label label =
  match label.pld_name.txt with
  | "id" -> Fmt.str "@%s{id}" id_label
  | _ when core_type_is_option label.pld_type ->
    let ct =
      List.hd_exn
        (match label.pld_type.ptyp_desc with
         | Ptyp_constr (_, core_types) -> core_types
         | _ -> assert false)
    in
    Fmt.str "@%a?{%s}" Pprintast.core_type ct label.pld_name.txt
  | _ -> Fmt.str "@%a{%s}" Pprintast.core_type label.pld_type label.pld_name.txt
;;

let label_to_input label =
  if core_type_is_option label.pld_type
  then (
    let ct =
      List.hd_exn
        (match label.pld_type.ptyp_desc with
         | Ptyp_constr (_, core_types) -> core_types
         | _ -> assert false)
    in
    Fmt.str "%%%a?{%s}" Pprintast.core_type ct label.pld_name.txt)
  else
    Caml.Format.asprintf "%%%a{%s}" Pprintast.core_type label.pld_type label.pld_name.txt
;;

let make_str_const s loc = Exp.constant (Pconst_string (s, loc, None))

let mk_select ~selections ~table_name ~id_label ~loc =
  let selection =
    Fmt.str {| SELECT %s FROM %s WHERE id = %%%s{id} |} selections table_name id_label
  in
  make_str_const selection loc
;;

let mk_select_many ~selections ~table_name ~loc =
  let select_many = Fmt.str {| SELECT %s FROM %s |} selections table_name in
  make_str_const select_many loc
;;

let mk_create mode ~id_lbl ~fields ~loc ~table_name =
  let label_to_output = label_to_output "id" in
  let create_select =
    List.map fields ~f:(fun l -> l.pld_name.txt) |> String.concat ~sep:","
  in
  let create_values = List.map fields ~f:label_to_input |> String.concat ~sep:"," in
  let create_results = List.map fields ~f:label_to_output |> String.concat ~sep:"," in
  let create =
    Fmt.str
      {| INSERT INTO %s ( %s ) VALUES ( %s ) RETURNING @%s{id},%s |}
      table_name
      create_select
      create_values
      id_lbl
      create_results
  in
  let create = make_str_const create loc in
  match mode with
  | `Args -> [%expr [%rapper get_one [%e create] record_out]]
  | `Record -> [%expr [%rapper get_one [%e create] record_in record_out]]
;;

let mk_get_by_id ~selections ~table_name ~key ~foreign_name ~foreign_key ~loc =
  let get_query =
    Fmt.str
      {| SELECT %s FROM %s INNER JOIN %s ON %s.%s = %s.%s WHERE %s.%s = %%int{%s} |}
      selections
      table_name
      foreign_name
      table_name
      key
      foreign_name
      foreign_key
      table_name
      key
      key
  in
  make_str_const get_query loc
;;

let mk_type_data ~loc ~labels =
  let type_data =
    type_declaration
      ~loc
      ~name:Location.{ txt = "data"; loc }
      ~params:[]
      ~cstrs:[]
      ~kind:(Ptype_record labels)
      ~manifest:None
      ~private_:Public
  in
  pstr_type ~loc Nonrecursive [ type_data ]
;;

(* let extract_payload () = *)
(*   let open Ast_pattern in *)
(*   (* (pstr_value nonrecursive (value_binding ~pat:(pstring __) ~expr:__ ^:: nil) *) *)
(*   pstr *)
(*     (pstr_value nonrecursive (value_binding ~pat:(pstring __) ~expr:__ ^:: nil) *)
(*      ^:: nil) *)
(* ;; *)

let payloader () =
  let open Ast_pattern in
  (* let x = attribute ~name:("foreign", __, __) ~payload:__ in *)
  let x = single_expr_payload (estring __) in
  x
;;

let mk_foreign ~loc ~labels ~selections ~table_name =
  let foreign =
    List.find labels ~f:(fun l ->
      List.find l.pld_attributes ~f:(fun a -> String.(a.attr_name.txt = "foreign"))
      |> Option.is_some)
  in
  let foreign =
    Option.map foreign ~f:(fun foreign ->
      let payload = (List.hd_exn foreign.pld_attributes).attr_payload in
      let _ = Ast_pattern.parse_res (payloader ()) loc payload (fun a -> a) in
      let foreign_key = foreign.pld_name.txt in
      let foreign_name =
        match payload with
        | PStr
            [ { pstr_desc =
                  Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident lident; _ }; _ }, _)
              ; _
              }
            ] -> lident
        | _ -> Location.raise_errorf ~loc "Unsupported payload for @foreign"
      in
      let foreign_query =
        mk_get_by_id
          ~loc
          ~selections
          ~table_name
          ~key:foreign_key
          ~foreign_name
          ~foreign_key
      in
      let body = [%expr [%rapper get_many [%e foreign_query] record_out]] in
      let pat = ppat_var ~loc { txt = "get_by_" ^ foreign_key; loc } in
      let binding = Ast_builder.Default.value_binding ~loc ~pat ~expr:body in
      pstr_value ~loc Nonrecursive [ binding ])
  in
  match foreign with
  | Some foreign -> foreign
  | None -> [%stri let _ = 0]
;;

let generate_impl ~ctxt (_rec_flag, type_decls) table_name =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_decls ~f:(function
    | { ptype_kind = Ptype_record labels; ptype_manifest; ptype_name; _ } ->
      let table_name = Option.value_exn table_name in
      let id_lbl = List.find_exn labels ~f:(fun l -> String.(l.pld_name.txt = "id")) in
      let id_lbl =
        match id_lbl.pld_type.ptyp_desc with
        | Ptyp_constr ({ txt = Ldot (Lident li, _); _ }, _) -> li
        | _ -> assert false
      in
      let fields = List.filter labels ~f:(fun l -> String.(l.pld_name.txt <> "id")) in
      let label_to_output = label_to_output id_lbl in
      let selections = List.map labels ~f:label_to_output |> String.concat ~sep:"," in
      let foreign = mk_foreign ~loc ~labels ~selections ~table_name in
      let select = mk_select ~selections ~table_name ~id_label:id_lbl ~loc in
      let select_many = mk_select_many ~selections ~table_name ~loc in
      let create = mk_create `Args ~id_lbl ~fields ~table_name ~loc in
      let create_record = mk_create `Record ~id_lbl ~fields ~table_name ~loc in
      let type_data = mk_type_data ~loc ~labels:fields in
      [%stri
        include struct
          [%%i type_data]

          let create = [%e create]
          let create_record = [%e create_record]
          let read = [%rapper get_opt [%e select] record_out]
          let read_all = [%rapper get_many [%e select_many] record_out]

          [%%i foreign]
        end]
    | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
;;

let args =
  let open Deriving.Args in
  empty +> arg "table_name" (estring __)
;;

let impl_generator = Deriving.Generator.V2.make args generate_impl
let model = Deriving.add "model" ~str_type_decl:impl_generator
