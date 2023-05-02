open Ppxlib
open Ast_helper
open Ast_builder.Default
open Base

let label_to_output label =
  Caml.Format.asprintf
    "@%a{%s}"
    Pprintast.core_type
    label.pld_type
    label.pld_name.txt
;;

let label_to_input label =
  Caml.Format.asprintf
    "%%%a{%s}"
    Pprintast.core_type
    label.pld_type
    label.pld_name.txt
;;

let make_str_const s loc = Exp.constant (Pconst_string (s, loc, None))

let mk_select ~selections ~table_name ~id_label ~loc =
  let selection =
    Fmt.str
      {| SELECT %s FROM %s WHERE id = %s |}
      selections
      table_name
      (label_to_input id_label)
  in
  make_str_const selection loc
;;

let mk_select_many ~selections ~table_name ~loc =
  let select_many = Fmt.str {| SELECT %s FROM %s |} selections table_name in
  make_str_const select_many loc
;;

let mk_create ~id_label ~non_id_labels ~loc ~table_name =
  let create_select =
    List.map non_id_labels ~f:(fun l -> l.pld_name.txt)
    |> String.concat ~sep:","
  in
  let create_values =
    List.map non_id_labels ~f:label_to_input |> String.concat ~sep:","
  in
  let create =
    "INSERT INTO "
    ^ table_name
    ^ " ("
    ^ create_select
    ^ ") VALUES ("
    ^ create_values
    ^ ") RETURNING "
    ^ label_to_output id_label
  in
  make_str_const create loc
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

let generate_impl ~ctxt (_rec_flag, type_decls) table_name =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_decls ~f:(function
    | { ptype_kind = Ptype_record labels; ptype_manifest; ptype_name; _ } ->
      let table_name = Option.value_exn table_name in
      let id_label =
        List.find_exn labels ~f:(fun l -> String.(l.pld_name.txt = "id"))
      in
      let non_id_labels =
        List.filter labels ~f:(fun l -> String.(l.pld_name.txt <> "id"))
      in
      let selections =
        List.map labels ~f:label_to_output |> String.concat ~sep:","
      in
      let foreign =
        List.find labels ~f:(fun l ->
          List.find l.pld_attributes ~f:(fun a ->
            String.(a.attr_name.txt = "foreign"))
          |> Option.is_some)
      in
      let foreign =
        Option.map foreign ~f:(fun foreign ->
          let payload = (List.hd_exn foreign.pld_attributes).attr_payload in
          let foreign_key = foreign.pld_name.txt in
          let foreign_name =
            match payload with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( { pexp_desc = Pexp_ident { txt = Lident lident; _ }
                          ; _
                          }
                        , _ )
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
          (* let id_argument = ppat_var ~loc { txt = foreign_key; loc } in *)
          (* let expr = pexp_fun ~loc Nolabel None id_argument body in *)
          let pat = ppat_var ~loc { txt = "get_by_" ^ foreign_key; loc } in
          let binding =
            Ast_builder.Default.value_binding ~loc ~pat ~expr:body
          in
          pstr_value ~loc Nonrecursive [ binding ])
      in
      let foreign =
        match foreign with
        | Some foreign -> foreign
        | None -> [%stri let _ = 0]
      in
      let select = mk_select ~selections ~table_name ~id_label ~loc in
      let select_many = mk_select_many ~selections ~table_name ~loc in
      let create = mk_create ~id_label ~non_id_labels ~table_name ~loc in
      let data_type =
        type_declaration
          ~loc
          ~name:Location.{ txt = "data"; loc }
          ~params:[]
          ~cstrs:[]
          ~kind:(Ptype_record non_id_labels)
          ~manifest:None
          ~private_:Public
      in
      let data_type = pstr_type ~loc Nonrecursive [ data_type ] in
      [%stri
        include struct
          [%%i data_type]

          let create = [%rapper get_one [%e create]]
          let create_record = [%rapper get_one [%e create] record_in]
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
