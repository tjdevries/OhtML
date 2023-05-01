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
      let selection =
        List.map labels ~f:label_to_output |> String.concat ~sep:","
      in
      let selection =
        "SELECT "
        ^ selection
        ^ " FROM "
        ^ table_name
        ^ " where id = "
        ^ label_to_input id_label
      in
      let selection = make_str_const selection loc in
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
      let create = make_str_const create loc in
      let mirror_type_name = "data" in
      let mirror_type =
        ptyp_constr
          ~loc
          (Located.lident ~loc mirror_type_name)
          (List.map rec_type.ptype_params ~f:snd)
      in
      let mirror_type_decl =
        type_declaration
          ~name:(Located.mk ~loc mirror_type_name)
          ~params:rec_type.ptype_params
          ~cstrs:[]
          ~kind:rec_type.ptype_kind
          ~private_:rec_type.ptype_private
          ~manifest:(Some mirror_type)
          ()
      in
      (* [ pstr_type ~loc Recursive [ mirror_type_decl ] ] in *)
      [%stri
        include struct
          (* type data = *)
          (*   { name : string *)
          (*   ; user_id : int *)
          (*   } *)
          (* type data [%type { name : string; user_id; int }] *)
          [%e mirror_type_decl]

          let create = [%rapper get_one [%e create]]
          let create_record = [%rapper get_one [%e create] record_in]
          let read = [%rapper get_opt [%e selection] record_out]
        end]
    | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
;;

let args =
  let open Deriving.Args in
  empty +> arg "table_name" (estring __)
;;

let impl_generator = Deriving.Generator.V2.make args generate_impl
let model = Deriving.add "model" ~str_type_decl:impl_generator
