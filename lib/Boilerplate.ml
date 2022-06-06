(**
   Boilerplate to be used as a template when mapping the tsx CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok
  )

let map_private_property_identifier (env : env) (tok : CST.private_property_identifier) =
  (* private_property_identifier *) token env tok

let map_import (env : env) (tok : CST.import) =
  (* import *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Decl tok -> (* "declare" *) token env tok
  | `Name tok -> (* "namespace" *) token env tok
  | `Type tok -> (* "type" *) token env tok
  | `Public tok -> (* "public" *) token env tok
  | `Priv tok -> (* "private" *) token env tok
  | `Prot tok -> (* "protected" *) token env tok
  | `Over tok -> (* "override" *) token env tok
  | `Read tok -> (* "readonly" *) token env tok
  | `Module tok -> (* "module" *) token env tok
  | `Any tok -> (* "any" *) token env tok
  | `Num tok -> (* "number" *) token env tok
  | `Bool tok -> (* "boolean" *) token env tok
  | `Str tok -> (* "string" *) token env tok
  | `Symb tok -> (* "symbol" *) token env tok
  | `Export tok -> (* "export" *) token env tok
  | `Choice_get x ->
      (match x with
      | `Get tok -> (* "get" *) token env tok
      | `Set tok -> (* "set" *) token env tok
      | `Async tok -> (* "async" *) token env tok
      | `Static tok -> (* "static" *) token env tok
      | `Export tok -> (* "export" *) token env tok
      )
  )

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_regex_pattern (env : env) (tok : CST.regex_pattern) =
  (* regex_pattern *) token env tok

let map_anon_choice_let_ca16eb3 (env : env) (x : CST.anon_choice_let_ca16eb3) =
  (match x with
  | `Let tok -> (* "let" *) token env tok
  | `Const tok -> (* "const" *) token env tok
  )

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  (* pattern #!.* *) token env tok

let map_regex_flags (env : env) (tok : CST.regex_flags) =
  (* pattern [a-z]+ *) token env tok

let map_unescaped_double_string_fragment (env : env) (tok : CST.unescaped_double_string_fragment) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_predefined_type (env : env) (x : CST.predefined_type) =
  (match x with
  | `Any tok -> (* "any" *) token env tok
  | `Num tok -> (* "number" *) token env tok
  | `Bool tok -> (* "boolean" *) token env tok
  | `Str tok -> (* "string" *) token env tok
  | `Symb tok -> (* "symbol" *) token env tok
  | `Void tok -> (* "void" *) token env tok
  | `Unkn tok -> (* "unknown" *) token env tok
  | `Never tok -> (* "never" *) token env tok
  | `Obj tok -> (* "object" *) token env tok
  )

let map_accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  (match x with
  | `Public tok -> (* "public" *) token env tok
  | `Priv tok -> (* "private" *) token env tok
  | `Prot tok -> (* "protected" *) token env tok
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_ternary_qmark (env : env) (tok : CST.ternary_qmark) =
  (* ternary_qmark *) token env tok

let map_jsx_text (env : env) (tok : CST.jsx_text) =
  (* pattern [^{}<>]+ *) token env tok

let map_anon_choice_get_8fb02de (env : env) (x : CST.anon_choice_get_8fb02de) =
  (match x with
  | `Get tok -> (* "get" *) token env tok
  | `Set tok -> (* "set" *) token env tok
  | `STAR tok -> (* "*" *) token env tok
  )

let map_anon_choice_DOT_d88d0af (env : env) (x : CST.anon_choice_DOT_d88d0af) =
  (match x with
  | `DOT tok -> (* "." *) token env tok
  | `QMARKDOT tok -> (* "?." *) token env tok
  )

let map_unescaped_single_string_fragment (env : env) (tok : CST.unescaped_single_string_fragment) =
  (* pattern "[^'\\\\]+" *) token env tok

let map_template_chars (env : env) (tok : CST.template_chars) =
  (* template_chars *) token env tok

let map_function_signature_automatic_semicolon (env : env) (tok : CST.function_signature_automatic_semicolon) =
  (* function_signature_automatic_semicolon *) token env tok

let map_anon_choice_type_2b11f6b (env : env) (x : CST.anon_choice_type_2b11f6b) =
  (match x with
  | `Type tok -> (* "type" *) token env tok
  | `Typeof tok -> (* "typeof" *) token env tok
  )

let map_meta_property (env : env) ((v1, v2, v3) : CST.meta_property) =
  let v1 = (* "new" *) token env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "target" *) token env v3 in
  todo env (v1, v2, v3)

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> (* automatic_semicolon *) token env tok
  | `SEMI tok -> (* ";" *) token env tok
  )

let map_regex (env : env) ((v1, v2, v3, v4) : CST.regex) =
  let v1 = (* "/" *) token env v1 in
  let v2 = (* regex_pattern *) token env v2 in
  let v3 = (* "/" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> (* pattern [a-z]+ *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Unes_double_str_frag tok ->
              (* pattern "[^\"\\\\]+" *) token env tok
          | `Esc_seq tok -> (* escape_sequence *) token env tok
          )
        ) v2
      in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Unes_single_str_frag tok ->
              (* pattern "[^'\\\\]+" *) token env tok
          | `Esc_seq tok -> (* escape_sequence *) token env tok
          )
        ) v2
      in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)
  )

let map_anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  (match x with
  | `COMMA tok -> (* "," *) token env tok
  | `Choice_auto_semi x -> map_semicolon env x
  )

let map_anon_choice_type_id_dd17e7d (env : env) (x : CST.anon_choice_type_id_dd17e7d) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Choice_decl x -> map_reserved_identifier env x
  )

let map_anon_choice_rese_id_515394d (env : env) (x : CST.anon_choice_rese_id_515394d) =
  (match x with
  | `Choice_decl x -> map_reserved_identifier env x
  | `Id tok -> (* identifier *) token env tok
  )

let map_namespace_import_export (env : env) ((v1, v2, v3) : CST.namespace_import_export) =
  let v1 = (* "*" *) token env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

let map_import_export_specifier (env : env) ((v1, v2, v3) : CST.import_export_specifier) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_type_2b11f6b env x
    | None -> todo env ())
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = (* identifier *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_infer_type (env : env) ((v1, v2) : CST.infer_type) =
  let v1 = (* "infer" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  todo env (v1, v2)

let map_jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  (match x with
  | `Jsx_id tok ->
      (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok
  | `Id tok -> (* identifier *) token env tok
  )

let rec map_anon_choice_type_id_42c0412 (env : env) (x : CST.anon_choice_type_id_42c0412) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Nested_id x -> map_nested_identifier env x
  )

and map_nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

let rec map_anon_choice_type_id_b8f8ced (env : env) (x : CST.anon_choice_type_id_b8f8ced) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Deco_member_exp x ->
      map_decorator_member_expression env x
  )

and map_decorator_member_expression (env : env) ((v1, v2, v3) : CST.decorator_member_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

let map_anon_choice_priv_prop_id_89abb74 (env : env) (x : CST.anon_choice_priv_prop_id_89abb74) =
  (match x with
  | `Priv_prop_id tok ->
      (* private_property_identifier *) token env tok
  | `Id tok -> (* identifier *) token env tok
  )

let map_identifier_ (env : env) (x : CST.identifier_) =
  (match x with
  | `Unde tok -> (* "undefined" *) token env tok
  | `Id tok -> (* identifier *) token env tok
  )

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 = map_string_ env v2 in
  todo env (v1, v2)

let map_import_require_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = (* "require" *) token env v3 in
  let v4 = (* "(" *) token env v4 in
  let v5 = map_string_ env v5 in
  let v6 = (* ")" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_literal_type (env : env) (x : CST.literal_type) =
  (match x with
  | `Num_ (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> (* "-" *) token env tok
        | `PLUS tok -> (* "+" *) token env tok
        )
      in
      let v2 = (* number *) token env v2 in
      todo env (v1, v2)
  | `Num tok -> (* number *) token env tok
  | `Str x -> map_string_ env x
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok
  | `Null tok -> (* "null" *) token env tok
  | `Unde tok -> (* "undefined" *) token env tok
  )

let map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d (env : env) ((v1, v2) : CST.anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d) =
  let v1 = map_import_export_specifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_import_export_specifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = map_jsx_identifier_ env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_jsx_identifier_ env v3 in
  todo env (v1, v2, v3)

let map_nested_type_identifier (env : env) ((v1, v2, v3) : CST.nested_type_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  todo env (v1, v2, v3)

let map_export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  (match x with
  | `Choice_jsx_id x -> map_jsx_identifier_ env x
  | `Jsx_name_name x -> map_jsx_namespace_name env x
  )

let map_jsx_element_name (env : env) (x : CST.jsx_element_name) =
  (match x with
  | `Choice_jsx_id x -> map_jsx_identifier_ env x
  | `Nested_id x -> map_nested_identifier env x
  | `Jsx_name_name x -> map_jsx_namespace_name env x
  )

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Name_import_export x -> map_namespace_import_export env x
  | `Named_imports x -> map_named_imports env x
  | `Id_opt_COMMA_choice_name_import_export (v1, v2) ->
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | `Name_import_export x -> map_namespace_import_export env x
              | `Named_imports x -> map_named_imports env x
              )
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

let map_jsx_closing_element (env : env) ((v1, v2, v3, v4) : CST.jsx_closing_element) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* "/" *) token env v2 in
  let v3 = map_jsx_element_name env v3 in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

let rec map_abstract_method_signature (env : env) ((v1, v2, v3, v4, v5, v6) : CST.abstract_method_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 = (* "abstract" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v4 = map_property_name env v4 in
  let v5 =
    (match v5 with
    | Some tok -> (* "?" *) token env tok
    | None -> todo env ())
  in
  let v6 = map_call_signature_ env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  (match x with
  | `Exp x -> map_expression env x
  | `Spread_elem x -> map_spread_element env x
  )

and map_anon_choice_exp_9cd0ed5 (env : env) (x : CST.anon_choice_exp_9cd0ed5) =
  (match x with
  | `Exp x -> map_expression env x
  | `Prim_exp x -> map_primary_expression env x
  )

and map_anon_choice_export_stmt_f90d83f (env : env) (x : CST.anon_choice_export_stmt_f90d83f) =
  (match x with
  | `Export_stmt x -> map_export_statement env x
  | `Prop_sign (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_accessibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> (* "static" *) token env tok
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> (* "override" *) token env tok
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> (* "readonly" *) token env tok
        | None -> todo env ())
      in
      let v5 = map_property_name env v5 in
      let v6 =
        (match v6 with
        | Some tok -> (* "?" *) token env tok
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Call_sign_ x -> map_call_signature_ env x
  | `Cons_sign (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "abstract" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "new" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Index_sign x -> map_index_signature env x
  | `Meth_sign x -> map_method_signature env x
  )

and map_anon_choice_jsx_attr_name_b052322 (env : env) (x : CST.anon_choice_jsx_attr_name_b052322) =
  (match x with
  | `Choice_choice_jsx_id x -> map_jsx_attribute_name env x
  | `Choice_id_opt_type_args (v1, v2) ->
      let v1 = map_anon_choice_type_id_42c0412 env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

and map_anon_choice_pair_20c9acd (env : env) (x : CST.anon_choice_pair_20c9acd) =
  (match x with
  | `Pair (v1, v2, v3) ->
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Spread_elem x -> map_spread_element env x
  | `Meth_defi x -> map_method_definition env x
  | `Choice_id x -> map_anon_choice_type_id_dd17e7d env x
  )

and map_anon_choice_pair_pat_3ff9cbe (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) =
  (match x with
  | `Pair_pat (v1, v2, v3) ->
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_anon_choice_pat_3297d92 env v3 in
      todo env (v1, v2, v3)
  | `Rest_pat x -> map_rest_pattern env x
  | `Obj_assign_pat (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Choice_choice_decl x ->
            map_anon_choice_rese_id_515394d env x
        | `Dest_pat x -> map_destructuring_pattern env x
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Choice_id x -> map_anon_choice_type_id_dd17e7d env x
  )

and map_anon_choice_pat_3297d92 (env : env) (x : CST.anon_choice_pat_3297d92) =
  (match x with
  | `Pat x -> map_pattern env x
  | `Assign_pat (v1, v2, v3) ->
      let v1 = map_pattern env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_prop_name_6cc9e4b (env : env) (x : CST.anon_choice_prop_name_6cc9e4b) =
  (match x with
  | `Prop_name x -> map_property_name env x
  | `Enum_assign (v1, v2) ->
      let v1 = map_property_name env v1 in
      let v2 = map_initializer_ env v2 in
      todo env (v1, v2)
  )

and map_anon_choice_type_id_940079a (env : env) (x : CST.anon_choice_type_id_940079a) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Dest_pat x -> map_destructuring_pattern env x
  )

and map_anon_choice_type_id_a85f573 (env : env) (x : CST.anon_choice_type_id_a85f573) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Nested_type_id x -> map_nested_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  )

and map_anon_choice_type_id_e96bf13 (env : env) (x : CST.anon_choice_type_id_e96bf13) =
  (match x with
  | `Id tok -> (* identifier *) token env tok
  | `Type_query_subs_exp x ->
      map_type_query_subscript_expression env x
  | `Type_query_member_exp x ->
      map_type_query_member_expression env x
  | `Type_query_call_exp x ->
      map_type_query_call_expression env x
  )

and map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env) (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  (match opt with
  | Some (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> map_anon_choice_exp_9818c1b env x
        | None -> todo env ())
      in
      let v2 = map_anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      todo env (v1, v2)
  | None -> todo env ())

and map_anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env) (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  List.map (fun (v1, v2) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> map_anon_choice_exp_9818c1b env x
      | None -> todo env ())
    in
    todo env (v1, v2)
  ) xs

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

and map_arrow_function (env : env) ((v1, v2, v3, v4) : CST.arrow_function) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Choice_choice_decl x ->
        map_anon_choice_rese_id_515394d env x
    | `Call_sign x -> map_call_signature_ env x
    )
  in
  let v3 = (* "=>" *) token env v3 in
  let v4 =
    (match v4 with
    | `Exp x -> map_expression env x
    | `Stmt_blk x -> map_statement_block env x
    )
  in
  todo env (v1, v2, v3, v4)

and map_asserts (env : env) ((v1, v2, v3) : CST.asserts) =
  let v1 = (* ":" *) token env v1 in
  let v2 = (* "asserts" *) token env v2 in
  let v3 =
    (match v3 with
    | `Type_pred x -> map_type_predicate env x
    | `Id tok -> (* identifier *) token env tok
    | `This tok -> (* "this" *) token env tok
    )
  in
  todo env (v1, v2, v3)

and map_augmented_assignment_lhs (env : env) (x : CST.augmented_assignment_lhs) =
  (match x with
  | `Choice_member_exp x ->
      (match x with
      | `Member_exp x -> map_member_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Choice_decl x -> map_reserved_identifier env x
      | `Id tok -> (* identifier *) token env tok
      | `Paren_exp x -> map_parenthesized_expression env x
      )
  | `Non_null_exp x -> map_non_null_expression env x
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "instanceof" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Exp_opt_type_args_choice_args (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Args x -> map_arguments env x
        | `Temp_str x -> map_template_string env x
        )
      in
      todo env (v1, v2, v3)
  | `Prim_exp_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "?." *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_call_signature (env : env) ((v1, v2, v3) : CST.call_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Type_anno x -> map_type_annotation env x
        | `Asserts x -> map_asserts env x
        | `Type_pred_anno x -> map_type_predicate_annotation env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_call_signature_ (env : env) (x : CST.call_signature_) =
  map_call_signature env x

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some x -> map_type_annotation env x
          | None -> todo env ())
        in
        let v4 = (* ")" *) token env v4 in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 = map_statement_block env v3 in
  todo env (v1, v2, v3)

and map_class_ (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 = (* "class" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> (* identifier *) token env tok
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_class_heritage env x
    | None -> todo env ())
  in
  let v6 = map_class_body env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Deco x -> map_decorator env x
      | `Meth_defi_opt_choice_auto_semi (v1, v2) ->
          let v1 = map_method_definition env v1 in
          let v2 =
            (match v2 with
            | Some x -> map_semicolon env x
            | None -> todo env ())
          in
          todo env (v1, v2)
      | `Meth_sign_choice_func_sign_auto_semi (v1, v2) ->
          let v1 = map_method_signature env v1 in
          let v2 =
            (match v2 with
            | `Func_sign_auto_semi tok ->
                (* function_signature_automatic_semicolon *) token env tok
            | `COMMA tok -> (* "," *) token env tok
            )
          in
          todo env (v1, v2)
      | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) ->
          let v1 =
            (match v1 with
            | `Abst_meth_sign x -> map_abstract_method_signature env x
            | `Index_sign x -> map_index_signature env x
            | `Meth_sign x -> map_method_signature env x
            | `Public_field_defi x -> map_public_field_definition env x
            )
          in
          let v2 =
            (match v2 with
            | `Choice_auto_semi x -> map_semicolon env x
            | `COMMA tok -> (* "," *) token env tok
            )
          in
          todo env (v1, v2)
      )
    ) v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 = (* "class" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_class_heritage env x
    | None -> todo env ())
  in
  let v6 = map_class_body env v6 in
  let v7 =
    (match v7 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_class_heritage (env : env) (x : CST.class_heritage) =
  (match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = map_extends_clause env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_implements_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Imples_clause x -> map_implements_clause env x
  )

and map_constraint_ (env : env) ((v1, v2) : CST.constraint_) =
  let v1 =
    (match v1 with
    | `Extends tok -> (* "extends" *) token env tok
    | `COLON tok -> (* ":" *) token env tok
    )
  in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Choice_func_decl x ->
      (match x with
      | `Func_decl x -> map_function_declaration env x
      | `Gene_func_decl x ->
          map_generator_function_declaration env x
      | `Class_decl x -> map_class_declaration env x
      | `Lexi_decl x -> map_lexical_declaration env x
      | `Var_decl x -> map_variable_declaration env x
      )
  | `Func_sign (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "async" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_call_signature_ env v4 in
      let v5 =
        (match v5 with
        | `Choice_auto_semi x -> map_semicolon env x
        | `Func_sign_auto_semi tok ->
            (* function_signature_automatic_semicolon *) token env tok
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `Abst_class_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = List.map (map_decorator env) v1 in
      let v2 = (* "abstract" *) token env v2 in
      let v3 = (* "class" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_class_heritage env x
        | None -> todo env ())
      in
      let v7 = map_class_body env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Module (v1, v2) ->
      let v1 = (* "module" *) token env v1 in
      let v2 = map_module__ env v2 in
      todo env (v1, v2)
  | `Inte_module x -> map_internal_module env x
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "type" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = map_semicolon env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Enum_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "const" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_enum_body env v4 in
      todo env (v1, v2, v3, v4)
  | `Inte_decl (v1, v2, v3, v4, v5) ->
      let v1 = (* "interface" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_extends_type_clause env x
        | None -> todo env ())
      in
      let v5 = map_object_type env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Import_alias (v1, v2, v3, v4, v5) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_anon_choice_type_id_42c0412 env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Ambi_decl (v1, v2) ->
      let v1 = (* "declare" *) token env v1 in
      let v2 =
        (match v2 with
        | `Decl x -> map_declaration env x
        | `Global_stmt_blk (v1, v2) ->
            let v1 = (* "global" *) token env v1 in
            let v2 = map_statement_block env v2 in
            todo env (v1, v2)
        | `Module_DOT_id_COLON_type_choice_auto_semi (v1, v2, v3, v4, v5, v6) ->
            let v1 = (* "module" *) token env v1 in
            let v2 = (* "." *) token env v2 in
            let v3 = (* identifier *) token env v3 in
            let v4 = (* ":" *) token env v4 in
            let v5 = map_type_ env v5 in
            let v6 = map_semicolon env v6 in
            todo env (v1, v2, v3, v4, v5, v6)
        )
      in
      todo env (v1, v2)
  )

and map_decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> (* identifier *) token env tok
    | `Deco_member_exp x ->
        map_decorator_member_expression env x
    | `Deco_call_exp x -> map_decorator_call_expression env x
    )
  in
  todo env (v1, v2)

and map_decorator_call_expression (env : env) ((v1, v2) : CST.decorator_call_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 = map_arguments env v2 in
  todo env (v1, v2)

and map_default_type (env : env) ((v1, v2) : CST.default_type) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_destructuring_pattern (env : env) (x : CST.destructuring_pattern) =
  (match x with
  | `Obj_pat (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | Some x -> map_anon_choice_pair_pat_3ff9cbe env x
              | None -> todo env ())
            in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> map_anon_choice_pair_pat_3ff9cbe env x
                  | None -> todo env ())
                in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `Array_pat (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 =
              (match v1 with
              | Some x -> map_anon_choice_pat_3297d92 env x
              | None -> todo env ())
            in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> map_anon_choice_pat_3297d92 env x
                  | None -> todo env ())
                in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_statement env v2 in
  todo env (v1, v2)

and map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_prop_name_6cc9e4b env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_prop_name_6cc9e4b env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x ->
      (match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2) ->
          let v1 = (* "export" *) token env v1 in
          let v2 =
            (match v2 with
            | `STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = (* "*" *) token env v1 in
                let v2 = map_from_clause env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            | `Name_import_export_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = map_namespace_import_export env v1 in
                let v2 = map_from_clause env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = map_export_clause env v1 in
                let v2 = map_from_clause env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_choice_auto_semi (v1, v2) ->
                let v1 = map_export_clause env v1 in
                let v2 = map_semicolon env v2 in
                todo env (v1, v2)
            )
          in
          todo env (v1, v2)
      | `Rep_deco_export_choice_decl (v1, v2, v3) ->
          let v1 = List.map (map_decorator env) v1 in
          let v2 = (* "export" *) token env v2 in
          let v3 =
            (match v3 with
            | `Decl x -> map_declaration env x
            | `Defa_choice_decl (v1, v2) ->
                let v1 = (* "default" *) token env v1 in
                let v2 =
                  (match v2 with
                  | `Decl x -> map_declaration env x
                  | `Exp_choice_auto_semi (v1, v2) ->
                      let v1 = map_expression env v1 in
                      let v2 = map_semicolon env v2 in
                      todo env (v1, v2)
                  )
                in
                todo env (v1, v2)
            )
          in
          todo env (v1, v2, v3)
      )
  | `Export_type_export_clause_opt_from_clause_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_export_clause env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_from_clause env x
        | None -> todo env ())
      in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Export_EQ_exp_choice_auto_semi (v1, v2, v3, v4) ->
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = (* "namespace" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `As_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (match v3 with
        | `Type x -> map_type_ env x
        | `Temp_lit_type x -> map_template_literal_type env x
        )
      in
      todo env (v1, v2, v3)
  | `Inte_module x -> map_internal_module env x
  | `Prim_exp x -> map_primary_expression env x
  | `Choice_jsx_elem x -> map_jsx_element_ env x
  | `Jsx_frag x -> map_jsx_fragment env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Paren_exp x -> map_parenthesized_expression env x
        | `Choice_choice_member_exp x -> map_lhs_expression env x
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 = map_augmented_assignment_lhs env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        | `GTGTGTEQ tok -> (* ">>>=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
        | `STARSTAREQ tok -> (* "**=" *) token env tok
        | `AMPAMPEQ tok -> (* "&&=" *) token env tok
        | `BARBAREQ tok -> (* "||=" *) token env tok
        | `QMARKQMARKEQ tok -> (* "??=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `BANG tok -> (* "!" *) token env tok
        | `TILDE tok -> (* "~" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `PLUS tok -> (* "+" *) token env tok
        | `Typeof tok -> (* "typeof" *) token env tok
        | `Void tok -> (* "void" *) token env tok
        | `Delete tok -> (* "delete" *) token env tok
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Bin_exp x -> map_binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* ternary_qmark *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Update_exp x -> map_update_expression env x
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = map_primary_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Yield_exp (v1, v2) ->
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | `STAR_exp (v1, v2) ->
            let v1 = (* "*" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | `Opt_exp opt ->
            (match opt with
            | Some x -> map_expression env x
            | None -> todo env ())
        )
      in
      todo env (v1, v2)
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expressions env v1 in
  let v2 = map_semicolon env v2 in
  todo env (v1, v2)

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> map_expression env x
  | `Seq_exp x -> map_sequence_expression env x
  )

and map_extends_clause (env : env) ((v1, v2, v3, v4) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v4 =
    List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
    ) v4
  in
  todo env (v1, v2, v3, v4)

and map_extends_type_clause (env : env) ((v1, v2, v3) : CST.extends_type_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_anon_choice_type_id_a85f573 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_type_id_a85f573 env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_statement_block env v2 in
  todo env (v1, v2)

and map_for_header (env : env) ((v1, v2, v3, v4, v5) : CST.for_header) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Choice_choice_choice_member_exp x ->
        (match x with
        | `Choice_choice_member_exp x -> map_lhs_expression env x
        | `Paren_exp x -> map_parenthesized_expression env x
        )
    | `Var_choice_id_opt_init (v1, v2, v3) ->
        let v1 = (* "var" *) token env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some x -> map_initializer_ env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | `Choice_let_choice_id (v1, v2) ->
        let v1 = map_anon_choice_let_ca16eb3 env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        todo env (v1, v2)
    )
  in
  let v3 =
    (match v3 with
    | `In tok -> (* "in" *) token env tok
    | `Of tok -> (* "of" *) token env tok
    )
  in
  let v4 = map_expressions env v4 in
  let v5 = (* ")" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Requ_param (v1, v2, v3) ->
      let v1 = map_parameter_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Opt_param (v1, v2, v3, v4) ->
      let v1 = map_parameter_name env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  )

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_formal_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_formal_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_function_ (env : env) ((v1, v2, v3, v4, v5) : CST.function_) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> (* identifier *) token env tok
    | None -> todo env ())
  in
  let v4 = map_call_signature_ env v4 in
  let v5 = map_statement_block env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 = map_call_signature_ env v4 in
  let v5 = map_statement_block env v5 in
  let v6 =
    (match v6 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_generator_function (env : env) ((v1, v2, v3, v4, v5, v6) : CST.generator_function) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* "*" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> (* identifier *) token env tok
    | None -> todo env ())
  in
  let v5 = map_call_signature_ env v5 in
  let v6 = map_statement_block env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_generator_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* "*" *) token env v3 in
  let v4 = (* identifier *) token env v4 in
  let v5 = map_call_signature_ env v5 in
  let v6 = map_statement_block env v6 in
  let v7 =
    (match v7 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> (* identifier *) token env tok
    | `Nested_type_id x -> map_nested_type_identifier env x
    )
  in
  let v2 = map_type_arguments env v2 in
  todo env (v1, v2)

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> (* "-" *) token env tok
          | None -> todo env ())
        in
        let v2 = (* "readonly" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    (match v3 with
    | `Choice_id_COLON_type (v1, v2, v3) ->
        let v1 = map_anon_choice_type_id_dd17e7d env v1 in
        let v2 = (* ":" *) token env v2 in
        let v3 = map_type_ env v3 in
        todo env (v1, v2, v3)
    | `Mapped_type_clause x -> map_mapped_type_clause env x
    )
  in
  let v4 = (* "]" *) token env v4 in
  let v5 =
    (match v5 with
    | `Type_anno x -> map_type_annotation env x
    | `Omit_type_anno x -> map_omitting_type_annotation env x
    | `Opting_type_anno x -> map_opting_type_annotation env x
    )
  in
  todo env (v1, v2, v3, v4, v5)

and map_initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let v1 = (* "namespace" *) token env v1 in
  let v2 = map_module__ env v2 in
  todo env (v1, v2)

and map_jsx_attribute_ (env : env) (x : CST.jsx_attribute_) =
  (match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = map_jsx_attribute_name env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_jsx_attribute_value env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Jsx_exp x -> map_jsx_expression env x
  )

and map_jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  (match x with
  | `Str x -> map_string_ env x
  | `Jsx_exp x -> map_jsx_expression env x
  | `Choice_jsx_elem x -> map_jsx_element_ env x
  | `Jsx_frag x -> map_jsx_fragment env x
  )

and map_jsx_child (env : env) (x : CST.jsx_child) =
  (match x with
  | `Jsx_text tok -> (* pattern [^{}<>]+ *) token env tok
  | `Choice_jsx_elem x -> map_jsx_element_ env x
  | `Jsx_frag x -> map_jsx_fragment env x
  | `Jsx_exp x -> map_jsx_expression env x
  )

and map_jsx_element_ (env : env) (x : CST.jsx_element_) =
  (match x with
  | `Jsx_elem (v1, v2, v3) ->
      let v1 = map_jsx_opening_element env v1 in
      let v2 = List.map (map_jsx_child env) v2 in
      let v3 = map_jsx_closing_element env v3 in
      todo env (v1, v2, v3)
  | `Jsx_self_clos_elem (v1, v2, v3, v4, v5) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = map_anon_choice_jsx_attr_name_b052322 env v2 in
      let v3 = List.map (map_jsx_attribute_ env) v3 in
      let v4 = (* "/" *) token env v4 in
      let v5 = (* ">" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Exp x -> map_expression env x
        | `Seq_exp x -> map_sequence_expression env x
        | `Spread_elem x -> map_spread_element env x
        )
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_jsx_fragment (env : env) ((v1, v2, v3, v4, v5, v6) : CST.jsx_fragment) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* ">" *) token env v2 in
  let v3 = List.map (map_jsx_child env) v3 in
  let v4 = (* "<" *) token env v4 in
  let v5 = (* "/" *) token env v5 in
  let v6 = (* ">" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_anon_choice_jsx_attr_name_b052322 env v2 in
  let v3 = List.map (map_jsx_attribute_ env) v3 in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration) =
  let v1 = map_anon_choice_let_ca16eb3 env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = map_semicolon env v4 in
  todo env (v1, v2, v3, v4)

and map_lhs_expression (env : env) (x : CST.lhs_expression) =
  (match x with
  | `Choice_member_exp x ->
      (match x with
      | `Member_exp x -> map_member_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Choice_unde x -> map_identifier_ env x
      | `Choice_decl x -> map_reserved_identifier env x
      | `Dest_pat x -> map_destructuring_pattern env x
      )
  | `Non_null_exp x -> map_non_null_expression env x
  )

and map_mapped_type_clause (env : env) ((v1, v2, v3, v4) : CST.mapped_type_clause) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "in" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 = map_anon_choice_exp_9cd0ed5 env v1 in
  let v2 = map_anon_choice_DOT_d88d0af env v2 in
  let v3 = map_anon_choice_priv_prop_id_89abb74 env v3 in
  todo env (v1, v2, v3)

and map_method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) : CST.method_definition) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> (* "static" *) token env tok
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "override" *) token env tok
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "readonly" *) token env tok
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v7 = map_property_name env v7 in
  let v8 =
    (match v8 with
    | Some tok -> (* "?" *) token env tok
    | None -> todo env ())
  in
  let v9 = map_call_signature_ env v9 in
  let v10 = map_statement_block env v10 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

and map_method_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.method_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> (* "static" *) token env tok
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "override" *) token env tok
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "readonly" *) token env tok
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v7 = map_property_name env v7 in
  let v8 =
    (match v8 with
    | Some tok -> (* "?" *) token env tok
    | None -> todo env ())
  in
  let v9 = map_call_signature_ env v9 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)

and map_module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 =
    (match v1 with
    | `Str x -> map_string_ env x
    | `Id tok -> (* identifier *) token env tok
    | `Nested_id x -> map_nested_identifier env x
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_statement_block env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_non_null_expression (env : env) ((v1, v2) : CST.non_null_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "!" *) token env v2 in
  todo env (v1, v2)

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_anon_choice_pair_20c9acd env x
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_anon_choice_pair_20c9acd env x
              | None -> todo env ())
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    (match v1 with
    | `LCURL tok -> (* "{" *) token env tok
    | `LCURLBAR tok -> (* "{|" *) token env tok
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `COMMA tok -> (* "," *) token env tok
              | `SEMI tok -> (* ";" *) token env tok
              )
          | None -> todo env ())
        in
        let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_COMMA_5194cb4 env v1 in
            let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 =
          (match v4 with
          | Some x -> map_anon_choice_COMMA_5194cb4 env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `RCURL tok -> (* "}" *) token env tok
    | `BARRCURL tok -> (* "|}" *) token env tok
    )
  in
  todo env (v1, v2, v3)

and map_omitting_type_annotation (env : env) ((v1, v2) : CST.omitting_type_annotation) =
  let v1 = (* "-?:" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_opting_type_annotation (env : env) ((v1, v2) : CST.opting_type_annotation) =
  let v1 = (* "?:" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_parameter_name (env : env) ((v1, v2, v3, v4, v5) : CST.parameter_name) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 =
    (match v2 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "override" *) token env tok
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "readonly" *) token env tok
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | `Pat x -> map_pattern env x
    | `This tok -> (* "this" *) token env tok
    )
  in
  todo env (v1, v2, v3, v4, v5)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Exp_opt_type_anno (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_type_annotation env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Choice_choice_member_exp x -> map_lhs_expression env x
  | `Rest_pat x -> map_rest_pattern env x
  )

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_subs_exp x ->
      (match x with
      | `Subs_exp x -> map_subscript_expression env x
      | `Member_exp x -> map_member_expression env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Choice_unde x -> map_identifier_ env x
      | `Choice_decl x -> map_reserved_identifier env x
      | `This tok -> (* "this" *) token env tok
      | `Super tok -> (* "super" *) token env tok
      | `Num tok -> (* number *) token env tok
      | `Str x -> map_string_ env x
      | `Temp_str x -> map_template_string env x
      | `Regex x -> map_regex env x
      | `True tok -> (* "true" *) token env tok
      | `False tok -> (* "false" *) token env tok
      | `Null tok -> (* "null" *) token env tok
      | `Import tok -> (* import *) token env tok
      | `Obj x -> map_object_ env x
      | `Array x -> map_array_ env x
      | `Func x -> map_function_ env x
      | `Arrow_func x -> map_arrow_function env x
      | `Gene_func x -> map_generator_function env x
      | `Class x -> map_class_ env x
      | `Meta_prop x -> map_meta_property env x
      | `Call_exp x -> map_call_expression env x
      )
  | `Non_null_exp x -> map_non_null_expression env x
  )

and map_primary_type (env : env) (x : CST.primary_type) =
  (match x with
  | `Paren_type (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Pred_type x -> map_predefined_type env x
  | `Id tok -> (* identifier *) token env tok
  | `Nested_type_id x -> map_nested_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  | `Obj_type x -> map_object_type env x
  | `Array_type (v1, v2, v3) ->
      let v1 = map_primary_type env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Tuple_type (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_tuple_type_member env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_tuple_type_member env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Flow_maybe_type (v1, v2) ->
      let v1 = (* "?" *) token env v1 in
      let v2 = map_primary_type env v2 in
      todo env (v1, v2)
  | `Type_query (v1, v2) ->
      let v1 = (* "typeof" *) token env v1 in
      let v2 =
        (match v2 with
        | `Type_query_subs_exp x ->
            map_type_query_subscript_expression env x
        | `Type_query_member_exp x ->
            map_type_query_member_expression env x
        | `Type_query_call_exp x ->
            map_type_query_call_expression env x
        | `Id tok -> (* identifier *) token env tok
        )
      in
      todo env (v1, v2)
  | `Index_type_query (v1, v2) ->
      let v1 = (* "keyof" *) token env v1 in
      let v2 = map_primary_type env v2 in
      todo env (v1, v2)
  | `This tok -> (* "this" *) token env tok
  | `Exis_type tok -> (* "*" *) token env tok
  | `Lit_type x -> map_literal_type env x
  | `Lookup_type (v1, v2, v3, v4) ->
      let v1 = map_primary_type env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Cond_type (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "extends" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "?" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_type_ env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Temp_lit_type x -> map_template_literal_type env x
  | `Inte_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Union_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  )

and map_property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Choice_id x -> map_anon_choice_type_id_dd17e7d env x
  | `Priv_prop_id tok ->
      (* private_property_identifier *) token env tok
  | `Str x -> map_string_ env x
  | `Num tok -> (* number *) token env tok
  | `Comp_prop_name (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_public_field_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.public_field_definition) =
  let v1 =
    (match v1 with
    | Some tok -> (* "declare" *) token env tok
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Opt_static_opt_over_modi_opt_read (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some tok -> (* "static" *) token env tok
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> (* "override" *) token env tok
          | None -> todo env ())
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "readonly" *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | `Opt_abst_opt_read (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> (* "abstract" *) token env tok
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> (* "readonly" *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_read_opt_abst (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> (* "readonly" *) token env tok
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> (* "abstract" *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v4 = map_property_name env v4 in
  let v5 =
    (match v5 with
    | Some x ->
        (match x with
        | `QMARK tok -> (* "?" *) token env tok
        | `BANG tok -> (* "!" *) token env tok
        )
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_type_annotation env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some x -> map_initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_lhs_expression env v2 in
  todo env (v1, v2)

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    (match v3 with
    | `Seq_exp x -> map_sequence_expression env x
    | `Exp x -> map_expression env x
    )
  in
  todo env (v1, v2, v3)

and map_spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Export_stmt x -> map_export_statement env x
  | `Import_stmt (v1, v2, v3, v4) ->
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_type_2b11f6b env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Import_clause_from_clause (v1, v2) ->
            let v1 = map_import_clause env v1 in
            let v2 = map_from_clause env v2 in
            todo env (v1, v2)
        | `Import_requ_clause x -> map_import_require_clause env x
        | `Str x -> map_string_ env x
        )
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Debu_stmt (v1, v2) ->
      let v1 = (* "debugger" *) token env v1 in
      let v2 = map_semicolon env v2 in
      todo env (v1, v2)
  | `Exp_stmt x -> map_expression_statement env x
  | `Decl x -> map_declaration env x
  | `Stmt_blk x -> map_statement_block env x
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_else_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_body env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Lexi_decl x -> map_lexical_declaration env x
        | `Var_decl x -> map_variable_declaration env x
        | `Exp_stmt x -> map_expression_statement env x
        | `Empty_stmt tok -> (* ";" *) token env tok
        )
      in
      let v4 =
        (match v4 with
        | `Exp_stmt x -> map_expression_statement env x
        | `Empty_stmt tok -> (* ";" *) token env tok
        )
      in
      let v5 =
        (match v5 with
        | Some x -> map_expressions env x
        | None -> todo env ())
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_statement env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "await" *) token env tok
        | None -> todo env ())
      in
      let v3 = map_for_header env v3 in
      let v4 = map_statement env v4 in
      todo env (v1, v2, v3, v4)
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_statement_block env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_catch_clause env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_finally_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `With_stmt (v1, v2, v3) ->
      let v1 = (* "with" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* identifier *) token env tok
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* identifier *) token env tok
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_expressions env x
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> (* ";" *) token env tok
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = map_anon_choice_type_id_dd17e7d env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  )

and map_statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_expression) =
  let v1 = map_anon_choice_exp_9cd0ed5 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> (* "?." *) token env tok
    | None -> todo env ())
  in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_expressions env v4 in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Switch_case x -> map_switch_case env x
      | `Switch_defa x -> map_switch_default env x
      )
    ) v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = List.map (map_statement env) v4 in
  todo env (v1, v2, v3, v4)

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = List.map (map_statement env) v3 in
  todo env (v1, v2, v3)

and map_template_literal_type (env : env) ((v1, v2, v3) : CST.template_literal_type) =
  let v1 = (* "`" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Temp_chars tok -> (* template_chars *) token env tok
      | `Temp_type x -> map_template_type env x
      )
    ) v2
  in
  let v3 = (* "`" *) token env v3 in
  todo env (v1, v2, v3)

and map_template_string (env : env) ((v1, v2, v3) : CST.template_string) =
  let v1 = (* "`" *) token env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Temp_chars tok -> (* template_chars *) token env tok
      | `Esc_seq tok -> (* escape_sequence *) token env tok
      | `Temp_subs x -> map_template_substitution env x
      )
    ) v2
  in
  let v3 = (* "`" *) token env v3 in
  todo env (v1, v2, v3)

and map_template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution) =
  let v1 = (* "${" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_template_type (env : env) ((v1, v2, v3) : CST.template_type) =
  let v1 = (* "${" *) token env v1 in
  let v2 =
    (match v2 with
    | `Prim_type x -> map_primary_type env x
    | `Infer_type x -> map_infer_type env x
    )
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_tuple_type_member (env : env) (x : CST.tuple_type_member) =
  (match x with
  | `Tuple_param (v1, v2) ->
      let v1 =
        (match v1 with
        | `Id tok -> (* identifier *) token env tok
        | `Rest_pat x -> map_rest_pattern env x
        )
      in
      let v2 = map_type_annotation env v2 in
      todo env (v1, v2)
  | `Opt_tuple_param (v1, v2, v3) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_type_annotation env v3 in
      todo env (v1, v2, v3)
  | `Opt_type (v1, v2) ->
      let v1 = map_type_ env v1 in
      let v2 = (* "?" *) token env v2 in
      todo env (v1, v2)
  | `Rest_type (v1, v2) ->
      let v1 = (* "..." *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Type x -> map_type_ env x
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Prim_type x -> map_primary_type env x
  | `Func_type (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v2 = map_formal_parameters env v2 in
      let v3 = (* "=>" *) token env v3 in
      let v4 =
        (match v4 with
        | `Type x -> map_type_ env x
        | `Type_pred x -> map_type_predicate env x
        )
      in
      todo env (v1, v2, v3, v4)
  | `Read_type (v1, v2) ->
      let v1 = (* "readonly" *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `Cons_type (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "abstract" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "new" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = map_formal_parameters env v4 in
      let v5 = (* "=>" *) token env v5 in
      let v6 = map_type_ env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Infer_type x -> map_infer_type env x
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_constraint_ env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_default_type env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 =
    (match v1 with
    | `Id tok -> (* identifier *) token env tok
    | `This tok -> (* "this" *) token env tok
    )
  in
  let v2 = (* "is" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_type_predicate_annotation (env : env) ((v1, v2) : CST.type_predicate_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_predicate env v2 in
  todo env (v1, v2)

and map_type_query_call_expression (env : env) ((v1, v2) : CST.type_query_call_expression) =
  let v1 =
    (match v1 with
    | `Import tok -> (* import *) token env tok
    | `Id tok -> (* identifier *) token env tok
    | `Type_query_member_exp x ->
        map_type_query_member_expression env x
    | `Type_query_subs_exp x ->
        map_type_query_subscript_expression env x
    )
  in
  let v2 = map_arguments env v2 in
  todo env (v1, v2)

and map_type_query_member_expression (env : env) ((v1, v2, v3) : CST.type_query_member_expression) =
  let v1 = map_anon_choice_type_id_e96bf13 env v1 in
  let v2 = map_anon_choice_DOT_d88d0af env v2 in
  let v3 = map_anon_choice_priv_prop_id_89abb74 env v3 in
  todo env (v1, v2, v3)

and map_type_query_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.type_query_subscript_expression) =
  let v1 = map_anon_choice_type_id_e96bf13 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> (* "?." *) token env tok
    | None -> todo env ())
  in
  let v3 = (* "[" *) token env v3 in
  let v4 =
    (match v4 with
    | `Pred_type x -> map_predefined_type env x
    | `Str x -> map_string_ env x
    | `Num tok -> (* number *) token env tok
    )
  in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      todo env (v1, v2)
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.variable_declaration) =
  let v1 = (* "var" *) token env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = map_semicolon env v4 in
  todo env (v1, v2, v3, v4)

and map_variable_declarator (env : env) (x : CST.variable_declarator) =
  (match x with
  | `Choice_id_opt_type_anno_opt_init (v1, v2, v3) ->
      let v1 = map_anon_choice_type_id_940079a env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Id_BANG_type_anno (v1, v2, v3) ->
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "!" *) token env v2 in
      let v3 = map_type_annotation env v3 in
      todo env (v1, v2, v3)
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some tok -> (* pattern #!.* *) token env tok
    | None -> todo env ())
  in
  let v2 = List.map (map_statement env) v2 in
  todo env (v1, v2)
