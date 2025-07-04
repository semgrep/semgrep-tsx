(**
   Boilerplate to be used as a template when mapping the tsx CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_jsx_text (env : env) (tok : CST.jsx_text) =
  (* jsx_text *) token env tok

let map_template_chars (env : env) (tok : CST.template_chars) =
  (* template_chars *) token env tok

let map_meta_property (env : env) (x : CST.meta_property) =
  (match x with
  | `New_DOT_target (v1, v2, v3) -> R.Case ("New_DOT_target",
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "target" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Import_DOT_meta (v1, v2, v3) -> R.Case ("Import_DOT_meta",
      let v1 = (* "import" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "meta" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  )

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  (* pattern #!.* *) token env tok

let map_unescaped_single_jsx_string_fragment (env : env) (tok : CST.unescaped_single_jsx_string_fragment) =
  (* pattern "([^'&]|&[^#A-Za-z])+" *) token env tok

let map_html_character_reference (env : env) (tok : CST.html_character_reference) =
  (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *) token env tok

let map_regex_pattern (env : env) (tok : CST.regex_pattern) =
  (* regex_pattern *) token env tok

let map_jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok

let map_accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  )

let map_ternary_qmark (env : env) (tok : CST.ternary_qmark) =
  (* ternary_qmark *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Decl tok -> R.Case ("Decl",
      (* "declare" *) token env tok
    )
  | `Name tok -> R.Case ("Name",
      (* "namespace" *) token env tok
    )
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  | `Over tok -> R.Case ("Over",
      (* "override" *) token env tok
    )
  | `Read tok -> R.Case ("Read",
      (* "readonly" *) token env tok
    )
  | `Module tok -> R.Case ("Module",
      (* "module" *) token env tok
    )
  | `Any tok -> R.Case ("Any",
      (* "any" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* "number" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "boolean" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Symb tok -> R.Case ("Symb",
      (* "symbol" *) token env tok
    )
  | `Export tok -> R.Case ("Export",
      (* "export" *) token env tok
    )
  | `Obj tok -> R.Case ("Obj",
      (* "object" *) token env tok
    )
  | `New tok -> R.Case ("New",
      (* "new" *) token env tok
    )
  | `Choice_get x -> R.Case ("Choice_get",
      (match x with
      | `Get tok -> R.Case ("Get",
          (* "get" *) token env tok
        )
      | `Set tok -> R.Case ("Set",
          (* "set" *) token env tok
        )
      | `Async tok -> R.Case ("Async",
          (* "async" *) token env tok
        )
      | `Static tok -> R.Case ("Static",
          (* "static" *) token env tok
        )
      | `Export tok -> R.Case ("Export",
          (* "export" *) token env tok
        )
      | `Let tok -> R.Case ("Let",
          (* "let" *) token env tok
        )
      )
    )
  )

let map_anon_choice_DASH_81d4819 (env : env) (x : CST.anon_choice_DASH_81d4819) =
  (match x with
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_import (env : env) (tok : CST.import) =
  (* import *) token env tok

let map_regex_flags (env : env) (tok : CST.regex_flags) =
  (* pattern [a-z]+ *) token env tok

let map_anon_choice_type_2b11f6b (env : env) (x : CST.anon_choice_type_2b11f6b) =
  (match x with
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  | `Typeof tok -> R.Case ("Typeof",
      (* "typeof" *) token env tok
    )
  )

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_unescaped_single_string_fragment (env : env) (tok : CST.unescaped_single_string_fragment) =
  (* pattern "[^'\\\\\\r\\n]+" *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_anon_choice_let_ca16eb3 (env : env) (x : CST.anon_choice_let_ca16eb3) =
  (match x with
  | `Let tok -> R.Case ("Let",
      (* "let" *) token env tok
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  )

let map_unescaped_double_jsx_string_fragment (env : env) (tok : CST.unescaped_double_jsx_string_fragment) =
  (* pattern "([^\"&]|&[^#A-Za-z])+" *) token env tok

let map_semgrep_metavar_ellipsis (env : env) (tok : CST.semgrep_metavar_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_private_property_identifier (env : env) (tok : CST.private_property_identifier) =
  (* private_property_identifier *) token env tok

let map_anon_choice_get_8fb02de (env : env) (x : CST.anon_choice_get_8fb02de) =
  (match x with
  | `Get tok -> R.Case ("Get",
      (* "get" *) token env tok
    )
  | `Set tok -> R.Case ("Set",
      (* "set" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_predefined_type (env : env) (x : CST.predefined_type) =
  (match x with
  | `Any tok -> R.Case ("Any",
      (* "any" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* "number" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "boolean" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Symb tok -> R.Case ("Symb",
      (* "symbol" *) token env tok
    )
  | `Unique_symb (v1, v2) -> R.Case ("Unique_symb",
      let v1 = (* "unique" *) token env v1 in
      let v2 = (* "symbol" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Void tok -> R.Case ("Void",
      (* "void" *) token env tok
    )
  | `Unkn tok -> R.Case ("Unkn",
      (* "unknown" *) token env tok
    )
  | `Never tok -> R.Case ("Never",
      (* "never" *) token env tok
    )
  | `Obj tok -> R.Case ("Obj",
      (* "object" *) token env tok
    )
  )

let map_unescaped_double_string_fragment (env : env) (tok : CST.unescaped_double_string_fragment) =
  (* pattern "[^\"\\\\\\r\\n]+" *) token env tok

let map_imm_tok_prec_p1_slash (env : env) (tok : CST.imm_tok_prec_p1_slash) =
  (* "/" *) token env tok

let map_function_signature_automatic_semicolon (env : env) (tok : CST.function_signature_automatic_semicolon) =
  (* function_signature_automatic_semicolon *) token env tok

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> R.Case ("Auto_semi",
      (* automatic_semicolon *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_jsx_string (env : env) (x : CST.jsx_string) =
  (match x with
  | `DQUOT_rep_choice_unes_double_jsx_str_frag_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_unes_double_jsx_str_frag_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_double_jsx_str_frag tok -> R.Case ("Unes_double_jsx_str_frag",
              (* pattern "([^\"&]|&[^#A-Za-z])+" *) token env tok
            )
          | `Html_char_ref tok -> R.Case ("Html_char_ref",
              (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_rep_choice_unes_single_jsx_str_frag_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_unes_single_jsx_str_frag_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_single_jsx_str_frag tok -> R.Case ("Unes_single_jsx_str_frag",
              (* pattern "([^'&]|&[^#A-Za-z])+" *) token env tok
            )
          | `Html_char_ref tok -> R.Case ("Html_char_ref",
              (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_rep_choice_unes_double_str_frag_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_double_str_frag tok -> R.Case ("Unes_double_str_frag",
              (* pattern "[^\"\\\\\\r\\n]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_rep_choice_unes_single_str_frag_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Unes_single_str_frag tok -> R.Case ("Unes_single_str_frag",
              (* pattern "[^'\\\\\\r\\n]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_regex (env : env) ((v1, v2, v3, v4) : CST.regex) =
  let v1 = (* "/" *) token env v1 in
  let v2 = (* regex_pattern *) token env v2 in
  let v3 = map_imm_tok_prec_p1_slash env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* pattern [a-z]+ *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_debugger_statement (env : env) ((v1, v2) : CST.debugger_statement) =
  let v1 = (* "debugger" *) token env v1 in
  let v2 = map_semicolon env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  (match x with
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
      map_semicolon env x
    )
  )

let map_break_statement (env : env) ((v1, v2, v3) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_semicolon env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_type_id_dd17e7d (env : env) (x : CST.anon_choice_type_id_dd17e7d) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Choice_decl x -> R.Case ("Choice_decl",
      map_reserved_identifier env x
    )
  )

let map_anon_choice_rese_id_515394d (env : env) (x : CST.anon_choice_rese_id_515394d) =
  (match x with
  | `Choice_decl x -> R.Case ("Choice_decl",
      map_reserved_identifier env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import) =
  let v1 = (* "*" *) token env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_anon_choice_type_id_b8f8ced (env : env) (x : CST.anon_choice_type_id_b8f8ced) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Deco_member_exp x -> R.Case ("Deco_member_exp",
      map_decorator_member_expression env x
    )
  )

and map_decorator_member_expression (env : env) ((v1, v2, v3) : CST.decorator_member_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_semicolon env v3 in
  R.Tuple [v1; v2; v3]

let map_identifier_ (env : env) (x : CST.identifier_) =
  (match x with
  | `Unde tok -> R.Case ("Unde",
      (* "undefined" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let rec map_anon_choice_type_id_42c0412 (env : env) (x : CST.anon_choice_type_id_42c0412) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Nested_id x -> R.Case ("Nested_id",
      map_nested_identifier env x
    )
  )

and map_nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  (match x with
  | `Jsx_id tok -> R.Case ("Jsx_id",
      (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_import_identifier (env : env) (x : CST.import_identifier) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  )

let map_anon_choice_priv_prop_id_89abb74 (env : env) (x : CST.anon_choice_priv_prop_id_89abb74) =
  (match x with
  | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
      (* private_property_identifier *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_module_export_name (env : env) (x : CST.module_export_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

let map_literal_type (env : env) (x : CST.literal_type) =
  (match x with
  | `Num_ (v1, v2) -> R.Case ("Num_",
      let v1 = map_anon_choice_DASH_81d4819 env v1 in
      let v2 = (* number *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "null" *) token env tok
    )
  | `Unde tok -> R.Case ("Unde",
      (* "undefined" *) token env tok
    )
  )

let map_import_require_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = (* "require" *) token env v3 in
  let v4 = (* "(" *) token env v4 in
  let v5 = map_string_ env v5 in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 =
    (match v2 with
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Semg_meta tok -> R.Case ("Semg_meta",
        (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_nested_type_identifier (env : env) ((v1, v2, v3) : CST.nested_type_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = map_jsx_identifier_ env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_jsx_identifier_ env v3 in
  R.Tuple [v1; v2; v3]

let map_namespace_export (env : env) ((v1, v2, v3) : CST.namespace_export) =
  let v1 = (* "*" *) token env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_module_export_name env v3 in
  R.Tuple [v1; v2; v3]

let map_export_specifier (env : env) ((v1, v2, v3) : CST.export_specifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_type_2b11f6b env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_module_export_name env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_module_export_name env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_import_specifier (env : env) (x : CST.import_specifier) =
  (match x with
  | `Opt_choice_type_choice_import_id (v1, v2) -> R.Case ("Opt_choice_type_choice_import_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_type_2b11f6b env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Import_id x -> R.Case ("Import_id",
            map_import_identifier env x
          )
        | `Choice_module_export_name_as_import_id (v1, v2, v3) -> R.Case ("Choice_module_export_name_as_import_id",
            let v1 =
              (match v1 with
              | `Module_export_name x -> R.Case ("Module_export_name",
                  map_module_export_name env x
                )
              | `Type tok -> R.Case ("Type",
                  (* "type" *) token env tok
                )
              )
            in
            let v2 = (* "as" *) token env v2 in
            let v3 = map_import_identifier env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

let map_jsx_element_name (env : env) (x : CST.jsx_element_name) =
  (match x with
  | `Choice_jsx_id x -> R.Case ("Choice_jsx_id",
      map_jsx_identifier_ env x
    )
  | `Nested_id x -> R.Case ("Nested_id",
      map_nested_identifier env x
    )
  | `Jsx_name_name x -> R.Case ("Jsx_name_name",
      map_jsx_namespace_name env x
    )
  )

let map_jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  (match x with
  | `Choice_jsx_id x -> R.Case ("Choice_jsx_id",
      map_jsx_identifier_ env x
    )
  | `Jsx_name_name x -> R.Case ("Jsx_name_name",
      map_jsx_namespace_name env x
    )
  )

let map_export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_export_specifier env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_export_specifier env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_import_specifier env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_import_specifier env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_jsx_closing_element (env : env) ((v1, v2, v3) : CST.jsx_closing_element) =
  let v1 = (* "</" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_jsx_element_name env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Name_import x -> R.Case ("Name_import",
      map_namespace_import env x
    )
  | `Named_imports x -> R.Case ("Named_imports",
      map_named_imports env x
    )
  | `Import_id_opt_COMMA_choice_name_import (v1, v2) -> R.Case ("Import_id_opt_COMMA_choice_name_import",
      let v1 = map_import_identifier env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | `Name_import x -> R.Case ("Name_import",
                  map_namespace_import env x
                )
              | `Named_imports x -> R.Case ("Named_imports",
                  map_named_imports env x
                )
              )
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let rec map_abstract_method_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.abstract_method_signature) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_accessibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "abstract" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "override" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_get_8fb02de env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_property_name env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v7 = map_call_signature_ env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_adding_type_annotation (env : env) ((v1, v2) : CST.adding_type_annotation) =
  let v1 = (* "+?:" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Spread_elem x -> R.Case ("Spread_elem",
      map_spread_element env x
    )
  )

and map_anon_choice_export_stmt_f90d83f (env : env) (x : CST.anon_choice_export_stmt_f90d83f) =
  (match x with
  | `Export_stmt x -> R.Case ("Export_stmt",
      map_export_statement env x
    )
  | `Prop_sign (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Prop_sign",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_accessibility_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "static" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "override" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "readonly" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_property_name env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "?" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Call_sign_ x -> R.Case ("Call_sign_",
      map_call_signature_ env x
    )
  | `Cons_sign (v1, v2, v3, v4, v5) -> R.Case ("Cons_sign",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "abstract" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "new" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_formal_parameters env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Index_sign x -> R.Case ("Index_sign",
      map_index_signature env x
    )
  | `Meth_sign x -> R.Case ("Meth_sign",
      map_method_signature env x
    )
  )

and map_anon_choice_import_c99ceb4 (env : env) (x : CST.anon_choice_import_c99ceb4) =
  (match x with
  | `Import tok -> R.Case ("Import",
      (* import *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Type_query_member_exp x -> R.Case ("Type_query_member_exp",
      map_type_query_member_expression env x
    )
  | `Type_query_subs_exp x -> R.Case ("Type_query_subs_exp",
      map_type_query_subscript_expression env x
    )
  )

and map_anon_choice_jsx_attr_name_b052322 (env : env) (x : CST.anon_choice_jsx_attr_name_b052322) =
  (match x with
  | `Choice_choice_jsx_id x -> R.Case ("Choice_choice_jsx_id",
      map_jsx_attribute_name env x
    )
  | `Choice_id_opt_type_args (v1, v2) -> R.Case ("Choice_id_opt_type_args",
      let v1 = map_anon_choice_type_id_42c0412 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_pair_20c9acd (env : env) (x : CST.anon_choice_pair_20c9acd) =
  (match x with
  | `Pair x -> R.Case ("Pair",
      map_pair env x
    )
  | `Spread_elem x -> R.Case ("Spread_elem",
      map_spread_element env x
    )
  | `Meth_defi x -> R.Case ("Meth_defi",
      map_method_definition env x
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_type_id_dd17e7d env x
    )
  )

and map_anon_choice_pair_pat_3ff9cbe (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) =
  (match x with
  | `Pair_pat x -> R.Case ("Pair_pat",
      map_pair_pattern env x
    )
  | `Rest_pat x -> R.Case ("Rest_pat",
      map_rest_pattern env x
    )
  | `Obj_assign_pat (v1, v2, v3) -> R.Case ("Obj_assign_pat",
      let v1 =
        (match v1 with
        | `Choice_choice_decl x -> R.Case ("Choice_choice_decl",
            map_anon_choice_rese_id_515394d env x
          )
        | `Dest_pat x -> R.Case ("Dest_pat",
            map_destructuring_pattern env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_type_id_dd17e7d env x
    )
  )

and map_anon_choice_pat_3297d92 (env : env) (x : CST.anon_choice_pat_3297d92) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Assign_pat (v1, v2, v3) -> R.Case ("Assign_pat",
      let v1 = map_pattern env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_prop_name_6cc9e4b (env : env) (x : CST.anon_choice_prop_name_6cc9e4b) =
  (match x with
  | `Prop_name x -> R.Case ("Prop_name",
      map_property_name env x
    )
  | `Enum_assign (v1, v2) -> R.Case ("Enum_assign",
      let v1 = map_property_name env v1 in
      let v2 = map_initializer_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_type_id_43e1312 (env : env) (x : CST.anon_choice_type_id_43e1312) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `Type_query_subs_exp x -> R.Case ("Type_query_subs_exp",
      map_type_query_subscript_expression env x
    )
  | `Type_query_member_exp x -> R.Case ("Type_query_member_exp",
      map_type_query_member_expression env x
    )
  | `Type_query_call_exp x -> R.Case ("Type_query_call_exp",
      map_type_query_call_expression env x
    )
  )

and map_anon_choice_type_id_940079a (env : env) (x : CST.anon_choice_type_id_940079a) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Dest_pat x -> R.Case ("Dest_pat",
      map_destructuring_pattern env x
    )
  )

and map_anon_choice_type_id_a85f573 (env : env) (x : CST.anon_choice_type_id_a85f573) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Nested_type_id x -> R.Case ("Nested_type_id",
      map_nested_type_identifier env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  )

and map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 (env : env) (opt : CST.anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 = map_anon_choice_jsx_attr_name_b052322 env v1 in
      let v2 = R.List (List.map (map_jsx_attribute_ env) v2) in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env) (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_9818c1b env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env) (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_anon_choice_exp_9818c1b env x
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_arrow_function (env : env) ((v1, v2, v3, v4) : CST.arrow_function) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_choice_decl x -> R.Case ("Choice_choice_decl",
        map_anon_choice_rese_id_515394d env x
      )
    | `Call_sign x -> R.Case ("Call_sign",
        map_call_signature_ env x
      )
    )
  in
  let v3 = (* "=>" *) token env v3 in
  let v4 =
    (match v4 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Stmt_blk x -> R.Case ("Stmt_blk",
        map_statement_block env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_asserts (env : env) ((v1, v2) : CST.asserts) =
  let v1 = (* "asserts" *) token env v1 in
  let v2 =
    (match v2 with
    | `Type_pred x -> R.Case ("Type_pred",
        map_type_predicate env x
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_asserts_annotation (env : env) ((v1, v2) : CST.asserts_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_asserts env v2 in
  R.Tuple [v1; v2]

and map_augmented_assignment_lhs (env : env) (x : CST.augmented_assignment_lhs) =
  (match x with
  | `Choice_member_exp x -> R.Case ("Choice_member_exp",
      (match x with
      | `Member_exp x -> R.Case ("Member_exp",
          map_member_expression env x
        )
      | `Subs_exp x -> R.Case ("Subs_exp",
          map_subscript_expression env x
        )
      | `Choice_decl x -> R.Case ("Choice_decl",
          map_reserved_identifier env x
        )
      | `Id tok -> R.Case ("Id",
          (* identifier *) token env tok
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      )
    )
  | `Non_null_exp x -> R.Case ("Non_null_exp",
      map_non_null_expression env x
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_inst_exp (v1, v2, v3) -> R.Case ("Exp_inst_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "instanceof" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_in_exp (v1, v2, v3) -> R.Case ("Choice_exp_in_exp",
      let v1 =
        (match v1 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
            (* private_property_identifier *) token env tok
          )
        )
      in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Choice_exp_opt_type_args_args (v1, v2, v3) -> R.Case ("Choice_exp_opt_type_args_args",
      let v1 =
        (match v1 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Import tok -> R.Case ("Import",
            (* import *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_arguments env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_prim_exp_temp_str (v1, v2) -> R.Case ("Choice_prim_exp_temp_str",
      let v1 =
        (match v1 with
        | `Prim_exp x -> R.Case ("Prim_exp",
            map_primary_expression env x
          )
        | `New_exp x -> R.Case ("New_exp",
            map_new_expression env x
          )
        )
      in
      let v2 = map_template_string env v2 in
      R.Tuple [v1; v2]
    )
  | `Prim_exp_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) -> R.Case ("Prim_exp_QMARKDOT_opt_type_args_args",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "?." *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_arguments env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_call_signature (env : env) ((v1, v2, v3) : CST.call_signature) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Type_anno x -> R.Case ("Type_anno",
            map_type_annotation env x
          )
        | `Asserts_anno x -> R.Case ("Asserts_anno",
            map_asserts_annotation env x
          )
        | `Type_pred_anno x -> R.Case ("Type_pred_anno",
            map_type_predicate_annotation env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_call_signature_ (env : env) (x : CST.call_signature_) =
  map_call_signature env x

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_type_annotation env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = map_statement_block env v3 in
  R.Tuple [v1; v2; v3]

and map_class_ (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 = (* "class" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_class_heritage env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_class_body env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Semg_ellips tok -> R.Case ("Semg_ellips",
          (* "..." *) token env tok
        )
      | `Deco x -> R.Case ("Deco",
          map_decorator env x
        )
      | `Meth_defi_opt_choice_auto_semi (v1, v2) -> R.Case ("Meth_defi_opt_choice_auto_semi",
          let v1 = map_method_definition env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_semicolon env x
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        )
      | `Meth_sign_choice_func_sign_auto_semi (v1, v2) -> R.Case ("Meth_sign_choice_func_sign_auto_semi",
          let v1 = map_method_signature env v1 in
          let v2 =
            (match v2 with
            | `Func_sign_auto_semi tok -> R.Case ("Func_sign_auto_semi",
                (* function_signature_automatic_semicolon *) token env tok
              )
            | `COMMA tok -> R.Case ("COMMA",
                (* "," *) token env tok
              )
            )
          in
          R.Tuple [v1; v2]
        )
      | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) -> R.Case ("Choice_abst_meth_sign_choice_choice_auto_semi",
          let v1 =
            (match v1 with
            | `Abst_meth_sign x -> R.Case ("Abst_meth_sign",
                map_abstract_method_signature env x
              )
            | `Index_sign x -> R.Case ("Index_sign",
                map_index_signature env x
              )
            | `Meth_sign x -> R.Case ("Meth_sign",
                map_method_signature env x
              )
            | `Public_field_defi x -> R.Case ("Public_field_defi",
                map_public_field_definition env x
              )
            )
          in
          let v2 =
            (match v2 with
            | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
                map_semicolon env x
              )
            | `COMMA tok -> R.Case ("COMMA",
                (* "," *) token env tok
              )
            )
          in
          R.Tuple [v1; v2]
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 = (* "class" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_class_heritage env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_class_body env v6 in
  let v7 =
    (match v7 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_class_heritage (env : env) (x : CST.class_heritage) =
  (match x with
  | `Extends_clause_opt_imples_clause (v1, v2) -> R.Case ("Extends_clause_opt_imples_clause",
      let v1 = map_extends_clause env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_implements_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Imples_clause x -> R.Case ("Imples_clause",
      map_implements_clause env x
    )
  )

and map_constraint_ (env : env) ((v1, v2) : CST.constraint_) =
  let v1 =
    (match v1 with
    | `Extends tok -> R.Case ("Extends",
        (* "extends" *) token env tok
      )
    | `COLON tok -> R.Case ("COLON",
        (* ":" *) token env tok
      )
    )
  in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Choice_func_decl x -> R.Case ("Choice_func_decl",
      (match x with
      | `Func_decl x -> R.Case ("Func_decl",
          map_function_declaration env x
        )
      | `Gene_func_decl x -> R.Case ("Gene_func_decl",
          map_generator_function_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Lexi_decl x -> R.Case ("Lexi_decl",
          map_lexical_declaration env x
        )
      | `Var_decl x -> R.Case ("Var_decl",
          map_variable_declaration env x
        )
      )
    )
  | `Func_sign (v1, v2, v3, v4, v5) -> R.Case ("Func_sign",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_call_signature_ env v4 in
      let v5 =
        (match v5 with
        | `Choice_auto_semi x -> R.Case ("Choice_auto_semi",
            map_semicolon env x
          )
        | `Func_sign_auto_semi tok -> R.Case ("Func_sign_auto_semi",
            (* function_signature_automatic_semicolon *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Abst_class_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Abst_class_decl",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 = (* "abstract" *) token env v2 in
      let v3 = (* "class" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_class_heritage env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_class_body env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Module (v1, v2) -> R.Case ("Module",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_module__ env v2 in
      R.Tuple [v1; v2]
    )
  | `Inte_module x -> R.Case ("Inte_module",
      map_internal_module env x
    )
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Type_alias_decl",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = map_semicolon env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Enum_decl (v1, v2, v3, v4) -> R.Case ("Enum_decl",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "const" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_enum_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Inte_decl (v1, v2, v3, v4, v5) -> R.Case ("Inte_decl",
      let v1 = (* "interface" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_extends_type_clause env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_object_type env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Import_alias (v1, v2, v3, v4, v5) -> R.Case ("Import_alias",
      let v1 = (* "import" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_anon_choice_type_id_42c0412 env v4 in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Ambi_decl (v1, v2) -> R.Case ("Ambi_decl",
      let v1 = (* "declare" *) token env v1 in
      let v2 =
        (match v2 with
        | `Decl x -> R.Case ("Decl",
            map_declaration env x
          )
        | `Global_stmt_blk (v1, v2) -> R.Case ("Global_stmt_blk",
            let v1 = (* "global" *) token env v1 in
            let v2 = map_statement_block env v2 in
            R.Tuple [v1; v2]
          )
        | `Module_DOT_id_COLON_type_choice_auto_semi (v1, v2, v3, v4, v5, v6) -> R.Case ("Module_DOT_id_COLON_type_choice_auto_semi",
            let v1 = (* "module" *) token env v1 in
            let v2 = (* "." *) token env v2 in
            let v3 = (* identifier *) token env v3 in
            let v4 = (* ":" *) token env v4 in
            let v5 = map_type_ env v5 in
            let v6 = map_semicolon env v6 in
            R.Tuple [v1; v2; v3; v4; v5; v6]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Deco_member_exp x -> R.Case ("Deco_member_exp",
        map_decorator_member_expression env x
      )
    | `Deco_call_exp x -> R.Case ("Deco_call_exp",
        map_decorator_call_expression env x
      )
    | `Deco_paren_exp x -> R.Case ("Deco_paren_exp",
        map_decorator_parenthesized_expression env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_decorator_call_expression (env : env) ((v1, v2, v3) : CST.decorator_call_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_arguments env v3 in
  R.Tuple [v1; v2; v3]

and map_decorator_parenthesized_expression (env : env) ((v1, v2, v3) : CST.decorator_parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Deco_member_exp x -> R.Case ("Deco_member_exp",
        map_decorator_member_expression env x
      )
    | `Deco_call_exp x -> R.Case ("Deco_call_exp",
        map_decorator_call_expression env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_default_type (env : env) ((v1, v2) : CST.default_type) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_destructuring_pattern (env : env) (x : CST.destructuring_pattern) =
  (match x with
  | `Obj_pat (v1, v2, v3) -> R.Case ("Obj_pat",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pair_pat_3ff9cbe env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_pair_pat_3ff9cbe env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Array_pat (v1, v2, v3) -> R.Case ("Array_pat",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pat_3297d92 env x
                ))
              | None -> R.Option None)
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_anon_choice_pat_3297d92 env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = (* "while" *) token env v3 in
  let v4 = map_parenthesized_expression env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_semicolon env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_prop_name_6cc9e4b env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_prop_name_6cc9e4b env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x -> R.Case ("Choice_export_choice_STAR_from_clause_choice_auto_semi",
      (match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2, v3) -> R.Case ("Export_choice_STAR_from_clause_choice_auto_semi",
          let v1 = (* "export" *) token env v1 in
          let v2 =
            (match v2 with
            | `STAR_from_clause (v1, v2) -> R.Case ("STAR_from_clause",
                let v1 = (* "*" *) token env v1 in
                let v2 = map_from_clause env v2 in
                R.Tuple [v1; v2]
              )
            | `Name_export_from_clause (v1, v2) -> R.Case ("Name_export_from_clause",
                let v1 = map_namespace_export env v1 in
                let v2 = map_from_clause env v2 in
                R.Tuple [v1; v2]
              )
            | `Export_clause_from_clause (v1, v2) -> R.Case ("Export_clause_from_clause",
                let v1 = map_export_clause env v1 in
                let v2 = map_from_clause env v2 in
                R.Tuple [v1; v2]
              )
            | `Export_clause x -> R.Case ("Export_clause",
                map_export_clause env x
              )
            )
          in
          let v3 = map_semicolon env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Rep_deco_export_choice_decl (v1, v2, v3) -> R.Case ("Rep_deco_export_choice_decl",
          let v1 = R.List (List.map (map_decorator env) v1) in
          let v2 = (* "export" *) token env v2 in
          let v3 =
            (match v3 with
            | `Decl x -> R.Case ("Decl",
                map_declaration env x
              )
            | `Defa_choice_decl (v1, v2) -> R.Case ("Defa_choice_decl",
                let v1 = (* "default" *) token env v1 in
                let v2 =
                  (match v2 with
                  | `Decl x -> R.Case ("Decl",
                      map_declaration env x
                    )
                  | `Exp_choice_auto_semi (v1, v2) -> R.Case ("Exp_choice_auto_semi",
                      let v1 = map_expression env v1 in
                      let v2 = map_semicolon env v2 in
                      R.Tuple [v1; v2]
                    )
                  )
                in
                R.Tuple [v1; v2]
              )
            )
          in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Export_type_export_clause_opt_from_clause_choice_auto_semi (v1, v2, v3, v4, v5) -> R.Case ("Export_type_export_clause_opt_from_clause_choice_auto_semi",
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_export_clause env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_from_clause env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Export_EQ_exp_choice_auto_semi (v1, v2, v3, v4) -> R.Case ("Export_EQ_exp_choice_auto_semi",
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) -> R.Case ("Export_as_name_id_choice_auto_semi",
      let v1 = (* "export" *) token env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = (* "namespace" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `As_exp (v1, v2, v3) -> R.Case ("As_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 =
        (match v3 with
        | `Const tok -> R.Case ("Const",
            (* "const" *) token env tok
          )
        | `Type x -> R.Case ("Type",
            map_type_ env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Satiss_exp (v1, v2, v3) -> R.Case ("Satiss_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "satisfies" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Inst_exp (v1, v2) -> R.Case ("Inst_exp",
      let v1 = map_expression env v1 in
      let v2 = map_type_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Inte_module x -> R.Case ("Inte_module",
      map_internal_module env x
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
      map_jsx_element_ env x
    )
  | `Assign_exp (v1, v2, v3, v4) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "using" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Choice_choice_member_exp x -> R.Case ("Choice_choice_member_exp",
            map_lhs_expression env x
          )
        )
      in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Augm_assign_exp (v1, v2, v3) -> R.Case ("Augm_assign_exp",
      let v1 = map_augmented_assignment_lhs env v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ",
            (* ">>>=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
            (* "**=" *) token env tok
          )
        | `AMPAMPEQ tok -> R.Case ("AMPAMPEQ",
            (* "&&=" *) token env tok
          )
        | `BARBAREQ tok -> R.Case ("BARBAREQ",
            (* "||=" *) token env tok
          )
        | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ",
            (* "??=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Await_exp (v1, v2) -> R.Case ("Await_exp",
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `Typeof tok -> R.Case ("Typeof",
            (* "typeof" *) token env tok
          )
        | `Void tok -> R.Case ("Void",
            (* "void" *) token env tok
          )
        | `Delete tok -> R.Case ("Delete",
            (* "delete" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ternary_qmark *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `New_exp x -> R.Case ("New_exp",
      map_new_expression env x
    )
  | `Yield_exp (v1, v2) -> R.Case ("Yield_exp",
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | `STAR_exp (v1, v2) -> R.Case ("STAR_exp",
            let v1 = (* "*" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_exp opt -> R.Case ("Opt_exp",
            (match opt with
            | Some x -> R.Option (Some (
                map_expression env x
              ))
            | None -> R.Option None)
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expressions env v1 in
  let v2 = map_semicolon env v2 in
  R.Tuple [v1; v2]

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Seq_exp x -> R.Case ("Seq_exp",
      map_sequence_expression env x
    )
  )

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_extends_clause_single env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_extends_clause_single env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_extends_clause_single (env : env) ((v1, v2) : CST.extends_clause_single) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_extends_type_clause (env : env) ((v1, v2, v3) : CST.extends_type_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_anon_choice_type_id_a85f573 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_type_id_a85f573 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_statement_block env v2 in
  R.Tuple [v1; v2]

and map_for_header (env : env) ((v1, v2, v3, v4, v5) : CST.for_header) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Choice_choice_choice_member_exp x -> R.Case ("Choice_choice_choice_member_exp",
        (match x with
        | `Choice_choice_member_exp x -> R.Case ("Choice_choice_member_exp",
            map_lhs_expression env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        )
      )
    | `Var_choice_id_opt_init (v1, v2, v3) -> R.Case ("Var_choice_id_opt_init",
        let v1 = (* "var" *) token env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_initializer_ env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Choice_let_choice_id_opt_auto_semi (v1, v2, v3) -> R.Case ("Choice_let_choice_id_opt_auto_semi",
        let v1 = map_anon_choice_let_ca16eb3 env v1 in
        let v2 = map_anon_choice_type_id_940079a env v2 in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* automatic_semicolon *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v3 =
    (match v3 with
    | `In tok -> R.Case ("In",
        (* "in" *) token env tok
      )
    | `Of tok -> R.Case ("Of",
        (* "of" *) token env tok
      )
    )
  in
  let v4 = map_expressions env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_in_statement (env : env) ((v1, v2, v3, v4) : CST.for_in_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "await" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_for_header env v3 in
  let v4 = map_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Semg_ellips tok -> R.Case ("Semg_ellips",
        (* "..." *) token env tok
      )
    | `Choice_choice_lexi_decl_choice_choice_exp_SEMI_opt_choice_exp (v1, v2, v3) -> R.Case ("Choice_choice_lexi_decl_choice_choice_exp_SEMI_opt_choice_exp",
        let v1 =
          (match v1 with
          | `Choice_lexi_decl x -> R.Case ("Choice_lexi_decl",
              (match x with
              | `Lexi_decl x -> R.Case ("Lexi_decl",
                  map_lexical_declaration env x
                )
              | `Var_decl x -> R.Case ("Var_decl",
                  map_variable_declaration env x
                )
              )
            )
          | `Choice_exp_SEMI (v1, v2) -> R.Case ("Choice_exp_SEMI",
              let v1 = map_expressions env v1 in
              let v2 = (* ";" *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Empty_stmt tok -> R.Case ("Empty_stmt",
              (* ";" *) token env tok
            )
          )
        in
        let v2 =
          (match v2 with
          | `Choice_exp_SEMI (v1, v2) -> R.Case ("Choice_exp_SEMI",
              let v1 = map_expressions env v1 in
              let v2 = (* ";" *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Empty_stmt tok -> R.Case ("Empty_stmt",
              (* ";" *) token env tok
            )
          )
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_expressions env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Requ_param (v1, v2, v3) -> R.Case ("Requ_param",
      let v1 = map_parameter_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_initializer_ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_param (v1, v2, v3, v4) -> R.Case ("Opt_param",
      let v1 = map_parameter_name env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_initializer_ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_formal_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_formal_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 = map_call_signature_ env v4 in
  let v5 = map_statement_block env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_function_expression (env : env) ((v1, v2, v3, v4, v5) : CST.function_expression) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_call_signature_ env v4 in
  let v5 = map_statement_block env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_generator_function (env : env) ((v1, v2, v3, v4, v5, v6) : CST.generator_function) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* "*" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_call_signature_ env v5 in
  let v6 = map_statement_block env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_generator_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = (* "*" *) token env v3 in
  let v4 = (* identifier *) token env v4 in
  let v5 = map_call_signature_ env v5 in
  let v6 = map_statement_block env v6 in
  let v7 =
    (match v7 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Nested_type_id x -> R.Case ("Nested_type_id",
        map_nested_type_identifier env x
      )
    )
  in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_import_attribute (env : env) ((v1, v2) : CST.import_attribute) =
  let v1 =
    (match v1 with
    | `With tok -> R.Case ("With",
        (* "with" *) token env tok
      )
    | `Assert tok -> R.Case ("Assert",
        (* "assert" *) token env tok
      )
    )
  in
  let v2 = map_object_ env v2 in
  R.Tuple [v1; v2]

and map_import_statement (env : env) ((v1, v2, v3, v4, v5) : CST.import_statement) =
  let v1 = (* "import" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_type_2b11f6b env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Import_clause_from_clause (v1, v2) -> R.Case ("Import_clause_from_clause",
        let v1 = map_import_clause env v1 in
        let v2 = map_from_clause env v2 in
        R.Tuple [v1; v2]
      )
    | `Import_requ_clause x -> R.Case ("Import_requ_clause",
        map_import_require_clause env x
      )
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    )
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_import_attribute env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_semicolon env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_DASH_81d4819 env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "readonly" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    (match v3 with
    | `Choice_id_COLON_type (v1, v2, v3) -> R.Case ("Choice_id_COLON_type",
        let v1 = map_anon_choice_type_id_dd17e7d env v1 in
        let v2 = (* ":" *) token env v2 in
        let v3 = map_type_ env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Mapped_type_clause x -> R.Case ("Mapped_type_clause",
        map_mapped_type_clause env x
      )
    )
  in
  let v4 = (* "]" *) token env v4 in
  let v5 =
    (match v5 with
    | `Type_anno x -> R.Case ("Type_anno",
        map_type_annotation env x
      )
    | `Omit_type_anno x -> R.Case ("Omit_type_anno",
        map_omitting_type_annotation env x
      )
    | `Adding_type_anno x -> R.Case ("Adding_type_anno",
        map_adding_type_annotation env x
      )
    | `Opting_type_anno x -> R.Case ("Opting_type_anno",
        map_opting_type_annotation env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_infer_type (env : env) ((v1, v2, v3) : CST.infer_type) =
  let v1 = (* "infer" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "extends" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let v1 = (* "namespace" *) token env v1 in
  let v2 = map_module__ env v2 in
  R.Tuple [v1; v2]

and map_jsx_attribute (env : env) ((v1, v2) : CST.jsx_attribute) =
  let v1 = map_jsx_attribute_name env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_jsx_attribute_value env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_jsx_attribute_ (env : env) (x : CST.jsx_attribute_) =
  (match x with
  | `Choice_jsx_attr x -> R.Case ("Choice_jsx_attr",
      (match x with
      | `Jsx_attr x -> R.Case ("Jsx_attr",
          map_jsx_attribute env x
        )
      | `Jsx_exp x -> R.Case ("Jsx_exp",
          map_jsx_expression env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  (match x with
  | `Choice_jsx_str x -> R.Case ("Choice_jsx_str",
      (match x with
      | `Jsx_str x -> R.Case ("Jsx_str",
          map_jsx_string env x
        )
      | `Jsx_exp x -> R.Case ("Jsx_exp",
          map_jsx_expression env x
        )
      | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
          map_jsx_element_ env x
        )
      )
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_jsx_child (env : env) (x : CST.jsx_child) =
  (match x with
  | `Choice_jsx_text x -> R.Case ("Choice_jsx_text",
      (match x with
      | `Jsx_text tok -> R.Case ("Jsx_text",
          (* jsx_text *) token env tok
        )
      | `Html_char_ref tok -> R.Case ("Html_char_ref",
          (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *) token env tok
        )
      | `Choice_jsx_elem x -> R.Case ("Choice_jsx_elem",
          map_jsx_element_ env x
        )
      | `Jsx_exp x -> R.Case ("Jsx_exp",
          map_jsx_expression env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_jsx_element_ (env : env) (x : CST.jsx_element_) =
  (match x with
  | `Jsx_elem (v1, v2, v3) -> R.Case ("Jsx_elem",
      let v1 = map_jsx_opening_element env v1 in
      let v2 = R.List (List.map (map_jsx_child env) v2) in
      let v3 = map_jsx_closing_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Jsx_self_clos_elem (v1, v2, v3) -> R.Case ("Jsx_self_clos_elem",
      let v1 = (* "<" *) token env v1 in
      let v2 =
        map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 env v2
      in
      let v3 = (* "/>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Seq_exp x -> R.Case ("Seq_exp",
            map_sequence_expression env x
          )
        | `Spread_elem x -> R.Case ("Spread_elem",
            map_spread_element env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_jsx_opening_element (env : env) ((v1, v2, v3) : CST.jsx_opening_element) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 env v2
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 = map_anon_choice_type_id_dd17e7d env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration) =
  let v1 = map_anon_choice_let_ca16eb3 env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_semicolon env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lhs_expression (env : env) (x : CST.lhs_expression) =
  (match x with
  | `Choice_member_exp x -> R.Case ("Choice_member_exp",
      (match x with
      | `Member_exp x -> R.Case ("Member_exp",
          map_member_expression env x
        )
      | `Subs_exp x -> R.Case ("Subs_exp",
          map_subscript_expression env x
        )
      | `Choice_unde x -> R.Case ("Choice_unde",
          map_identifier_ env x
        )
      | `Choice_decl x -> R.Case ("Choice_decl",
          map_reserved_identifier env x
        )
      | `Dest_pat x -> R.Case ("Dest_pat",
          map_destructuring_pattern env x
        )
      )
    )
  | `Non_null_exp x -> R.Case ("Non_null_exp",
      map_non_null_expression env x
    )
  )

and map_mapped_type_clause (env : env) ((v1, v2, v3, v4) : CST.mapped_type_clause) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "in" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    | `Import tok -> R.Case ("Import",
        (* import *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `Opt_chain tok -> R.Case ("Opt_chain",
        (* "?." *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
        (* private_property_identifier *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Semg_ellips tok -> R.Case ("Semg_ellips",
        (* "..." *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) : CST.method_definition) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_accessibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "static" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "override" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "readonly" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anon_choice_get_8fb02de env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_property_name env v7 in
  let v8 =
    (match v8 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v9 = map_call_signature_ env v9 in
  let v10 = map_statement_block env v10 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]

and map_method_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.method_signature) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_accessibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "static" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "override" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "readonly" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "async" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anon_choice_get_8fb02de env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_property_name env v7 in
  let v8 =
    (match v8 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v9 = map_call_signature_ env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 =
    (match v1 with
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `Nested_id x -> R.Case ("Nested_id",
        map_nested_identifier env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statement_block env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_new_expression (env : env) ((v1, v2, v3, v4) : CST.new_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 = map_primary_expression env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_non_null_expression (env : env) ((v1, v2) : CST.non_null_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "!" *) token env v2 in
  R.Tuple [v1; v2]

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice_pair_20c9acd env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_anon_choice_pair_20c9acd env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    (match v1 with
    | `LCURL tok -> R.Case ("LCURL",
        (* "{" *) token env tok
      )
    | `LCURLBAR tok -> R.Case ("LCURLBAR",
        (* "{|" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              (match x with
              | `COMMA tok -> R.Case ("COMMA",
                  (* "," *) token env tok
                )
              | `SEMI tok -> R.Case ("SEMI",
                  (* ";" *) token env tok
                )
              )
            ))
          | None -> R.Option None)
        in
        let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_COMMA_5194cb4 env v1 in
            let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_anon_choice_COMMA_5194cb4 env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `RCURL tok -> R.Case ("RCURL",
        (* "}" *) token env tok
      )
    | `BARRCURL tok -> R.Case ("BARRCURL",
        (* "|}" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_omitting_type_annotation (env : env) ((v1, v2) : CST.omitting_type_annotation) =
  let v1 = (* "-?:" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_opting_type_annotation (env : env) ((v1, v2) : CST.opting_type_annotation) =
  let v1 = (* "?:" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_pair (env : env) (x : CST.pair) =
  (match x with
  | `Prop_name_COLON_exp (v1, v2, v3) -> R.Case ("Prop_name_COLON_exp",
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_pair_pattern (env : env) (x : CST.pair_pattern) =
  (match x with
  | `Prop_name_COLON_choice_pat (v1, v2, v3) -> R.Case ("Prop_name_COLON_choice_pat",
      let v1 = map_property_name env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_anon_choice_pat_3297d92 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_parameter_name (env : env) ((v1, v2, v3, v4, v5) : CST.parameter_name) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_accessibility_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "override" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "readonly" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | `Pat x -> R.Case ("Pat",
        map_pattern env x
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Exp_opt_type_anno (v1, v2) -> R.Case ("Exp_opt_type_anno",
        let v1 = map_expression env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_type_annotation env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Seq_exp x -> R.Case ("Seq_exp",
        map_sequence_expression env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Choice_choice_choice_member_exp x -> R.Case ("Choice_choice_choice_member_exp",
      (match x with
      | `Choice_choice_member_exp x -> R.Case ("Choice_choice_member_exp",
          map_lhs_expression env x
        )
      | `Rest_pat x -> R.Case ("Rest_pat",
          map_rest_pattern env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_choice_subs_exp x -> R.Case ("Choice_choice_subs_exp",
      (match x with
      | `Choice_subs_exp x -> R.Case ("Choice_subs_exp",
          (match x with
          | `Subs_exp x -> R.Case ("Subs_exp",
              map_subscript_expression env x
            )
          | `Member_exp x -> R.Case ("Member_exp",
              map_member_expression env x
            )
          | `Paren_exp x -> R.Case ("Paren_exp",
              map_parenthesized_expression env x
            )
          | `Choice_unde x -> R.Case ("Choice_unde",
              map_identifier_ env x
            )
          | `Choice_decl x -> R.Case ("Choice_decl",
              map_reserved_identifier env x
            )
          | `This tok -> R.Case ("This",
              (* "this" *) token env tok
            )
          | `Super tok -> R.Case ("Super",
              (* "super" *) token env tok
            )
          | `Num tok -> R.Case ("Num",
              (* number *) token env tok
            )
          | `Str x -> R.Case ("Str",
              map_string_ env x
            )
          | `Temp_str x -> R.Case ("Temp_str",
              map_template_string env x
            )
          | `Regex x -> R.Case ("Regex",
              map_regex env x
            )
          | `True tok -> R.Case ("True",
              (* "true" *) token env tok
            )
          | `False tok -> R.Case ("False",
              (* "false" *) token env tok
            )
          | `Null tok -> R.Case ("Null",
              (* "null" *) token env tok
            )
          | `Obj x -> R.Case ("Obj",
              map_object_ env x
            )
          | `Array x -> R.Case ("Array",
              map_array_ env x
            )
          | `Func_exp x -> R.Case ("Func_exp",
              map_function_expression env x
            )
          | `Arrow_func x -> R.Case ("Arrow_func",
              map_arrow_function env x
            )
          | `Gene_func x -> R.Case ("Gene_func",
              map_generator_function env x
            )
          | `Class x -> R.Case ("Class",
              map_class_ env x
            )
          | `Meta_prop x -> R.Case ("Meta_prop",
              map_meta_property env x
            )
          | `Call_exp x -> R.Case ("Call_exp",
              map_call_expression env x
            )
          )
        )
      | `Non_null_exp x -> R.Case ("Non_null_exp",
          map_non_null_expression env x
        )
      )
    )
  | `Semg_exp_ellips tok -> R.Case ("Semg_exp_ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_meta_ellips tok -> R.Case ("Semg_meta_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_primary_type (env : env) (x : CST.primary_type) =
  (match x with
  | `Paren_type (v1, v2, v3) -> R.Case ("Paren_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pred_type x -> R.Case ("Pred_type",
      map_predefined_type env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Nested_type_id x -> R.Case ("Nested_type_id",
      map_nested_type_identifier env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  | `Obj_type x -> R.Case ("Obj_type",
      map_object_type env x
    )
  | `Array_type (v1, v2, v3) -> R.Case ("Array_type",
      let v1 = map_primary_type env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple_type (v1, v2, v3, v4) -> R.Case ("Tuple_type",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_tuple_type_member env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_tuple_type_member env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Flow_maybe_type (v1, v2) -> R.Case ("Flow_maybe_type",
      let v1 = (* "?" *) token env v1 in
      let v2 = map_primary_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Type_query (v1, v2) -> R.Case ("Type_query",
      let v1 = (* "typeof" *) token env v1 in
      let v2 =
        (match v2 with
        | `Type_query_subs_exp x -> R.Case ("Type_query_subs_exp",
            map_type_query_subscript_expression env x
          )
        | `Type_query_member_exp x -> R.Case ("Type_query_member_exp",
            map_type_query_member_expression env x
          )
        | `Type_query_call_exp x -> R.Case ("Type_query_call_exp",
            map_type_query_call_expression env x
          )
        | `Type_query_inst_exp x -> R.Case ("Type_query_inst_exp",
            map_type_query_instantiation_expression env x
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `This tok -> R.Case ("This",
            (* "this" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Index_type_query (v1, v2) -> R.Case ("Index_type_query",
      let v1 = (* "keyof" *) token env v1 in
      let v2 = map_primary_type env v2 in
      R.Tuple [v1; v2]
    )
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `Exis_type tok -> R.Case ("Exis_type",
      (* "*" *) token env tok
    )
  | `Lit_type x -> R.Case ("Lit_type",
      map_literal_type env x
    )
  | `Lookup_type (v1, v2, v3, v4) -> R.Case ("Lookup_type",
      let v1 = map_primary_type env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cond_type (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Cond_type",
      let v1 = map_type_ env v1 in
      let v2 = (* "extends" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 = (* "?" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_type_ env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Temp_lit_type (v1, v2, v3) -> R.Case ("Temp_lit_type",
      let v1 = (* "`" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Temp_chars tok -> R.Case ("Temp_chars",
              (* template_chars *) token env tok
            )
          | `Temp_type x -> R.Case ("Temp_type",
              map_template_type env x
            )
          )
        ) v2)
      in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Inte_type (v1, v2, v3) -> R.Case ("Inte_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Union_type (v1, v2, v3) -> R.Case ("Union_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  )

and map_property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_type_id_dd17e7d env x
    )
  | `Priv_prop_id tok -> R.Case ("Priv_prop_id",
      (* private_property_identifier *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Comp_prop_name (v1, v2, v3) -> R.Case ("Comp_prop_name",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_public_field_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.public_field_definition) =
  let v1 = R.List (List.map (map_decorator env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Decl_opt_acce_modi (v1, v2) -> R.Case ("Decl_opt_acce_modi",
            let v1 = (* "declare" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_accessibility_modifier env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Acce_modi_opt_decl (v1, v2) -> R.Case ("Acce_modi_opt_decl",
            let v1 = map_accessibility_modifier env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* "declare" *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Opt_static_opt_over_modi_opt_read (v1, v2, v3) -> R.Case ("Opt_static_opt_over_modi_opt_read",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "static" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "override" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "readonly" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Opt_abst_opt_read (v1, v2) -> R.Case ("Opt_abst_opt_read",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "abstract" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "readonly" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Opt_read_opt_abst (v1, v2) -> R.Case ("Opt_read_opt_abst",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "readonly" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "abstract" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Opt_acce opt -> R.Case ("Opt_acce",
        (match opt with
        | Some tok -> R.Option (Some (
            (* "accessor" *) token env tok
          ))
        | None -> R.Option None)
      )
    )
  in
  let v4 = map_property_name env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        (match x with
        | `QMARK tok -> R.Case ("QMARK",
            (* "?" *) token env tok
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_initializer_ env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_lhs_expression env v2 in
  R.Tuple [v1; v2]

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expressions env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_semicolon env v3 in
  R.Tuple [v1; v2; v3]

and map_sequence_expression (env : env) ((v1, v2) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_export_stmt x -> R.Case ("Choice_export_stmt",
      (match x with
      | `Export_stmt x -> R.Case ("Export_stmt",
          map_export_statement env x
        )
      | `Import_stmt x -> R.Case ("Import_stmt",
          map_import_statement env x
        )
      | `Debu_stmt x -> R.Case ("Debu_stmt",
          map_debugger_statement env x
        )
      | `Exp_stmt x -> R.Case ("Exp_stmt",
          map_expression_statement env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Stmt_blk x -> R.Case ("Stmt_blk",
          map_statement_block env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `Switch_stmt x -> R.Case ("Switch_stmt",
          map_switch_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `For_in_stmt x -> R.Case ("For_in_stmt",
          map_for_in_statement env x
        )
      | `While_stmt x -> R.Case ("While_stmt",
          map_while_statement env x
        )
      | `Do_stmt x -> R.Case ("Do_stmt",
          map_do_statement env x
        )
      | `Try_stmt x -> R.Case ("Try_stmt",
          map_try_statement env x
        )
      | `With_stmt x -> R.Case ("With_stmt",
          map_with_statement env x
        )
      | `Brk_stmt x -> R.Case ("Brk_stmt",
          map_break_statement env x
        )
      | `Cont_stmt x -> R.Case ("Cont_stmt",
          map_continue_statement env x
        )
      | `Ret_stmt x -> R.Case ("Ret_stmt",
          map_return_statement env x
        )
      | `Throw_stmt x -> R.Case ("Throw_stmt",
          map_throw_statement env x
        )
      | `Empty_stmt tok -> R.Case ("Empty_stmt",
          (* ";" *) token env tok
        )
      | `Labe_stmt x -> R.Case ("Labe_stmt",
          map_labeled_statement env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?." *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "[" *) token env v3 in
  let v4 = map_expressions env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Switch_case x -> R.Case ("Switch_case",
          map_switch_case env x
        )
      | `Switch_defa x -> R.Case ("Switch_defa",
          map_switch_default env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = R.List (List.map (map_statement env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = R.List (List.map (map_statement env) v3) in
  R.Tuple [v1; v2; v3]

and map_switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_switch_body env v3 in
  R.Tuple [v1; v2; v3]

and map_template_string (env : env) ((v1, v2, v3) : CST.template_string) =
  let v1 = (* "`" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Temp_chars tok -> R.Case ("Temp_chars",
          (* template_chars *) token env tok
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      | `Temp_subs x -> R.Case ("Temp_subs",
          map_template_substitution env x
        )
      )
    ) v2)
  in
  let v3 = (* "`" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution) =
  let v1 = (* "${" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_template_type (env : env) ((v1, v2, v3) : CST.template_type) =
  let v1 = (* "${" *) token env v1 in
  let v2 =
    (match v2 with
    | `Prim_type x -> R.Case ("Prim_type",
        map_primary_type env x
      )
    | `Infer_type x -> R.Case ("Infer_type",
        map_infer_type env x
      )
    )
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_expressions env v2 in
  let v3 = map_semicolon env v3 in
  R.Tuple [v1; v2; v3]

and map_try_statement (env : env) ((v1, v2, v3, v4) : CST.try_statement) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_statement_block env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_catch_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_finally_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_tuple_type_member (env : env) (x : CST.tuple_type_member) =
  (match x with
  | `Tuple_param (v1, v2) -> R.Case ("Tuple_param",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Rest_pat x -> R.Case ("Rest_pat",
            map_rest_pattern env x
          )
        )
      in
      let v2 = map_type_annotation env v2 in
      R.Tuple [v1; v2]
    )
  | `Opt_tuple_param (v1, v2, v3) -> R.Case ("Opt_tuple_param",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_type_annotation env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_type (v1, v2) -> R.Case ("Opt_type",
      let v1 = map_type_ env v1 in
      let v2 = (* "?" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Rest_type (v1, v2) -> R.Case ("Rest_type",
      let v1 = (* "..." *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Prim_type x -> R.Case ("Prim_type",
      map_primary_type env x
    )
  | `Func_type (v1, v2, v3, v4) -> R.Case ("Func_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_formal_parameters env v2 in
      let v3 = (* "=>" *) token env v3 in
      let v4 =
        (match v4 with
        | `Type x -> R.Case ("Type",
            map_type_ env x
          )
        | `Asserts x -> R.Case ("Asserts",
            map_asserts env x
          )
        | `Type_pred x -> R.Case ("Type_pred",
            map_type_predicate env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Read_type (v1, v2) -> R.Case ("Read_type",
      let v1 = (* "readonly" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Cons_type (v1, v2, v3, v4, v5, v6) -> R.Case ("Cons_type",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "abstract" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "new" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_parameters env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_formal_parameters env v4 in
      let v5 = (* "=>" *) token env v5 in
      let v6 = map_type_ env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Infer_type x -> R.Case ("Infer_type",
      map_infer_type env x
    )
  | `Type_query_member_exp_in_type_anno x -> R.Case ("Type_query_member_exp_in_type_anno",
      map_type_query_member_expression_in_type_annotation env x
    )
  | `Type_query_call_exp_in_type_anno x -> R.Case ("Type_query_call_exp_in_type_anno",
      map_type_query_call_expression_in_type_annotation env x
    )
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "const" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_constraint_ env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_default_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ">" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* identifier *) token env tok
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    | `Pred_type x -> R.Case ("Pred_type",
        map_predefined_type env x
      )
    )
  in
  let v2 = (* "is" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_type_predicate_annotation (env : env) ((v1, v2) : CST.type_predicate_annotation) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_predicate env v2 in
  R.Tuple [v1; v2]

and map_type_query_call_expression (env : env) ((v1, v2) : CST.type_query_call_expression) =
  let v1 = map_anon_choice_import_c99ceb4 env v1 in
  let v2 = map_arguments env v2 in
  R.Tuple [v1; v2]

and map_type_query_call_expression_in_type_annotation (env : env) ((v1, v2) : CST.type_query_call_expression_in_type_annotation) =
  let v1 =
    (match v1 with
    | `Import tok -> R.Case ("Import",
        (* import *) token env tok
      )
    | `Type_query_member_exp_in_type_anno x -> R.Case ("Type_query_member_exp_in_type_anno",
        map_type_query_member_expression_in_type_annotation env x
      )
    )
  in
  let v2 = map_arguments env v2 in
  R.Tuple [v1; v2]

and map_type_query_instantiation_expression (env : env) ((v1, v2) : CST.type_query_instantiation_expression) =
  let v1 = map_anon_choice_import_c99ceb4 env v1 in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_type_query_member_expression (env : env) ((v1, v2, v3) : CST.type_query_member_expression) =
  let v1 = map_anon_choice_type_id_43e1312 env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `QMARKDOT tok -> R.Case ("QMARKDOT",
        (* "?." *) token env tok
      )
    )
  in
  let v3 = map_anon_choice_priv_prop_id_89abb74 env v3 in
  R.Tuple [v1; v2; v3]

and map_type_query_member_expression_in_type_annotation (env : env) ((v1, v2, v3) : CST.type_query_member_expression_in_type_annotation) =
  let v1 =
    (match v1 with
    | `Import tok -> R.Case ("Import",
        (* import *) token env tok
      )
    | `Type_query_member_exp_in_type_anno x -> R.Case ("Type_query_member_exp_in_type_anno",
        map_type_query_member_expression_in_type_annotation env x
      )
    | `Type_query_call_exp_in_type_anno x -> R.Case ("Type_query_call_exp_in_type_anno",
        map_type_query_call_expression_in_type_annotation env x
      )
    )
  in
  let v2 = (* "." *) token env v2 in
  let v3 = map_anon_choice_priv_prop_id_89abb74 env v3 in
  R.Tuple [v1; v2; v3]

and map_type_query_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.type_query_subscript_expression) =
  let v1 = map_anon_choice_type_id_43e1312 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "?." *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "[" *) token env v3 in
  let v4 =
    (match v4 with
    | `Pred_type x -> R.Case ("Pred_type",
        map_predefined_type env x
      )
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Num tok -> R.Case ("Num",
        (* number *) token env tok
      )
    )
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) -> R.Case ("Exp_choice_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_PLUSPLUS_exp (v1, v2) -> R.Case ("Choice_PLUSPLUS_exp",
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.variable_declaration) =
  let v1 = (* "var" *) token env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = map_semicolon env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_variable_declarator (env : env) (x : CST.variable_declarator) =
  (match x with
  | `Choice_id_opt_type_anno_opt_init (v1, v2, v3) -> R.Case ("Choice_id_opt_type_anno_opt_init",
      let v1 = map_anon_choice_type_id_940079a env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_initializer_ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Id_BANG_type_anno (v1, v2, v3) -> R.Case ("Id_BANG_type_anno",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "!" *) token env v2 in
      let v3 = map_type_annotation env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_with_statement (env : env) ((v1, v2, v3) : CST.with_statement) =
  let v1 = (* "with" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

let map_method_pattern (env : env) (x : CST.method_pattern) =
  (match x with
  | `Rep1_deco_public_field_defi (v1, v2) -> R.Case ("Rep1_deco_public_field_defi",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 = map_public_field_definition env v2 in
      R.Tuple [v1; v2]
    )
  | `Rep_deco_choice_abst_meth_sign (v1, v2) -> R.Case ("Rep_deco_choice_abst_meth_sign",
      let v1 = R.List (List.map (map_decorator env) v1) in
      let v2 =
        (match v2 with
        | `Abst_meth_sign x -> R.Case ("Abst_meth_sign",
            map_abstract_method_signature env x
          )
        | `Index_sign x -> R.Case ("Index_sign",
            map_index_signature env x
          )
        | `Meth_sign x -> R.Case ("Meth_sign",
            map_method_signature env x
          )
        | `Meth_defi_opt_choice_auto_semi (v1, v2) -> R.Case ("Meth_defi_opt_choice_auto_semi",
            let v1 = map_method_definition env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_semicolon env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

let map_semgrep_pattern (env : env) (x : CST.semgrep_pattern) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Pair_opt_COMMA (v1, v2) -> R.Case ("Pair_opt_COMMA",
      let v1 = map_pair env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Meth_pat x -> R.Case ("Meth_pat",
      map_method_pattern env x
    )
  | `Func_decl_pat (v1, v2, v3, v4, v5, v6) -> R.Case ("Func_decl_pat",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "async" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "function" *) token env v2 in
      let v3 =
        (match v3 with
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        | `Semg_ellips tok -> R.Case ("Semg_ellips",
            (* "..." *) token env tok
          )
        )
      in
      let v4 = map_call_signature_ env v4 in
      let v5 = map_statement_block env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Fina_clause x -> R.Case ("Fina_clause",
      map_finally_clause env x
    )
  | `Catch_clause x -> R.Case ("Catch_clause",
      map_catch_clause env x
    )
  )

let map_program (env : env) (x : CST.program) =
  (match x with
  | `Opt_hash_bang_line_rep_stmt (v1, v2) -> R.Case ("Opt_hash_bang_line_rep_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern #!.* *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_statement env) v2) in
      R.Tuple [v1; v2]
    )
  | `Switch_case x -> R.Case ("Switch_case",
      map_switch_case env x
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_semgrep_pattern env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_html_comment (env : env) (tok : CST.html_comment) =
  (* html_comment *) token env tok

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Html_comment (_loc, x) -> ("html_comment", "html_comment", map_html_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
