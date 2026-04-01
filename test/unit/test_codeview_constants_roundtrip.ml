(** QCheck roundtrip tests for CodeView constants.

    Uses ppx_import to copy type definitions and ppx_deriving_qcheck to generate
    random generators. When variants are added to the library, the generators
    update automatically. *)

open Pdb.Codeview_constants

(* Import types and derive QCheck generators *)

type leaf_kind = [%import: Pdb.Codeview_constants.leaf_kind] [@@deriving qcheck]

type symbol_kind = [%import: Pdb.Codeview_constants.symbol_kind]
[@@deriving qcheck]

type simple_type_kind = [%import: Pdb.Codeview_constants.simple_type_kind]
[@@deriving qcheck]

type simple_type_mode = [%import: Pdb.Codeview_constants.simple_type_mode]
[@@deriving qcheck]

type calling_convention = [%import: Pdb.Codeview_constants.calling_convention]
[@@deriving qcheck]

type pointer_kind = [%import: Pdb.Codeview_constants.pointer_kind]
[@@deriving qcheck]

type pointer_mode = [%import: Pdb.Codeview_constants.pointer_mode]
[@@deriving qcheck]

type member_access = [%import: Pdb.Codeview_constants.member_access]
[@@deriving qcheck]

type method_kind = [%import: Pdb.Codeview_constants.method_kind]
[@@deriving qcheck]

type debug_subsection_kind =
  [%import: Pdb.Codeview_constants.debug_subsection_kind]
[@@deriving qcheck]

(* --- Helpers --- *)

let arb gen = QCheck.make gen

let roundtrip name gen encode decode =
  QCheck.Test.make ~name:(name ^ " roundtrip") (arb gen) (fun v ->
      decode (encode v) = v)

(* --- Roundtrip tests --- *)

let roundtrip_tests =
  [
    roundtrip "leaf_kind" gen_leaf_kind int_of_leaf_kind leaf_kind_of_int;
    roundtrip "symbol_kind" gen_symbol_kind int_of_symbol_kind
      symbol_kind_of_int;
    roundtrip "simple_type_kind" gen_simple_type_kind int_of_simple_type_kind
      simple_type_kind_of_int;
    roundtrip "simple_type_mode" gen_simple_type_mode int_of_simple_type_mode
      simple_type_mode_of_int;
    roundtrip "calling_convention" gen_calling_convention
      int_of_calling_convention calling_convention_of_int;
    roundtrip "pointer_kind" gen_pointer_kind int_of_pointer_kind
      pointer_kind_of_int;
    roundtrip "pointer_mode" gen_pointer_mode int_of_pointer_mode
      pointer_mode_of_int;
    roundtrip "member_access" gen_member_access int_of_member_access
      member_access_of_int;
    roundtrip "method_kind" gen_method_kind int_of_method_kind
      method_kind_of_int;
    roundtrip "debug_subsection_kind" gen_debug_subsection_kind
      int_of_debug_subsection_kind debug_subsection_kind_of_int;
  ]

(* --- String consistency tests --- *)

let string_tests =
  [
    QCheck.Test.make ~name:"leaf_kind string non-empty" (arb gen_leaf_kind)
      (fun v -> String.length (string_of_leaf_kind v) > 0);
    QCheck.Test.make ~name:"symbol_kind string non-empty" (arb gen_symbol_kind)
      (fun v -> String.length (string_of_symbol_kind v) > 0);
    QCheck.Test.make ~name:"simple_type_kind string non-empty"
      (arb gen_simple_type_kind) (fun v ->
        String.length (string_of_simple_type_kind v) > 0);
    QCheck.Test.make ~name:"calling_convention string non-empty"
      (arb gen_calling_convention) (fun v ->
        String.length (string_of_calling_convention v) > 0);
  ]

let () =
  let open Alcotest in
  run "CodeView constants roundtrip"
    [
      ("roundtrip", List.map QCheck_alcotest.to_alcotest roundtrip_tests);
      ("string", List.map QCheck_alcotest.to_alcotest string_tests);
    ]
