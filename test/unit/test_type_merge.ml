(** Tests for type deduplication. *)

let u32 n = Unsigned.UInt32.of_int n
let ti n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n)
let ti_to_int ti = Unsigned.UInt32.to_int (Pdb.Type_index.to_u32 ti)

let test_distinct_records () =
  let t = Pdb.Type_merge.create () in
  let idx0 =
    Pdb.Type_merge.insert t
      (Pdb.Codeview_types.ArgList { args = [||] })
  in
  let idx1 =
    Pdb.Type_merge.insert t
      (Pdb.Codeview_types.Procedure
         {
           return_type = ti 0x0074;
           calling_conv = Pdb.Codeview_constants.NearC;
      options = 0;
           param_count = 0;
           arg_list = ti 0x1000;
         })
  in
  Alcotest.(check int) "first index" 0x1000 (ti_to_int idx0);
  Alcotest.(check int) "second index" 0x1001 (ti_to_int idx1);
  Alcotest.(check int) "count" 2 (Pdb.Type_merge.count t);
  let records = Pdb.Type_merge.records t in
  Alcotest.(check int) "records length" 2 (List.length records)

let test_duplicate_records () =
  let t = Pdb.Type_merge.create () in
  let record = Pdb.Codeview_types.ArgList { args = [||] } in
  let idx0 = Pdb.Type_merge.insert t record in
  let idx1 = Pdb.Type_merge.insert t record in
  Alcotest.(check int) "same index" (ti_to_int idx0) (ti_to_int idx1);
  Alcotest.(check int) "count still 1" 1 (Pdb.Type_merge.count t)

let test_duplicate_pointer () =
  let t = Pdb.Type_merge.create () in
  let record =
    Pdb.Codeview_types.Pointer { pointee_type = ti 0x0074; attrs = u32 0x1000C }
  in
  let idx0 = Pdb.Type_merge.insert t record in
  let idx1 = Pdb.Type_merge.insert t record in
  Alcotest.(check int) "same index" (ti_to_int idx0) (ti_to_int idx1);
  Alcotest.(check int) "count 1" 1 (Pdb.Type_merge.count t)

let test_similar_but_different () =
  (* Two structures with the same name but different sizes *)
  let t = Pdb.Type_merge.create () in
  let idx0 =
    Pdb.Type_merge.insert t
      (Pdb.Codeview_types.Structure
         {
           field_count = 1;
           properties = Pdb.Codeview_types.parse_type_properties 0;
           field_list = ti 0x1000;
           derived_from = ti 0;
           vtable_shape = ti 0;
           size = 4L;
           name = "Point";
           unique_name = Option.None;
         })
  in
  let idx1 =
    Pdb.Type_merge.insert t
      (Pdb.Codeview_types.Structure
         {
           field_count = 2;
           properties = Pdb.Codeview_types.parse_type_properties 0;
           field_list = ti 0x1001;
           derived_from = ti 0;
           vtable_shape = ti 0;
           size = 8L;
           name = "Point";
           unique_name = Option.None;
         })
  in
  Alcotest.(check bool) "different indices" true (ti_to_int idx0 <> ti_to_int idx1);
  Alcotest.(check int) "count 2" 2 (Pdb.Type_merge.count t)

let test_find_index () =
  let t = Pdb.Type_merge.create () in
  let record = Pdb.Codeview_types.Bitfield
    { underlying_type = ti 0x0074; length = 5; position = 3 }
  in
  Alcotest.(check bool) "not found before insert" true
    (Pdb.Type_merge.find_index t record = Option.None);
  let idx = Pdb.Type_merge.insert t record in
  Alcotest.(check bool) "found after insert" true
    (Pdb.Type_merge.find_index t record = Some idx)

let test_records_in_order () =
  let t = Pdb.Type_merge.create () in
  let r0 = Pdb.Codeview_types.ArgList { args = [||] } in
  let r1 = Pdb.Codeview_types.ArgList { args = [| ti 0x0074 |] } in
  let r2 = Pdb.Codeview_types.ArgList { args = [| ti 0x0074; ti 0x0041 |] } in
  let _ = Pdb.Type_merge.insert t r0 in
  let _ = Pdb.Type_merge.insert t r1 in
  let _ = Pdb.Type_merge.insert t r2 in
  (* Insert duplicates -- should not change order *)
  let _ = Pdb.Type_merge.insert t r1 in
  let _ = Pdb.Type_merge.insert t r0 in
  let records = Pdb.Type_merge.records t in
  Alcotest.(check int) "3 unique records" 3 (List.length records);
  (* Verify order: 0 args, 1 arg, 2 args *)
  (match List.nth records 0 with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "first has 0 args" 0 (Array.length args)
  | _ -> Alcotest.fail "expected ArgList");
  (match List.nth records 1 with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "second has 1 arg" 1 (Array.length args)
  | _ -> Alcotest.fail "expected ArgList");
  match List.nth records 2 with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "third has 2 args" 2 (Array.length args)
  | _ -> Alcotest.fail "expected ArgList"

let test_many_duplicates () =
  let t = Pdb.Type_merge.create () in
  let record =
    Pdb.Codeview_types.Modifier { modified_type = ti 0x0074; modifiers = 1 }
  in
  for _ = 1 to 100 do
    ignore (Pdb.Type_merge.insert t record)
  done;
  Alcotest.(check int) "still 1" 1 (Pdb.Type_merge.count t)

(** {2 map_type_indices} *)

let id_int t = Unsigned.UInt32.to_int (Pdb.Type_index.to_u32 t)

(** type_ref and id_ref are applied to the correct fields. FuncId has an
    id reference (scope_id) and a type reference (func_type). *)
let test_map_distinguishes_type_and_id () =
  let bump_type t = ti (id_int t + 0x100) in
  let bump_id t = ti (id_int t + 0x200) in
  let r =
    Pdb.Codeview_types.FuncId
      { scope_id = ti 0x1000; func_type = ti 0x1000; name = "f" }
  in
  match
    Pdb.Codeview_types.map_type_indices ~type_ref:bump_type ~id_ref:bump_id r
  with
  | Pdb.Codeview_types.FuncId { scope_id; func_type; _ } ->
      Alcotest.(check int) "scope_id via id_ref" 0x1200 (id_int scope_id);
      Alcotest.(check int) "func_type via type_ref" 0x1100 (id_int func_type)
  | _ -> Alcotest.fail "expected FuncId"

(** Simple (built-in) indices are passed to the callback but a no-op
    remap leaves them unchanged; user indices in a Pointer are remapped. *)
let test_map_pointer_and_arglist () =
  let f t = ti (id_int t + 1) in
  (match
     Pdb.Codeview_types.map_type_indices ~type_ref:f ~id_ref:f
       (Pdb.Codeview_types.Pointer
          { pointee_type = ti 0x1005; attrs = Unsigned.UInt32.of_int 0x1000C })
   with
  | Pdb.Codeview_types.Pointer { pointee_type; _ } ->
      Alcotest.(check int) "pointee remapped" 0x1006 (id_int pointee_type)
  | _ -> Alcotest.fail "expected Pointer");
  match
    Pdb.Codeview_types.map_type_indices ~type_ref:f ~id_ref:f
      (Pdb.Codeview_types.ArgList { args = [| ti 0x1000; ti 0x1001 |] })
  with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "arg0" 0x1001 (id_int args.(0));
      Alcotest.(check int) "arg1" 0x1002 (id_int args.(1))
  | _ -> Alcotest.fail "expected ArgList"

(** {2 Cross-compilation-unit merging} *)

(* A pointer-to-int and a procedure returning that pointer, as a small CU. *)
let cu_records ptr_index =
  [
    Pdb.Codeview_types.Pointer
      { pointee_type = ti 0x0074; attrs = Unsigned.UInt32.of_int 0x1000C };
    Pdb.Codeview_types.Procedure
      {
        return_type = ti ptr_index;
        calling_conv = Pdb.Codeview_constants.NearC;
        options = 0;
        param_count = 0;
        arg_list = ti 0x0000;
      };
  ]

(** Two structurally identical CUs collapse to a single merged set, and
    references are rewritten correctly. *)
let test_cross_identical_units () =
  let c = Pdb.Type_merge.create_cross () in
  (* In both CUs the pointer is local index 0x1000 and the procedure
     references it. *)
  let remap1 = Pdb.Type_merge.merge_types c (cu_records 0x1000) in
  let remap2 = Pdb.Type_merge.merge_types c (cu_records 0x1000) in
  let merged = Pdb.Type_merge.cross_types c in
  Alcotest.(check int) "2 unique records" 2 (List.length merged);
  (* Both units map to the same merged indices. *)
  Alcotest.(check int) "remap1.(0) = remap2.(0)" (id_int remap1.(0))
    (id_int remap2.(0));
  Alcotest.(check int) "remap1.(1) = remap2.(1)" (id_int remap1.(1))
    (id_int remap2.(1));
  (* The merged procedure's return_type points at the merged pointer. *)
  (match List.nth merged 1 with
  | Pdb.Codeview_types.Procedure { return_type; _ } ->
      Alcotest.(check int) "proc return -> merged pointer"
        (id_int remap1.(0)) (id_int return_type)
  | _ -> Alcotest.fail "expected Procedure")

(** Distinct types across units stay distinct. *)
let test_cross_distinct_units () =
  let c = Pdb.Type_merge.create_cross () in
  let _ =
    Pdb.Type_merge.merge_types c
      [
        Pdb.Codeview_types.Pointer
          { pointee_type = ti 0x0074; attrs = Unsigned.UInt32.of_int 0x1000C };
      ]
  in
  let _ =
    Pdb.Type_merge.merge_types c
      [
        Pdb.Codeview_types.Pointer
          { pointee_type = ti 0x0075; attrs = Unsigned.UInt32.of_int 0x1000C };
      ]
  in
  Alcotest.(check int) "2 distinct pointers" 2
    (List.length (Pdb.Type_merge.cross_types c))

(** Cross-unit merge with differently-numbered references: CU2 puts the
    same pointer at a different local index, but it still dedups. *)
let test_cross_renumbered_refs () =
  let c = Pdb.Type_merge.create_cross () in
  (* CU1: [0x1000]=ptr-to-int, [0x1001]=proc returning 0x1000 *)
  let _ = Pdb.Type_merge.merge_types c (cu_records 0x1000) in
  (* CU2: prepend an unrelated record so the pointer lands at 0x1001 and
     the proc at 0x1002, referencing 0x1001. The pointer + proc should
     still dedup against CU1; only the unrelated record is new. *)
  let cu2 =
    Pdb.Codeview_types.Modifier { modified_type = ti 0x0074; modifiers = 1 }
    :: [
         Pdb.Codeview_types.Pointer
           {
             pointee_type = ti 0x0074;
             attrs = Unsigned.UInt32.of_int 0x1000C;
           };
         Pdb.Codeview_types.Procedure
           {
             return_type = ti 0x1001;
             calling_conv = Pdb.Codeview_constants.NearC;
             options = 0;
             param_count = 0;
             arg_list = ti 0x0000;
           };
       ]
  in
  let remap2 = Pdb.Type_merge.merge_types c cu2 in
  let merged = Pdb.Type_merge.cross_types c in
  (* CU1 produced 2 records; CU2 adds only the Modifier. *)
  Alcotest.(check int) "3 unique records" 3 (List.length merged);
  (* CU2's pointer (local idx 1) and proc (local idx 2) map back to CU1's
     merged indices 0x1000 and 0x1001. *)
  Alcotest.(check int) "CU2 pointer dedups" 0x1000 (id_int remap2.(1));
  Alcotest.(check int) "CU2 proc dedups" 0x1001 (id_int remap2.(2))

(** IDs merge with both type-ref and id-ref distinction: a StringId chain
    plus a FuncId referencing a type. *)
let test_cross_ids () =
  let c = Pdb.Type_merge.create_cross () in
  (* One CU of types: a procedure at local 0x1000. *)
  let type_remap =
    Pdb.Type_merge.merge_types c
      [
        Pdb.Codeview_types.Procedure
          {
            return_type = ti 0x0003;
            calling_conv = Pdb.Codeview_constants.NearC;
            options = 0;
            param_count = 0;
            arg_list = ti 0x0000;
          };
      ]
  in
  (* IDs: a StringId then a FuncId referencing local type 0x1000 and id 0. *)
  let ids =
    [
      Pdb.Codeview_types.StringId { id = ti 0x0000; str = "main.c" };
      Pdb.Codeview_types.FuncId
        { scope_id = ti 0x0000; func_type = ti 0x1000; name = "main" };
    ]
  in
  let id_remap = Pdb.Type_merge.merge_ids c ~type_remap ids in
  let merged_ids = Pdb.Type_merge.cross_ids c in
  Alcotest.(check int) "2 id records" 2 (List.length merged_ids);
  (* The merged FuncId's func_type points at the merged procedure. *)
  match List.nth merged_ids 1 with
  | Pdb.Codeview_types.FuncId { func_type; _ } ->
      Alcotest.(check int) "func_type remapped to merged proc"
        (id_int type_remap.(0)) (id_int func_type);
      Alcotest.(check int) "funcid assigned id index" 0x1001
        (id_int id_remap.(1))
  | _ -> Alcotest.fail "expected FuncId"

let () =
  Alcotest.run "Type Merge"
    [
      ( "dedup",
        [
          Alcotest.test_case "distinct records" `Quick test_distinct_records;
          Alcotest.test_case "duplicate records" `Quick test_duplicate_records;
          Alcotest.test_case "duplicate pointer" `Quick test_duplicate_pointer;
          Alcotest.test_case "similar but different" `Quick
            test_similar_but_different;
          Alcotest.test_case "find_index" `Quick test_find_index;
          Alcotest.test_case "records in order" `Quick test_records_in_order;
          Alcotest.test_case "many duplicates" `Quick test_many_duplicates;
        ] );
      ( "map_type_indices",
        [
          Alcotest.test_case "distinguishes type and id refs" `Quick
            test_map_distinguishes_type_and_id;
          Alcotest.test_case "pointer and arglist" `Quick
            test_map_pointer_and_arglist;
        ] );
      ( "cross_unit",
        [
          Alcotest.test_case "identical units collapse" `Quick
            test_cross_identical_units;
          Alcotest.test_case "distinct units stay distinct" `Quick
            test_cross_distinct_units;
          Alcotest.test_case "renumbered refs dedup" `Quick
            test_cross_renumbered_refs;
          Alcotest.test_case "ids merge with ref distinction" `Quick
            test_cross_ids;
        ] );
    ]
