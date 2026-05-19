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
    ]
