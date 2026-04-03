(** Global/Public Symbol Index (GSI/PSI) writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/GSIStreamBuilder.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/GlobalsStream.h *)

module Buffer = Stdlib.Buffer

type symbol_entry = {
  name : string;
  sym_offset : int;
}

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

(* Number of hash buckets, must match IPHR_HASH in LLVM *)
let iphr_hash = 4096

(* GSI header constants *)
let gsi_hdr_signature = 0xFFFFFFFF
let gsi_hdr_version = 0xF12F091A (* 0xeffe0000 + 19990810 *)

(* Size of HROffsetCalc struct used for bucket offset calculation *)
let size_of_hr_offset_calc = 12

(** Case-insensitive comparison matching the PDB reference implementation.
    Shorter strings sort before longer strings. For equal-length ASCII strings,
    compare case-insensitively. *)
let gsi_record_cmp s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  if l1 <> l2 then compare l1 l2
  else String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)

let write_gsi (buf : Buffer.t) (entries : symbol_entry list) : unit =
  let num_entries = List.length entries in
  if num_entries = 0 then begin
    (* Write empty GSI: header + empty bitmap + no buckets *)
    write_u32_le buf gsi_hdr_signature;
    write_u32_le buf gsi_hdr_version;
    write_u32_le buf 0; (* HrSize = 0 *)
    let bitmap_words = (iphr_hash + 32) / 32 in
    let bitmap_bytes = bitmap_words * 4 in
    write_u32_le buf bitmap_bytes; (* NumBuckets = bitmap size only *)
    for _ = 1 to bitmap_words do
      write_u32_le buf 0
    done
  end
  else begin
    (* Assign each entry to a bucket using hash_string_v1 *)
    let bucketed =
      List.map
        (fun e ->
          let bucket = Hash.hash_string_v1 e.name mod iphr_hash in
          let bucket = if bucket < 0 then bucket + iphr_hash else bucket in
          (bucket, e))
        entries
    in
    (* Count entries per bucket *)
    let bucket_counts = Array.make iphr_hash 0 in
    List.iter (fun (b, _) -> bucket_counts.(b) <- bucket_counts.(b) + 1) bucketed;
    (* Exclusive prefix sum for bucket start offsets *)
    let bucket_starts = Array.make iphr_hash 0 in
    let sum = ref 0 in
    for i = 0 to iphr_hash - 1 do
      bucket_starts.(i) <- !sum;
      sum := !sum + bucket_counts.(i)
    done;
    (* Place entries into hash records array in bucket order *)
    let hash_records = Array.make num_entries (0, 0) in (* (record_index, sym_offset) *)
    let bucket_cursors = Array.copy bucket_starts in
    List.iteri
      (fun idx (b, e) ->
        let pos = bucket_cursors.(b) in
        bucket_cursors.(b) <- pos + 1;
        hash_records.(pos) <- (idx, e.sym_offset))
      bucketed;
    (* Sort within each bucket by name (case-insensitive), then by offset *)
    let entries_arr = Array.of_list entries in
    for i = 0 to iphr_hash - 1 do
      let start = bucket_starts.(i) in
      let stop = bucket_cursors.(i) in
      if stop - start > 1 then begin
        let sub = Array.sub hash_records start (stop - start) in
        Array.sort
          (fun (idx1, off1) (idx2, off2) ->
            let c =
              gsi_record_cmp entries_arr.(idx1).name entries_arr.(idx2).name
            in
            if c <> 0 then c else compare off1 off2)
          sub;
        Array.blit sub 0 hash_records start (stop - start)
      end
    done;
    (* Replace record indices with symbol offsets + 1 (per GSI1::fixSymRecs) *)
    let final_records =
      Array.map (fun (_idx, sym_offset) -> (sym_offset + 1, 1)) hash_records
    in
    (* Build bitmap and bucket offset list *)
    let bitmap_words = (iphr_hash + 32) / 32 in
    let bitmap = Array.make bitmap_words 0 in
    let bucket_offsets = ref [] in
    for i = 0 to iphr_hash - 1 do
      if bucket_starts.(i) < bucket_cursors.(i) then begin
        let word_idx = i / 32 in
        let bit_idx = i mod 32 in
        bitmap.(word_idx) <- bitmap.(word_idx) lor (1 lsl bit_idx);
        bucket_offsets :=
          (bucket_starts.(i) * size_of_hr_offset_calc) :: !bucket_offsets
      end
    done;
    let bucket_offsets = List.rev !bucket_offsets in
    (* Write GSI header *)
    let hr_size = num_entries * 8 in (* 8 bytes per PSHashRecord *)
    let num_buckets_field =
      (bitmap_words * 4) + (List.length bucket_offsets * 4)
    in
    write_u32_le buf gsi_hdr_signature;
    write_u32_le buf gsi_hdr_version;
    write_u32_le buf hr_size;
    write_u32_le buf num_buckets_field;
    (* Write hash records *)
    Array.iter
      (fun (off, cref) ->
        write_u32_le buf off;
        write_u32_le buf cref)
      final_records;
    (* Write bitmap *)
    Array.iter (write_u32_le buf) bitmap;
    (* Write bucket offsets *)
    List.iter (write_u32_le buf) bucket_offsets
  end

(** Extract the name from a public symbol record. *)
let pub_name = function
  | Codeview_symbols.Pub32 { name; _ } -> name
  | _ -> ""

let write_publics_stream (buf : Buffer.t)
    (symbols : Codeview_symbols.symbol_record list) : unit =
  (* Serialize all symbol records to get offsets *)
  let sym_buf = Buffer.create 256 in
  let sym_entries =
    List.map
      (fun sym ->
        let offset = Buffer.length sym_buf in
        Codeview_symbols.write_symbol_record sym_buf sym;
        let name = pub_name sym in
        { name; sym_offset = offset })
      symbols
  in
  let sym_bytes = Buffer.length sym_buf in
  (* Build GSI hash *)
  let gsi_buf = Buffer.create 256 in
  write_gsi gsi_buf sym_entries;
  let gsi_bytes = Buffer.length gsi_buf in
  (* Build address map: sorted by (segment, offset) *)
  let addr_map_entries =
    List.filter_map
      (fun (i, sym) ->
        match sym with
        | Codeview_symbols.Pub32 { offset; segment; _ } ->
            Some (i, Unsigned.UInt32.to_int offset, segment)
        | _ -> Option.None)
      (List.mapi (fun i s -> (i, s)) symbols)
  in
  let sorted_addr =
    List.sort
      (fun (_, off1, seg1) (_, off2, seg2) ->
        let c = compare seg1 seg2 in
        if c <> 0 then c else compare off1 off2)
      addr_map_entries
  in
  let addr_map_size = List.length sorted_addr * 4 in
  (* Write publics header *)
  write_u32_le buf gsi_bytes; (* SymHash *)
  write_u32_le buf addr_map_size; (* AddrMap *)
  write_u32_le buf 0; (* NumThunks *)
  write_u32_le buf 0; (* SizeOfThunk *)
  write_u16_le buf 0; (* ISectThunkTable *)
  write_u16_le buf 0; (* padding *)
  write_u32_le buf 0; (* OffThunkTable *)
  write_u32_le buf 0; (* NumSections *)
  (* Write GSI hash *)
  Buffer.add_string buf (Buffer.contents gsi_buf);
  (* Write address map *)
  List.iter (fun (idx, _, _) -> write_u32_le buf idx) sorted_addr;
  (* The symbol records themselves go in a separate stream *)
  ignore sym_bytes;
  ignore sym_buf

type gsi_streams = {
  sym_record_stream : string;
  globals_stream : string;
  publics_stream : string;
}

(** Extract symbol name for GSI hashing *)
let global_name = function
  | Codeview_symbols.GData32 d -> d.name
  | LData32 d -> d.name
  | GThread32 d -> d.name
  | LThread32 d -> d.name
  | GProc32 p -> p.name
  | LProc32 p -> p.name
  | GProc32Id p -> p.name
  | LProc32Id p -> p.name
  | Constant { name; _ } -> name
  | Udt { name; _ } -> name
  | _ -> ""

let build_gsi_streams ~(publics : Codeview_symbols.symbol_record list)
    ~(globals : Codeview_symbols.symbol_record list) : gsi_streams =
  (* Build the symbol record stream: publics first, then globals *)
  let sym_buf = Buffer.create 512 in
  (* Serialize publics and collect entries for the publics hash *)
  let pub_entries =
    List.map
      (fun sym ->
        let offset = Buffer.length sym_buf in
        Codeview_symbols.write_symbol_record sym_buf sym;
        let name = pub_name sym in
        { name; sym_offset = offset })
      publics
  in
  let pub_record_bytes = Buffer.length sym_buf in
  (* Serialize globals and collect entries for the globals hash *)
  let global_entries =
    List.map
      (fun sym ->
        let offset = Buffer.length sym_buf in
        Codeview_symbols.write_symbol_record sym_buf sym;
        let name = global_name sym in
        { name; sym_offset = offset })
      globals
  in
  let sym_record_stream = Buffer.contents sym_buf in
  (* Build publics hash stream *)
  let pub_hash_buf = Buffer.create 256 in
  let gsi_buf = Buffer.create 256 in
  write_gsi gsi_buf pub_entries;
  let gsi_bytes = Buffer.length gsi_buf in
  (* Address map: sorted by (segment, offset) *)
  let addr_map_entries =
    List.filter_map
      (fun (i, sym) ->
        match sym with
        | Codeview_symbols.Pub32 { offset; segment; _ } ->
            Some (i, Unsigned.UInt32.to_int offset, segment)
        | _ -> Option.None)
      (List.mapi (fun i s -> (i, s)) publics)
  in
  let sorted_addr =
    List.sort
      (fun (_, off1, seg1) (_, off2, seg2) ->
        let c = compare seg1 seg2 in
        if c <> 0 then c else compare off1 off2)
      addr_map_entries
  in
  let addr_map_size = List.length sorted_addr * 4 in
  (* Publics header *)
  write_u32_le pub_hash_buf gsi_bytes;
  write_u32_le pub_hash_buf addr_map_size;
  write_u32_le pub_hash_buf 0;
  write_u32_le pub_hash_buf 0;
  write_u16_le pub_hash_buf 0;
  write_u16_le pub_hash_buf 0;
  write_u32_le pub_hash_buf 0;
  write_u32_le pub_hash_buf 0;
  Buffer.add_string pub_hash_buf (Buffer.contents gsi_buf);
  List.iter (fun (idx, _, _) -> write_u32_le pub_hash_buf idx) sorted_addr;
  let publics_stream = Buffer.contents pub_hash_buf in
  (* Build globals hash stream.
     Global symbol offsets are already correct -- they point into the
     combined symbol record stream where publics come first. *)
  let _ = pub_record_bytes in
  let global_hash_buf = Buffer.create 256 in
  write_gsi global_hash_buf global_entries;
  let globals_stream = Buffer.contents global_hash_buf in
  { sym_record_stream; globals_stream; publics_stream }
