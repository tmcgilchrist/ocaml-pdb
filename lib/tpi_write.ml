(** TPI/IPI stream writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/TpiStreamBuilder.cpp *)

module Buffer = Stdlib.Buffer

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

(* TPI version: V80 = 20040203 *)
let tpi_version_v80 = 20040203
let tpi_header_size = 56
let first_type_index = 0x1000

let write (buf : Buffer.t) (records : Codeview_types.type_record list) : unit =
  (* Serialize all records to compute total bytes *)
  let rec_buf = Buffer.create 1024 in
  List.iter (fun r -> Codeview_types.write_type_record rec_buf r) records;
  let type_record_bytes = Buffer.length rec_buf in
  let num_records = List.length records in
  (* Write header (56 bytes) *)
  write_u32_le buf tpi_version_v80;
  write_u32_le buf tpi_header_size;
  write_u32_le buf first_type_index;
  (* TypeIndexBegin *)
  write_u32_le buf (first_type_index + num_records);
  (* TypeIndexEnd *)
  write_u32_le buf type_record_bytes;
  write_u16_le buf 0xFFFF;
  (* HashStreamIndex: -1 = no hash stream *)
  write_u16_le buf 0xFFFF;
  (* HashAuxStreamIndex *)
  write_u32_le buf 4;
  (* HashKeySize *)
  write_u32_le buf 0x40000;
  (* NumHashBuckets *)
  (* Three EmbeddedBuf fields (offset + length), all zero *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* HashValueBuffer *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* IndexOffsetBuffer *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* HashAdjBuffer *)
  (* Write type records *)
  Buffer.add_string buf (Buffer.contents rec_buf)
