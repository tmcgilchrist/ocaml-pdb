(** PDB Info Stream (Stream 1) writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/InfoStreamBuilder.cpp *)

module Buffer = Stdlib.Buffer

open Binary_writer

let write_guid buf (g : Pdb_types.guid) =
  write_u32_le buf (Unsigned.UInt32.to_int g.data1);
  write_u16_le buf (Unsigned.UInt16.to_int g.data2);
  write_u16_le buf (Unsigned.UInt16.to_int g.data3);
  Buffer.add_string buf g.data4

(* Feature signature constants *)
let feature_vc110 = 20091201
let feature_vc140 = 20140508
let feature_no_type_merge = 0x4D544F4E
let feature_minimal_debug_info = 0x494E494D

let write buf (info : Pdb_stream.t) =
  (* InfoStreamHeader *)
  write_u32_le buf (Pdb_stream.pdb_version_to_int info.version);
  write_u32_le buf (Unsigned.UInt32.to_int info.signature);
  write_u32_le buf (Unsigned.UInt32.to_int info.age);
  write_guid buf info.guid;
  (* Named stream map *)
  Named_stream_map.write buf info.named_streams;
  (* Feature signatures *)
  List.iter
    (fun (feat : Pdb_stream.feature) ->
      match feat with
      | ContainsIdStream ->
          (* Check if VC110 should be used as a stop marker or VC140.
             For modern PDBs, use VC140 which allows further features. *)
          write_u32_le buf feature_vc140
      | NoTypeMerging -> write_u32_le buf feature_no_type_merge
      | MinimalDebugInfo -> write_u32_le buf feature_minimal_debug_info)
    info.features
