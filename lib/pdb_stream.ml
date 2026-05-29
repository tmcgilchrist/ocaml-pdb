(** PDB Info Stream (Stream 1) reader.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/InfoStream.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h (InfoStreamHeader)
    - LLVM docs: https://llvm.org/docs/PDB/PdbStream.html *)

open Pdb_types

type pdb_version = VC70 | VC80 | VC110 | VC140 | Unknown of int
type feature = ContainsIdStream | NoTypeMerging | MinimalDebugInfo

type t = {
  version : pdb_version;
  signature : u32;
  age : u32;
  guid : guid;
  named_streams : Named_stream_map.t;
  features : feature list;
}

let pdb_version_to_int = function
  | VC70 -> 20000404
  | VC80 -> 20030901
  | VC110 -> 20091201
  | VC140 -> 20140508
  | Unknown v -> v

let int_to_pdb_version = function
  | 20000404 -> VC70
  | 20030901 -> VC80
  | 20091201 -> VC110
  | 20140508 -> VC140
  | v -> Unknown v

let string_of_pdb_version = function
  | VC70 -> "VC70"
  | VC80 -> "VC80"
  | VC110 -> "VC110"
  | VC140 -> "VC140"
  | Unknown v -> Printf.sprintf "Unknown(%d)" v

(* Feature signature constants *)
let feature_vc110 = 20091201
let feature_vc140 = 20140508
let feature_no_type_merge = 0x4D544F4E (* "NOTM" *)
let feature_minimal_debug_info = 0x494E494D (* "MINI" *)

let read_guid cur =
  let data1 = Object.Buffer.Read.u32 cur in
  let data2 = Object.Buffer.Read.u16 cur in
  let data3 = Object.Buffer.Read.u16 cur in
  let data4 = Object.Buffer.Read.fixed_string cur 8 in
  { data1; data2; data3; data4 }

let parse cur =
  (* InfoStreamHeader fixed prefix is 28 bytes: Version + Signature + Age
     (3 u32) + GUID (u32 + 2 u16 + 8 bytes). *)
  Object.Buffer.ensure cur 28 "PDB info stream: truncated header";
  let version_raw = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let version = int_to_pdb_version version_raw in
  let signature = Object.Buffer.Read.u32 cur in
  let age = Object.Buffer.Read.u32 cur in
  let guid = read_guid cur in
  (* Named stream map *)
  let named_streams = Named_stream_map.parse cur in
  (* Feature signatures: remaining u32 values until end of stream.
     We read as many as available. VC110 stops further reading. *)
  let features = ref [] in
  let stop = ref false in
  while (not !stop) && not (Object.Buffer.at_end cur) do
    let sig_val = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
    match sig_val with
    | v when v = feature_vc110 ->
        features := ContainsIdStream :: !features;
        stop := true
    | v when v = feature_vc140 -> features := ContainsIdStream :: !features
    | v when v = feature_no_type_merge -> features := NoTypeMerging :: !features
    | v when v = feature_minimal_debug_info ->
        features := MinimalDebugInfo :: !features
    | _ -> ()
  done;
  {
    version;
    signature;
    age;
    guid;
    named_streams;
    features = List.rev !features;
  }
