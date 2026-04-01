(** PDB hash functions.

    Ported from LLVM's llvm/lib/DebugInfo/PDB/Native/Hash.cpp which
    corresponds to [Hasher::lhashPbCb] in PDB/include/misc.h. *)

let hash_string_v1 (str : string) : int =
  let len = String.length str in
  let result = ref 0l in
  (* XOR full 32-bit little-endian words *)
  let num_longs = len / 4 in
  for i = 0 to num_longs - 1 do
    let off = i * 4 in
    let v =
      Int32.logor
        (Int32.logor
           (Int32.of_int (Char.code str.[off]))
           (Int32.shift_left (Int32.of_int (Char.code str.[off + 1])) 8))
        (Int32.logor
           (Int32.shift_left (Int32.of_int (Char.code str.[off + 2])) 16)
           (Int32.shift_left (Int32.of_int (Char.code str.[off + 3])) 24))
    in
    result := Int32.logxor !result v
  done;
  let remainder_off = num_longs * 4 in
  let remainder_size = len mod 4 in
  (* Hash a 2-byte word if possible *)
  let remainder_off, remainder_size =
    if remainder_size >= 2 then begin
      let v =
        Int32.logor
          (Int32.of_int (Char.code str.[remainder_off]))
          (Int32.shift_left (Int32.of_int (Char.code str.[remainder_off + 1])) 8)
      in
      result := Int32.logxor !result v;
      (remainder_off + 2, remainder_size - 2)
    end
    else (remainder_off, remainder_size)
  in
  (* Hash possible odd byte *)
  if remainder_size = 1 then
    result := Int32.logxor !result (Int32.of_int (Char.code str.[remainder_off]));
  let to_lower_mask = 0x20202020l in
  result := Int32.logor !result to_lower_mask;
  result := Int32.logxor !result (Int32.shift_right_logical !result 11);
  result := Int32.logxor !result (Int32.shift_right_logical !result 16);
  Int32.to_int !result land 0xFFFFFFFF
