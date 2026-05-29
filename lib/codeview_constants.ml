(** CodeView constant definitions.

    Enum types for leaf kinds (type records), symbol kinds, simple type kinds,
    calling conventions, pointer attributes, and member access flags.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/CodeView.h
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/CodeViewTypes.def
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/CodeViewSymbols.def
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/TypeIndex.h *)

(** {2 Type Leaf Kinds}

    Identifies the kind of a CodeView type record in the TPI or IPI stream. *)

type leaf_kind =
  (* Core type records *)
  | LF_MODIFIER
  | LF_POINTER
  | LF_PROCEDURE
  | LF_MFUNCTION
  | LF_LABEL
  | LF_ARGLIST
  | LF_FIELDLIST
  | LF_ARRAY
  | LF_CLASS
  | LF_STRUCTURE
  | LF_INTERFACE
  | LF_UNION
  | LF_ENUM
  | LF_TYPESERVER2
  | LF_VFTABLE
  | LF_VTSHAPE
  | LF_BITFIELD
  | LF_METHODLIST
  | LF_PRECOMP
  | LF_ENDPRECOMP
  (* Member records (inside LF_FIELDLIST) *)
  | LF_BCLASS
  | LF_BINTERFACE
  | LF_VBCLASS
  | LF_IVBCLASS
  | LF_VFUNCTAB
  | LF_STMEMBER
  | LF_METHOD
  | LF_MEMBER
  | LF_NESTTYPE
  | LF_ONEMETHOD
  | LF_ENUMERATE
  | LF_INDEX
  (* ID records (IPI stream) *)
  | LF_FUNC_ID
  | LF_MFUNC_ID
  | LF_BUILDINFO
  | LF_SUBSTR_LIST
  | LF_STRING_ID
  | LF_UDT_SRC_LINE
  | LF_UDT_MOD_SRC_LINE
  (* Misc *)
  | LF_ALIAS
  | LF_DEFARG
  | LF_FRIENDFCN
  | LF_NESTTYPEEX
  | LF_MEMBERMODIFY
  | LF_MANAGED

let leaf_kind_of_int = function
  | 0x1001 -> LF_MODIFIER
  | 0x1002 -> LF_POINTER
  | 0x1008 -> LF_PROCEDURE
  | 0x1009 -> LF_MFUNCTION
  | 0x000e -> LF_LABEL
  | 0x1201 -> LF_ARGLIST
  | 0x1203 -> LF_FIELDLIST
  | 0x1503 -> LF_ARRAY
  | 0x1504 -> LF_CLASS
  | 0x1505 -> LF_STRUCTURE
  | 0x1519 -> LF_INTERFACE
  | 0x1506 -> LF_UNION
  | 0x1507 -> LF_ENUM
  | 0x1515 -> LF_TYPESERVER2
  | 0x151d -> LF_VFTABLE
  | 0x000a -> LF_VTSHAPE
  | 0x1205 -> LF_BITFIELD
  | 0x1206 -> LF_METHODLIST
  | 0x1509 -> LF_PRECOMP
  | 0x0014 -> LF_ENDPRECOMP
  | 0x1400 -> LF_BCLASS
  | 0x151a -> LF_BINTERFACE
  | 0x1401 -> LF_VBCLASS
  | 0x1402 -> LF_IVBCLASS
  | 0x1409 -> LF_VFUNCTAB
  | 0x150e -> LF_STMEMBER
  | 0x150f -> LF_METHOD
  | 0x150d -> LF_MEMBER
  | 0x1510 -> LF_NESTTYPE
  | 0x1511 -> LF_ONEMETHOD
  | 0x1502 -> LF_ENUMERATE
  | 0x1404 -> LF_INDEX
  | 0x1601 -> LF_FUNC_ID
  | 0x1602 -> LF_MFUNC_ID
  | 0x1603 -> LF_BUILDINFO
  | 0x1604 -> LF_SUBSTR_LIST
  | 0x1605 -> LF_STRING_ID
  | 0x1606 -> LF_UDT_SRC_LINE
  | 0x1607 -> LF_UDT_MOD_SRC_LINE
  | 0x150a -> LF_ALIAS
  | 0x150b -> LF_DEFARG
  | 0x150c -> LF_FRIENDFCN
  | 0x1512 -> LF_NESTTYPEEX
  | 0x1513 -> LF_MEMBERMODIFY
  | 0x1514 -> LF_MANAGED
  | n -> failwith (Printf.sprintf "Unknown leaf_kind: 0x%04x" n)

let int_of_leaf_kind = function
  | LF_MODIFIER -> 0x1001
  | LF_POINTER -> 0x1002
  | LF_PROCEDURE -> 0x1008
  | LF_MFUNCTION -> 0x1009
  | LF_LABEL -> 0x000e
  | LF_ARGLIST -> 0x1201
  | LF_FIELDLIST -> 0x1203
  | LF_ARRAY -> 0x1503
  | LF_CLASS -> 0x1504
  | LF_STRUCTURE -> 0x1505
  | LF_INTERFACE -> 0x1519
  | LF_UNION -> 0x1506
  | LF_ENUM -> 0x1507
  | LF_TYPESERVER2 -> 0x1515
  | LF_VFTABLE -> 0x151d
  | LF_VTSHAPE -> 0x000a
  | LF_BITFIELD -> 0x1205
  | LF_METHODLIST -> 0x1206
  | LF_PRECOMP -> 0x1509
  | LF_ENDPRECOMP -> 0x0014
  | LF_BCLASS -> 0x1400
  | LF_BINTERFACE -> 0x151a
  | LF_VBCLASS -> 0x1401
  | LF_IVBCLASS -> 0x1402
  | LF_VFUNCTAB -> 0x1409
  | LF_STMEMBER -> 0x150e
  | LF_METHOD -> 0x150f
  | LF_MEMBER -> 0x150d
  | LF_NESTTYPE -> 0x1510
  | LF_ONEMETHOD -> 0x1511
  | LF_ENUMERATE -> 0x1502
  | LF_INDEX -> 0x1404
  | LF_FUNC_ID -> 0x1601
  | LF_MFUNC_ID -> 0x1602
  | LF_BUILDINFO -> 0x1603
  | LF_SUBSTR_LIST -> 0x1604
  | LF_STRING_ID -> 0x1605
  | LF_UDT_SRC_LINE -> 0x1606
  | LF_UDT_MOD_SRC_LINE -> 0x1607
  | LF_ALIAS -> 0x150a
  | LF_DEFARG -> 0x150b
  | LF_FRIENDFCN -> 0x150c
  | LF_NESTTYPEEX -> 0x1512
  | LF_MEMBERMODIFY -> 0x1513
  | LF_MANAGED -> 0x1514

let string_of_leaf_kind = function
  | LF_MODIFIER -> "LF_MODIFIER"
  | LF_POINTER -> "LF_POINTER"
  | LF_PROCEDURE -> "LF_PROCEDURE"
  | LF_MFUNCTION -> "LF_MFUNCTION"
  | LF_LABEL -> "LF_LABEL"
  | LF_ARGLIST -> "LF_ARGLIST"
  | LF_FIELDLIST -> "LF_FIELDLIST"
  | LF_ARRAY -> "LF_ARRAY"
  | LF_CLASS -> "LF_CLASS"
  | LF_STRUCTURE -> "LF_STRUCTURE"
  | LF_INTERFACE -> "LF_INTERFACE"
  | LF_UNION -> "LF_UNION"
  | LF_ENUM -> "LF_ENUM"
  | LF_TYPESERVER2 -> "LF_TYPESERVER2"
  | LF_VFTABLE -> "LF_VFTABLE"
  | LF_VTSHAPE -> "LF_VTSHAPE"
  | LF_BITFIELD -> "LF_BITFIELD"
  | LF_METHODLIST -> "LF_METHODLIST"
  | LF_PRECOMP -> "LF_PRECOMP"
  | LF_ENDPRECOMP -> "LF_ENDPRECOMP"
  | LF_BCLASS -> "LF_BCLASS"
  | LF_BINTERFACE -> "LF_BINTERFACE"
  | LF_VBCLASS -> "LF_VBCLASS"
  | LF_IVBCLASS -> "LF_IVBCLASS"
  | LF_VFUNCTAB -> "LF_VFUNCTAB"
  | LF_STMEMBER -> "LF_STMEMBER"
  | LF_METHOD -> "LF_METHOD"
  | LF_MEMBER -> "LF_MEMBER"
  | LF_NESTTYPE -> "LF_NESTTYPE"
  | LF_ONEMETHOD -> "LF_ONEMETHOD"
  | LF_ENUMERATE -> "LF_ENUMERATE"
  | LF_INDEX -> "LF_INDEX"
  | LF_FUNC_ID -> "LF_FUNC_ID"
  | LF_MFUNC_ID -> "LF_MFUNC_ID"
  | LF_BUILDINFO -> "LF_BUILDINFO"
  | LF_SUBSTR_LIST -> "LF_SUBSTR_LIST"
  | LF_STRING_ID -> "LF_STRING_ID"
  | LF_UDT_SRC_LINE -> "LF_UDT_SRC_LINE"
  | LF_UDT_MOD_SRC_LINE -> "LF_UDT_MOD_SRC_LINE"
  | LF_ALIAS -> "LF_ALIAS"
  | LF_DEFARG -> "LF_DEFARG"
  | LF_FRIENDFCN -> "LF_FRIENDFCN"
  | LF_NESTTYPEEX -> "LF_NESTTYPEEX"
  | LF_MEMBERMODIFY -> "LF_MEMBERMODIFY"
  | LF_MANAGED -> "LF_MANAGED"

(** {2 Symbol Kinds}

    Identifies the kind of a CodeView symbol record. *)

type symbol_kind =
  | S_END
  | S_INLINESITE_END
  | S_PROC_ID_END
  | S_THUNK32
  | S_TRAMPOLINE
  | S_SECTION
  | S_COFFGROUP
  | S_EXPORT
  | S_LPROC32
  | S_GPROC32
  | S_LPROC32_ID
  | S_GPROC32_ID
  | S_LPROC32_DPC
  | S_LPROC32_DPC_ID
  | S_REGISTER
  | S_PUB32
  | S_PROCREF
  | S_LPROCREF
  | S_ENVBLOCK
  | S_INLINESITE
  | S_LOCAL
  | S_DEFRANGE
  | S_DEFRANGE_SUBFIELD
  | S_DEFRANGE_REGISTER
  | S_DEFRANGE_FRAMEPOINTER_REL
  | S_DEFRANGE_SUBFIELD_REGISTER
  | S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE
  | S_DEFRANGE_REGISTER_REL
  | S_BLOCK32
  | S_LABEL32
  | S_OBJNAME
  | S_COMPILE2
  | S_COMPILE3
  | S_FRAMEPROC
  | S_CALLSITEINFO
  | S_FILESTATIC
  | S_HEAPALLOCSITE
  | S_FRAMECOOKIE
  | S_CALLEES
  | S_CALLERS
  | S_UDT
  | S_COBOLUDT
  | S_BUILDINFO
  | S_BPREL32
  | S_REGREL32
  | S_CONSTANT
  | S_MANCONSTANT
  | S_LDATA32
  | S_GDATA32
  | S_LMANDATA
  | S_GMANDATA
  | S_LTHREAD32
  | S_GTHREAD32
  | S_UNAMESPACE
  | S_ANNOTATION
  | S_INLINEES
  | S_SEPCODE

let symbol_kind_of_int = function
  | 0x0006 -> S_END
  | 0x114e -> S_INLINESITE_END
  | 0x114f -> S_PROC_ID_END
  | 0x1102 -> S_THUNK32
  | 0x112c -> S_TRAMPOLINE
  | 0x1136 -> S_SECTION
  | 0x1137 -> S_COFFGROUP
  | 0x1138 -> S_EXPORT
  | 0x110f -> S_LPROC32
  | 0x1110 -> S_GPROC32
  | 0x1146 -> S_LPROC32_ID
  | 0x1147 -> S_GPROC32_ID
  | 0x1155 -> S_LPROC32_DPC
  | 0x1156 -> S_LPROC32_DPC_ID
  | 0x1106 -> S_REGISTER
  | 0x110e -> S_PUB32
  | 0x1125 -> S_PROCREF
  | 0x1127 -> S_LPROCREF
  | 0x113d -> S_ENVBLOCK
  | 0x114d -> S_INLINESITE
  | 0x113e -> S_LOCAL
  | 0x113f -> S_DEFRANGE
  | 0x1140 -> S_DEFRANGE_SUBFIELD
  | 0x1141 -> S_DEFRANGE_REGISTER
  | 0x1142 -> S_DEFRANGE_FRAMEPOINTER_REL
  | 0x1143 -> S_DEFRANGE_SUBFIELD_REGISTER
  | 0x1144 -> S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE
  | 0x1145 -> S_DEFRANGE_REGISTER_REL
  | 0x1103 -> S_BLOCK32
  | 0x1105 -> S_LABEL32
  | 0x1101 -> S_OBJNAME
  | 0x1116 -> S_COMPILE2
  | 0x113c -> S_COMPILE3
  | 0x1012 -> S_FRAMEPROC
  | 0x1139 -> S_CALLSITEINFO
  | 0x1153 -> S_FILESTATIC
  | 0x115e -> S_HEAPALLOCSITE
  | 0x113a -> S_FRAMECOOKIE
  | 0x115a -> S_CALLEES
  | 0x115b -> S_CALLERS
  | 0x1108 -> S_UDT
  | 0x1109 -> S_COBOLUDT
  | 0x114c -> S_BUILDINFO
  | 0x110b -> S_BPREL32
  | 0x1111 -> S_REGREL32
  | 0x1107 -> S_CONSTANT
  | 0x112d -> S_MANCONSTANT
  | 0x110c -> S_LDATA32
  | 0x110d -> S_GDATA32
  | 0x111c -> S_LMANDATA
  | 0x111d -> S_GMANDATA
  | 0x1112 -> S_LTHREAD32
  | 0x1113 -> S_GTHREAD32
  | 0x1124 -> S_UNAMESPACE
  | 0x1019 -> S_ANNOTATION
  | 0x1168 -> S_INLINEES
  | 0x1132 -> S_SEPCODE
  | n -> failwith (Printf.sprintf "Unknown symbol_kind: 0x%04x" n)

let int_of_symbol_kind = function
  | S_END -> 0x0006
  | S_INLINESITE_END -> 0x114e
  | S_PROC_ID_END -> 0x114f
  | S_THUNK32 -> 0x1102
  | S_TRAMPOLINE -> 0x112c
  | S_SECTION -> 0x1136
  | S_COFFGROUP -> 0x1137
  | S_EXPORT -> 0x1138
  | S_LPROC32 -> 0x110f
  | S_GPROC32 -> 0x1110
  | S_LPROC32_ID -> 0x1146
  | S_GPROC32_ID -> 0x1147
  | S_LPROC32_DPC -> 0x1155
  | S_LPROC32_DPC_ID -> 0x1156
  | S_REGISTER -> 0x1106
  | S_PUB32 -> 0x110e
  | S_PROCREF -> 0x1125
  | S_LPROCREF -> 0x1127
  | S_ENVBLOCK -> 0x113d
  | S_INLINESITE -> 0x114d
  | S_LOCAL -> 0x113e
  | S_DEFRANGE -> 0x113f
  | S_DEFRANGE_SUBFIELD -> 0x1140
  | S_DEFRANGE_REGISTER -> 0x1141
  | S_DEFRANGE_FRAMEPOINTER_REL -> 0x1142
  | S_DEFRANGE_SUBFIELD_REGISTER -> 0x1143
  | S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE -> 0x1144
  | S_DEFRANGE_REGISTER_REL -> 0x1145
  | S_BLOCK32 -> 0x1103
  | S_LABEL32 -> 0x1105
  | S_OBJNAME -> 0x1101
  | S_COMPILE2 -> 0x1116
  | S_COMPILE3 -> 0x113c
  | S_FRAMEPROC -> 0x1012
  | S_CALLSITEINFO -> 0x1139
  | S_FILESTATIC -> 0x1153
  | S_HEAPALLOCSITE -> 0x115e
  | S_FRAMECOOKIE -> 0x113a
  | S_CALLEES -> 0x115a
  | S_CALLERS -> 0x115b
  | S_UDT -> 0x1108
  | S_COBOLUDT -> 0x1109
  | S_BUILDINFO -> 0x114c
  | S_BPREL32 -> 0x110b
  | S_REGREL32 -> 0x1111
  | S_CONSTANT -> 0x1107
  | S_MANCONSTANT -> 0x112d
  | S_LDATA32 -> 0x110c
  | S_GDATA32 -> 0x110d
  | S_LMANDATA -> 0x111c
  | S_GMANDATA -> 0x111d
  | S_LTHREAD32 -> 0x1112
  | S_GTHREAD32 -> 0x1113
  | S_UNAMESPACE -> 0x1124
  | S_ANNOTATION -> 0x1019
  | S_INLINEES -> 0x1168
  | S_SEPCODE -> 0x1132

let string_of_symbol_kind = function
  | S_END -> "S_END"
  | S_INLINESITE_END -> "S_INLINESITE_END"
  | S_PROC_ID_END -> "S_PROC_ID_END"
  | S_THUNK32 -> "S_THUNK32"
  | S_TRAMPOLINE -> "S_TRAMPOLINE"
  | S_SECTION -> "S_SECTION"
  | S_COFFGROUP -> "S_COFFGROUP"
  | S_EXPORT -> "S_EXPORT"
  | S_LPROC32 -> "S_LPROC32"
  | S_GPROC32 -> "S_GPROC32"
  | S_LPROC32_ID -> "S_LPROC32_ID"
  | S_GPROC32_ID -> "S_GPROC32_ID"
  | S_LPROC32_DPC -> "S_LPROC32_DPC"
  | S_LPROC32_DPC_ID -> "S_LPROC32_DPC_ID"
  | S_REGISTER -> "S_REGISTER"
  | S_PUB32 -> "S_PUB32"
  | S_PROCREF -> "S_PROCREF"
  | S_LPROCREF -> "S_LPROCREF"
  | S_ENVBLOCK -> "S_ENVBLOCK"
  | S_INLINESITE -> "S_INLINESITE"
  | S_LOCAL -> "S_LOCAL"
  | S_DEFRANGE -> "S_DEFRANGE"
  | S_DEFRANGE_SUBFIELD -> "S_DEFRANGE_SUBFIELD"
  | S_DEFRANGE_REGISTER -> "S_DEFRANGE_REGISTER"
  | S_DEFRANGE_FRAMEPOINTER_REL -> "S_DEFRANGE_FRAMEPOINTER_REL"
  | S_DEFRANGE_SUBFIELD_REGISTER -> "S_DEFRANGE_SUBFIELD_REGISTER"
  | S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE ->
      "S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE"
  | S_DEFRANGE_REGISTER_REL -> "S_DEFRANGE_REGISTER_REL"
  | S_BLOCK32 -> "S_BLOCK32"
  | S_LABEL32 -> "S_LABEL32"
  | S_OBJNAME -> "S_OBJNAME"
  | S_COMPILE2 -> "S_COMPILE2"
  | S_COMPILE3 -> "S_COMPILE3"
  | S_FRAMEPROC -> "S_FRAMEPROC"
  | S_CALLSITEINFO -> "S_CALLSITEINFO"
  | S_FILESTATIC -> "S_FILESTATIC"
  | S_HEAPALLOCSITE -> "S_HEAPALLOCSITE"
  | S_FRAMECOOKIE -> "S_FRAMECOOKIE"
  | S_CALLEES -> "S_CALLEES"
  | S_CALLERS -> "S_CALLERS"
  | S_UDT -> "S_UDT"
  | S_COBOLUDT -> "S_COBOLUDT"
  | S_BUILDINFO -> "S_BUILDINFO"
  | S_BPREL32 -> "S_BPREL32"
  | S_REGREL32 -> "S_REGREL32"
  | S_CONSTANT -> "S_CONSTANT"
  | S_MANCONSTANT -> "S_MANCONSTANT"
  | S_LDATA32 -> "S_LDATA32"
  | S_GDATA32 -> "S_GDATA32"
  | S_LMANDATA -> "S_LMANDATA"
  | S_GMANDATA -> "S_GMANDATA"
  | S_LTHREAD32 -> "S_LTHREAD32"
  | S_GTHREAD32 -> "S_GTHREAD32"
  | S_UNAMESPACE -> "S_UNAMESPACE"
  | S_ANNOTATION -> "S_ANNOTATION"
  | S_INLINEES -> "S_INLINEES"
  | S_SEPCODE -> "S_SEPCODE"

(** {2 Simple Type Kind}

    Built-in type indices below 0x1000. The low byte is the type kind and the
    high byte is the pointer mode. *)

type simple_type_kind =
  | None
  | Void
  | NotTranslated
  | HResult
  | SignedCharacter
  | UnsignedCharacter
  | NarrowCharacter
  | WideCharacter
  | Character16
  | Character32
  | Character8
  | SByte
  | Byte
  | Int16Short
  | UInt16Short
  | Int16
  | UInt16
  | Int32Long
  | UInt32Long
  | Int32
  | UInt32
  | Int64Quad
  | UInt64Quad
  | Int64
  | UInt64
  | Int128Oct
  | UInt128Oct
  | Int128
  | UInt128
  | Float16
  | Float32
  | Float32PartialPrecision
  | Float48
  | Float64
  | Float80
  | Float128
  | Complex16
  | Complex32
  | Complex32PartialPrecision
  | Complex48
  | Complex64
  | Complex80
  | Complex128
  | Boolean8
  | Boolean16
  | Boolean32
  | Boolean64
  | Boolean128

let simple_type_kind_of_int = function
  | 0x0000 -> None
  | 0x0003 -> Void
  | 0x0007 -> NotTranslated
  | 0x0008 -> HResult
  | 0x0010 -> SignedCharacter
  | 0x0020 -> UnsignedCharacter
  | 0x0070 -> NarrowCharacter
  | 0x0071 -> WideCharacter
  | 0x007a -> Character16
  | 0x007b -> Character32
  | 0x007c -> Character8
  | 0x0068 -> SByte
  | 0x0069 -> Byte
  | 0x0011 -> Int16Short
  | 0x0021 -> UInt16Short
  | 0x0072 -> Int16
  | 0x0073 -> UInt16
  | 0x0012 -> Int32Long
  | 0x0022 -> UInt32Long
  | 0x0074 -> Int32
  | 0x0075 -> UInt32
  | 0x0013 -> Int64Quad
  | 0x0023 -> UInt64Quad
  | 0x0076 -> Int64
  | 0x0077 -> UInt64
  | 0x0014 -> Int128Oct
  | 0x0024 -> UInt128Oct
  | 0x0078 -> Int128
  | 0x0079 -> UInt128
  | 0x0046 -> Float16
  | 0x0040 -> Float32
  | 0x0045 -> Float32PartialPrecision
  | 0x0044 -> Float48
  | 0x0041 -> Float64
  | 0x0042 -> Float80
  | 0x0043 -> Float128
  | 0x0056 -> Complex16
  | 0x0050 -> Complex32
  | 0x0055 -> Complex32PartialPrecision
  | 0x0054 -> Complex48
  | 0x0051 -> Complex64
  | 0x0052 -> Complex80
  | 0x0053 -> Complex128
  | 0x0030 -> Boolean8
  | 0x0031 -> Boolean16
  | 0x0032 -> Boolean32
  | 0x0033 -> Boolean64
  | 0x0034 -> Boolean128
  | n -> failwith (Printf.sprintf "Unknown simple_type_kind: 0x%04x" n)

let int_of_simple_type_kind = function
  | None -> 0x0000
  | Void -> 0x0003
  | NotTranslated -> 0x0007
  | HResult -> 0x0008
  | SignedCharacter -> 0x0010
  | UnsignedCharacter -> 0x0020
  | NarrowCharacter -> 0x0070
  | WideCharacter -> 0x0071
  | Character16 -> 0x007a
  | Character32 -> 0x007b
  | Character8 -> 0x007c
  | SByte -> 0x0068
  | Byte -> 0x0069
  | Int16Short -> 0x0011
  | UInt16Short -> 0x0021
  | Int16 -> 0x0072
  | UInt16 -> 0x0073
  | Int32Long -> 0x0012
  | UInt32Long -> 0x0022
  | Int32 -> 0x0074
  | UInt32 -> 0x0075
  | Int64Quad -> 0x0013
  | UInt64Quad -> 0x0023
  | Int64 -> 0x0076
  | UInt64 -> 0x0077
  | Int128Oct -> 0x0014
  | UInt128Oct -> 0x0024
  | Int128 -> 0x0078
  | UInt128 -> 0x0079
  | Float16 -> 0x0046
  | Float32 -> 0x0040
  | Float32PartialPrecision -> 0x0045
  | Float48 -> 0x0044
  | Float64 -> 0x0041
  | Float80 -> 0x0042
  | Float128 -> 0x0043
  | Complex16 -> 0x0056
  | Complex32 -> 0x0050
  | Complex32PartialPrecision -> 0x0055
  | Complex48 -> 0x0054
  | Complex64 -> 0x0051
  | Complex80 -> 0x0052
  | Complex128 -> 0x0053
  | Boolean8 -> 0x0030
  | Boolean16 -> 0x0031
  | Boolean32 -> 0x0032
  | Boolean64 -> 0x0033
  | Boolean128 -> 0x0034

let string_of_simple_type_kind = function
  | None -> "None"
  | Void -> "Void"
  | NotTranslated -> "NotTranslated"
  | HResult -> "HResult"
  | SignedCharacter -> "SignedCharacter"
  | UnsignedCharacter -> "UnsignedCharacter"
  | NarrowCharacter -> "NarrowCharacter"
  | WideCharacter -> "WideCharacter"
  | Character16 -> "Character16"
  | Character32 -> "Character32"
  | Character8 -> "Character8"
  | SByte -> "SByte"
  | Byte -> "Byte"
  | Int16Short -> "Int16Short"
  | UInt16Short -> "UInt16Short"
  | Int16 -> "Int16"
  | UInt16 -> "UInt16"
  | Int32Long -> "Int32Long"
  | UInt32Long -> "UInt32Long"
  | Int32 -> "Int32"
  | UInt32 -> "UInt32"
  | Int64Quad -> "Int64Quad"
  | UInt64Quad -> "UInt64Quad"
  | Int64 -> "Int64"
  | UInt64 -> "UInt64"
  | Int128Oct -> "Int128Oct"
  | UInt128Oct -> "UInt128Oct"
  | Int128 -> "Int128"
  | UInt128 -> "UInt128"
  | Float16 -> "Float16"
  | Float32 -> "Float32"
  | Float32PartialPrecision -> "Float32PartialPrecision"
  | Float48 -> "Float48"
  | Float64 -> "Float64"
  | Float80 -> "Float80"
  | Float128 -> "Float128"
  | Complex16 -> "Complex16"
  | Complex32 -> "Complex32"
  | Complex32PartialPrecision -> "Complex32PartialPrecision"
  | Complex48 -> "Complex48"
  | Complex64 -> "Complex64"
  | Complex80 -> "Complex80"
  | Complex128 -> "Complex128"
  | Boolean8 -> "Boolean8"
  | Boolean16 -> "Boolean16"
  | Boolean32 -> "Boolean32"
  | Boolean64 -> "Boolean64"
  | Boolean128 -> "Boolean128"

(** {2 Simple Type Mode}

    The pointer mode encoded in the high byte of a simple type index. *)

type simple_type_mode =
  | Direct
  | NearPointer
  | FarPointer
  | HugePointer
  | NearPointer32
  | FarPointer32
  | NearPointer64
  | NearPointer128

let simple_type_mode_of_int = function
  | 0x00 -> Direct
  | 0x01 -> NearPointer
  | 0x02 -> FarPointer
  | 0x03 -> HugePointer
  | 0x04 -> NearPointer32
  | 0x05 -> FarPointer32
  | 0x06 -> NearPointer64
  | 0x07 -> NearPointer128
  | n -> failwith (Printf.sprintf "Unknown simple_type_mode: 0x%02x" n)

let int_of_simple_type_mode = function
  | Direct -> 0x00
  | NearPointer -> 0x01
  | FarPointer -> 0x02
  | HugePointer -> 0x03
  | NearPointer32 -> 0x04
  | FarPointer32 -> 0x05
  | NearPointer64 -> 0x06
  | NearPointer128 -> 0x07

let string_of_simple_type_mode = function
  | Direct -> "Direct"
  | NearPointer -> "NearPointer"
  | FarPointer -> "FarPointer"
  | HugePointer -> "HugePointer"
  | NearPointer32 -> "NearPointer32"
  | FarPointer32 -> "FarPointer32"
  | NearPointer64 -> "NearPointer64"
  | NearPointer128 -> "NearPointer128"

(** {2 Calling Convention} *)

type calling_convention =
  | NearC
  | FarC
  | NearPascal
  | FarPascal
  | NearFast
  | FarFast
  | NearStdCall
  | FarStdCall
  | NearSysCall
  | FarSysCall
  | ThisCall
  | MipsCall
  | Generic
  | AlphaCall
  | PpcCall
  | SHCall
  | ArmCall
  | AM33Call
  | TriCall
  | SH5Call
  | M32RCall
  | ClrCall
  | Inline
  | NearVector

let calling_convention_of_int = function
  | 0x00 -> NearC
  | 0x01 -> FarC
  | 0x02 -> NearPascal
  | 0x03 -> FarPascal
  | 0x04 -> NearFast
  | 0x05 -> FarFast
  | 0x07 -> NearStdCall
  | 0x08 -> FarStdCall
  | 0x09 -> NearSysCall
  | 0x0a -> FarSysCall
  | 0x0b -> ThisCall
  | 0x0c -> MipsCall
  | 0x0d -> Generic
  | 0x0e -> AlphaCall
  | 0x0f -> PpcCall
  | 0x10 -> SHCall
  | 0x11 -> ArmCall
  | 0x12 -> AM33Call
  | 0x13 -> TriCall
  | 0x14 -> SH5Call
  | 0x15 -> M32RCall
  | 0x16 -> ClrCall
  | 0x17 -> Inline
  | 0x18 -> NearVector
  | n -> failwith (Printf.sprintf "Unknown calling_convention: 0x%02x" n)

let int_of_calling_convention = function
  | NearC -> 0x00
  | FarC -> 0x01
  | NearPascal -> 0x02
  | FarPascal -> 0x03
  | NearFast -> 0x04
  | FarFast -> 0x05
  | NearStdCall -> 0x07
  | FarStdCall -> 0x08
  | NearSysCall -> 0x09
  | FarSysCall -> 0x0a
  | ThisCall -> 0x0b
  | MipsCall -> 0x0c
  | Generic -> 0x0d
  | AlphaCall -> 0x0e
  | PpcCall -> 0x0f
  | SHCall -> 0x10
  | ArmCall -> 0x11
  | AM33Call -> 0x12
  | TriCall -> 0x13
  | SH5Call -> 0x14
  | M32RCall -> 0x15
  | ClrCall -> 0x16
  | Inline -> 0x17
  | NearVector -> 0x18

let string_of_calling_convention = function
  | NearC -> "NearC"
  | FarC -> "FarC"
  | NearPascal -> "NearPascal"
  | FarPascal -> "FarPascal"
  | NearFast -> "NearFast"
  | FarFast -> "FarFast"
  | NearStdCall -> "NearStdCall"
  | FarStdCall -> "FarStdCall"
  | NearSysCall -> "NearSysCall"
  | FarSysCall -> "FarSysCall"
  | ThisCall -> "ThisCall"
  | MipsCall -> "MipsCall"
  | Generic -> "Generic"
  | AlphaCall -> "AlphaCall"
  | PpcCall -> "PpcCall"
  | SHCall -> "SHCall"
  | ArmCall -> "ArmCall"
  | AM33Call -> "AM33Call"
  | TriCall -> "TriCall"
  | SH5Call -> "SH5Call"
  | M32RCall -> "M32RCall"
  | ClrCall -> "ClrCall"
  | Inline -> "Inline"
  | NearVector -> "NearVector"

(** {2 Pointer Kind} *)

type pointer_kind =
  | Near16
  | Far16
  | Huge16
  | BasedOnSegment
  | BasedOnValue
  | BasedOnSegmentValue
  | BasedOnAddress
  | BasedOnSegmentAddress
  | BasedOnType
  | BasedOnSelf
  | Near32
  | Far32
  | Near64

let pointer_kind_of_int = function
  | 0x00 -> Near16
  | 0x01 -> Far16
  | 0x02 -> Huge16
  | 0x03 -> BasedOnSegment
  | 0x04 -> BasedOnValue
  | 0x05 -> BasedOnSegmentValue
  | 0x06 -> BasedOnAddress
  | 0x07 -> BasedOnSegmentAddress
  | 0x08 -> BasedOnType
  | 0x09 -> BasedOnSelf
  | 0x0a -> Near32
  | 0x0b -> Far32
  | 0x0c -> Near64
  | n -> failwith (Printf.sprintf "Unknown pointer_kind: 0x%02x" n)

let int_of_pointer_kind = function
  | Near16 -> 0x00
  | Far16 -> 0x01
  | Huge16 -> 0x02
  | BasedOnSegment -> 0x03
  | BasedOnValue -> 0x04
  | BasedOnSegmentValue -> 0x05
  | BasedOnAddress -> 0x06
  | BasedOnSegmentAddress -> 0x07
  | BasedOnType -> 0x08
  | BasedOnSelf -> 0x09
  | Near32 -> 0x0a
  | Far32 -> 0x0b
  | Near64 -> 0x0c

let string_of_pointer_kind = function
  | Near16 -> "Near16"
  | Far16 -> "Far16"
  | Huge16 -> "Huge16"
  | BasedOnSegment -> "BasedOnSegment"
  | BasedOnValue -> "BasedOnValue"
  | BasedOnSegmentValue -> "BasedOnSegmentValue"
  | BasedOnAddress -> "BasedOnAddress"
  | BasedOnSegmentAddress -> "BasedOnSegmentAddress"
  | BasedOnType -> "BasedOnType"
  | BasedOnSelf -> "BasedOnSelf"
  | Near32 -> "Near32"
  | Far32 -> "Far32"
  | Near64 -> "Near64"

(** {2 Pointer Mode} *)

type pointer_mode =
  | Pointer
  | LValueReference
  | PointerToDataMember
  | PointerToMemberFunction
  | RValueReference

let pointer_mode_of_int = function
  | 0x00 -> Pointer
  | 0x01 -> LValueReference
  | 0x02 -> PointerToDataMember
  | 0x03 -> PointerToMemberFunction
  | 0x04 -> RValueReference
  | n -> failwith (Printf.sprintf "Unknown pointer_mode: 0x%02x" n)

let int_of_pointer_mode = function
  | Pointer -> 0x00
  | LValueReference -> 0x01
  | PointerToDataMember -> 0x02
  | PointerToMemberFunction -> 0x03
  | RValueReference -> 0x04

let string_of_pointer_mode = function
  | Pointer -> "Pointer"
  | LValueReference -> "LValueReference"
  | PointerToDataMember -> "PointerToDataMember"
  | PointerToMemberFunction -> "PointerToMemberFunction"
  | RValueReference -> "RValueReference"

(** {2 Pointer Attributes (LF_POINTER's "attrs" field)}

    Bit layout in the u32: bits 0-4 PointerKind (5 bits) bits 5-7 PointerMode (3
    bits) bits 8-12 PointerOptions (5 bits, flags) bits 13-18 PointerSize (6
    bits, in bytes) *)

let make_pointer_attrs ?(mode = Pointer) ?(flags = 0) kind ~size =
  let kind_bits = int_of_pointer_kind kind in
  let mode_bits = int_of_pointer_mode mode in
  kind_bits land 0x1F
  lor ((mode_bits land 0x07) lsl 5)
  lor ((flags land 0x1F) lsl 8)
  lor ((size land 0x3F) lsl 13)

(** Common pointer attribute values. A "standard" 32-bit C pointer has kind =
    Near32 and size = 4 bytes; a 64-bit one has Near64 and 8. *)
let near32_pointer_attrs = make_pointer_attrs Near32 ~size:4
(** [near32_pointer_attrs] = 0x800A *)

(** [near64_pointer_attrs] = 0x1000C *)
let near64_pointer_attrs = make_pointer_attrs Near64 ~size:8

(** {2 Member Access} *)

type member_access = NoAccess | Private | Protected | Public

let member_access_of_int = function
  | 0 -> NoAccess
  | 1 -> Private
  | 2 -> Protected
  | 3 -> Public
  | n -> failwith (Printf.sprintf "Unknown member_access: %d" n)

let int_of_member_access = function
  | NoAccess -> 0
  | Private -> 1
  | Protected -> 2
  | Public -> 3

let string_of_member_access = function
  | NoAccess -> "NoAccess"
  | Private -> "Private"
  | Protected -> "Protected"
  | Public -> "Public"

(** {2 Method Kind} *)

type method_kind =
  | Vanilla
  | Virtual
  | Static
  | Friend
  | IntroducingVirtual
  | PureVirtual
  | PureIntroducingVirtual

let method_kind_of_int = function
  | 0x00 -> Vanilla
  | 0x01 -> Virtual
  | 0x02 -> Static
  | 0x03 -> Friend
  | 0x04 -> IntroducingVirtual
  | 0x05 -> PureVirtual
  | 0x06 -> PureIntroducingVirtual
  | n -> failwith (Printf.sprintf "Unknown method_kind: 0x%02x" n)

let int_of_method_kind = function
  | Vanilla -> 0x00
  | Virtual -> 0x01
  | Static -> 0x02
  | Friend -> 0x03
  | IntroducingVirtual -> 0x04
  | PureVirtual -> 0x05
  | PureIntroducingVirtual -> 0x06

let string_of_method_kind = function
  | Vanilla -> "Vanilla"
  | Virtual -> "Virtual"
  | Static -> "Static"
  | Friend -> "Friend"
  | IntroducingVirtual -> "IntroducingVirtual"
  | PureVirtual -> "PureVirtual"
  | PureIntroducingVirtual -> "PureIntroducingVirtual"

(** {2 Debug Subsection Kind} *)

type debug_subsection_kind =
  | DEBUG_S_SYMBOLS
  | DEBUG_S_LINES
  | DEBUG_S_STRINGTABLE
  | DEBUG_S_FILECHKSMS
  | DEBUG_S_FRAMEDATA
  | DEBUG_S_INLINEELINES
  | DEBUG_S_CROSSSCOPEIMPORTS
  | DEBUG_S_CROSSSCOPEEXPORTS
  | DEBUG_S_IL_LINES
  | DEBUG_S_FUNC_MDTOKEN_MAP
  | DEBUG_S_TYPE_MDTOKEN_MAP
  | DEBUG_S_MERGED_ASSEMBLYINPUT
  | DEBUG_S_COFF_SYMBOL_RVA

let debug_subsection_kind_of_int = function
  | 0xf1 -> DEBUG_S_SYMBOLS
  | 0xf2 -> DEBUG_S_LINES
  | 0xf3 -> DEBUG_S_STRINGTABLE
  | 0xf4 -> DEBUG_S_FILECHKSMS
  | 0xf5 -> DEBUG_S_FRAMEDATA
  | 0xf6 -> DEBUG_S_INLINEELINES
  | 0xf7 -> DEBUG_S_CROSSSCOPEIMPORTS
  | 0xf8 -> DEBUG_S_CROSSSCOPEEXPORTS
  | 0xf9 -> DEBUG_S_IL_LINES
  | 0xfa -> DEBUG_S_FUNC_MDTOKEN_MAP
  | 0xfb -> DEBUG_S_TYPE_MDTOKEN_MAP
  | 0xfc -> DEBUG_S_MERGED_ASSEMBLYINPUT
  | 0xfd -> DEBUG_S_COFF_SYMBOL_RVA
  | n -> failwith (Printf.sprintf "Unknown debug_subsection_kind: 0x%02x" n)

let int_of_debug_subsection_kind = function
  | DEBUG_S_SYMBOLS -> 0xf1
  | DEBUG_S_LINES -> 0xf2
  | DEBUG_S_STRINGTABLE -> 0xf3
  | DEBUG_S_FILECHKSMS -> 0xf4
  | DEBUG_S_FRAMEDATA -> 0xf5
  | DEBUG_S_INLINEELINES -> 0xf6
  | DEBUG_S_CROSSSCOPEIMPORTS -> 0xf7
  | DEBUG_S_CROSSSCOPEEXPORTS -> 0xf8
  | DEBUG_S_IL_LINES -> 0xf9
  | DEBUG_S_FUNC_MDTOKEN_MAP -> 0xfa
  | DEBUG_S_TYPE_MDTOKEN_MAP -> 0xfb
  | DEBUG_S_MERGED_ASSEMBLYINPUT -> 0xfc
  | DEBUG_S_COFF_SYMBOL_RVA -> 0xfd

let string_of_debug_subsection_kind = function
  | DEBUG_S_SYMBOLS -> "DEBUG_S_SYMBOLS"
  | DEBUG_S_LINES -> "DEBUG_S_LINES"
  | DEBUG_S_STRINGTABLE -> "DEBUG_S_STRINGTABLE"
  | DEBUG_S_FILECHKSMS -> "DEBUG_S_FILECHKSMS"
  | DEBUG_S_FRAMEDATA -> "DEBUG_S_FRAMEDATA"
  | DEBUG_S_INLINEELINES -> "DEBUG_S_INLINEELINES"
  | DEBUG_S_CROSSSCOPEIMPORTS -> "DEBUG_S_CROSSSCOPEIMPORTS"
  | DEBUG_S_CROSSSCOPEEXPORTS -> "DEBUG_S_CROSSSCOPEEXPORTS"
  | DEBUG_S_IL_LINES -> "DEBUG_S_IL_LINES"
  | DEBUG_S_FUNC_MDTOKEN_MAP -> "DEBUG_S_FUNC_MDTOKEN_MAP"
  | DEBUG_S_TYPE_MDTOKEN_MAP -> "DEBUG_S_TYPE_MDTOKEN_MAP"
  | DEBUG_S_MERGED_ASSEMBLYINPUT -> "DEBUG_S_MERGED_ASSEMBLYINPUT"
  | DEBUG_S_COFF_SYMBOL_RVA -> "DEBUG_S_COFF_SYMBOL_RVA"
