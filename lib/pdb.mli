(** Read and write Microsoft PDB (Program Database) files and the CodeView
    debugging records they contain.

    A PDB is an MSF (Multi-Stream File) container holding the debug
    information for a Windows binary: type records (TPI/IPI streams),
    symbol records, line-number tables, public/global symbol indices,
    and a handful of auxiliary streams. This library is the PDB-side
    counterpart of [durin] (DWARF read/write).

    The top-level entry points are the {!Msf} container and the various
    stream parsers ({!Pdb_stream}, {!Tpi}, {!Dbi}, {!Gsi}, ...). For
    writing, the {!Pdb_builder} module assembles a complete PDB file
    from structured inputs.

    References:
    - LLVM's [llvm/lib/DebugInfo/PDB] and [llvm/lib/DebugInfo/CodeView]
      ([https://llvm.org/docs/PDB/]).
    - Microsoft's [microsoft/microsoft-pdb] source dump
      ([https://github.com/microsoft/microsoft-pdb]). *)

module Pdb_types = Pdb_types
module Msf = Msf
module Msf_write = Msf_write
module Hash = Hash
module Named_stream_map = Named_stream_map
module Pdb_stream = Pdb_stream
module Pdb_stream_write = Pdb_stream_write
module Codeview_constants = Codeview_constants
module Type_index = Type_index
module Codeview_types = Codeview_types
module Tpi = Tpi
module Tpi_write = Tpi_write
module Codeview_symbols = Codeview_symbols
module Dbi = Dbi
module Dbi_write = Dbi_write
module Debug_subsections = Debug_subsections
module Gsi = Gsi
module Gsi_write = Gsi_write
module Pdb_string_table = Pdb_string_table
module Pdb_builder = Pdb_builder
module Type_merge = Type_merge
module Unwind = Unwind
module Omap = Omap
module Fpo = Fpo
