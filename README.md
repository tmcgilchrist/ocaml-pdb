# ocaml-pdb

An OCaml library for reading and writing Microsoft PDB (Program Database)
files and the CodeView debugging records they contain.

The library is a work in progress and not feature-complete. See **Status**
below for what is and isn't covered.

## Motivation

This project started as my way of understanding Microsoft's PDB and CodeView
formats for debugging information. Based on existing experience with DWARF
on Unix systems, what is required to read and write valid debugging
information on Windows such that debuggers like WinDbg work.

Potentially this work will find it's way into the OCaml compiler.

## Status

What works today:

- MSF (Multi-Stream File) container -- read and write.
- PDB Info stream (Stream 1) -- version, GUID, age, named-stream map,
  feature flags.
- TPI and IPI streams -- most CodeView leaf kinds covered.
- DBI stream -- module list, section contributions, FileInfo source files,
  Optional Debug Header, EC substream, machine type.
- CodeView symbol records -- ~30 symbol kinds plus an `Unknown` fallback.
- C13 debug subsections -- Lines, FileChecksums, InlineeLines, StringTable,
  FrameData, CrossModuleExports, CrossModuleImports, Unknown.
- GSI/PSI hash tables -- public and global symbol indices.
- A `Pdb_builder` high-level API that assembles a complete PDB file from
  structured inputs (types, symbols, modules, source files).
- Windows SEH unwind tables for x86-64 and ARM64 (`Unwind` module), the
  PE/COFF analogue of DWARF CFI.
- An `pdbdump` example tool, mirroring durin's `dwarfdump`.

Known gaps:

- The Free Page Map is written correctly for files small enough to fit in
  a single FPM block (one FPM block covers `block_size * 8` blocks, i.e.
  128 MB at 4 KB block size). Larger PDBs would need multiple FPM blocks
  at `block_size` intervals; this writer does not yet produce them.
- Cross-compilation-unit type merging is implemented by re-serialisation
  (LLVM's `MergingTypeTableBuilder` approach): references are remapped onto
  a shared numbering and identical records deduplicated. The alternative
  content-hash scheme (LLVM's truncated-BLAKE3 `GlobalTypeTableBuilder`,
  needed to emit COFF `.debug$H` sections) is not implemented.
- Many symbol and type variants exist primarily as a parser fallback
  (`Unknown`); only the kinds we've needed have hand-written fields.
- The OMAP address-translation tables, type-server records, and the FPO
  data subsection are not implemented.

## Design

- **Reader / writer split.** Each stream has a read module (`Tpi`,
  `Dbi`) and a paired `_write` module (`Tpi_write`, `Dbi_write`).
- **Cursor-based reads, buffer-based writes.** Readers consume
  `Object.Buffer.cursor` (from the `object` library), writers accumulate
  into `Stdlib.Buffer.t`.
- **Lazy iteration.** Type records, symbol records, module lists, and
  debug subsections are exposed as `Seq.t` so large streams don't
  materialise eagerly.
- **No cyclic data.** TypeIndex references stay as `u32`. A resolver
  function looks records up by index; the type record itself never holds
  a pointer to another record.
- **Unsigned arithmetic everywhere.** The `integers` library's
  `Unsigned.UInt32`/`UInt16` are used uniformly for sizes, offsets, and
  type indices.
- **Architecture is a parameter, not a separate library.** Windows
  unwind info uses one of two on-disk formats depending on architecture
  (x86-64 SEH vs ARM64 SEH). The `Unwind` module exposes both via a
  common interface.

## Dependencies

- `object`: multi-format object-file library that provides the PE/COFF
  reader, the `Object.Buffer` abstraction, and a few cross-format
  utilities.
- `integers`: for `Unsigned.UInt32`/`UInt16` types that aren't in OCaml's stdlib.
- `alcotest`, `qcheck-core`, `qcheck-alcotest` -- testing.
- LLVM 15+ (`llvm-pdbutil`) at test time. It is used to validate written PDBs
  and to convert LLVM YAML fixtures into reference PDBs. Later versions of
  LLVM provide better PDB support

## Building and testing

```sh
dune build
dune runtest
```

Tests are organised in three layers:

- `test/unit/` -- alcotest + qcheck unit tests for each module, including
  property-based roundtrip tests.
- `test/integration/` -- end-to-end tests that build PDBs via
  `Pdb_builder`, validate them with `llvm-pdbutil`, and parse LLVM-built
  PDBs back through our reader.
- `test/cram/` -- cram tests for the `pdbdump` example.

The integration suite includes a "LLVM equivalence" harness that, for each
of LLVM's bundled YAML test fixtures, builds an equivalent PDB using our
writer and asserts that `llvm-pdbutil dump` output matches text-for-text.
The LLVM YAML files are not copied into this repo -- they are read live
from a local checkout of `llvm-project`, located via the
`LLVM_PROJECT_DIR` environment variable. Tests skip cleanly when
`llvm-pdbutil` is not on PATH or the LLVM tree cannot be found.

## Example

```ocaml
let () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let no_type = Pdb.Type_index.of_u32 Unsigned.UInt32.zero in
  let proc =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         { return_type = Pdb.Type_index.int32;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 0;
           arg_list = no_type })
  in
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.FuncId
         { scope_id = no_type;
           func_type = proc;
           name = "main" })
  in
  Pdb.Pdb_builder.add_module b
    { name = "main.obj";
      obj_file = "main.obj";
      symbols = [];
      subsections = [];
      section_contrib = None;
      source_files = [ "main.ml" ] };
  print_string (Pdb.Pdb_builder.finalize b)
```

The `pdbdump` example reads a PDB and dumps human-readable output similar
to `llvm-pdbutil dump`:

```sh
dune exec example/pdbdump.exe -- --summary --types path/to/file.pdb
```

## Reference implementations and acknowledgements

The PDB and CodeView formats have no formal specification. Everything in
this library was derived by reading existing implementations and
cross-checking their behaviour against each other. Where one
implementation was particularly load-bearing for a given subsystem it is
called out below. This project owes a great deal to all of them.

- **LLVM** ([`llvm/lib/DebugInfo/PDB`](https://github.com/llvm/llvm-project/tree/main/llvm/lib/DebugInfo/PDB),
  [`llvm/lib/DebugInfo/CodeView`](https://github.com/llvm/llvm-project/tree/main/llvm/lib/DebugInfo/CodeView),
  [docs](https://llvm.org/docs/PDB/index.html), Apache 2.0 WITH
  LLVM-exception) is the primary reference. It is the most complete
  open-source PDB reader and writer, and `llvm-pdbutil` is the
  ground-truth oracle used throughout this project's test suite. The PDB
  V1 hash and CRC32-based V8 hash in `lib/hash.ml` are ports of LLVM's
  `Hash.cpp`. A small C reference implementation of those hashes from
  LLVM is kept verbatim in `test/oracle/hash_oracle.c` (with the
  original Apache-2.0 WITH LLVM-exception header preserved) and used as
  a differential oracle.

- **getsentry/pdb** ([Rust](https://github.com/getsentry/pdb), Apache
  2.0 / MIT) is the most mature standalone PDB reader. Its lazy parsing
  approach, similar in style to `gimli` for DWARF, informed the shape of
  our `Seq.t`-based iteration.

- **microsoft/pdb** ([C++](https://github.com/microsoft/microsoft-pdb))
  is Microsoft's reference dump of the PDB sources; it is unbuildable in
  isolation but is the closest thing to authoritative documentation for
  the on-disk layout.

- **MolecularMatters/raw_pdb** ([C++11](https://github.com/MolecularMatters/raw_pdb),
  BSD-2-Clause) is a zero-copy PDB reader. Useful as a performance and
  layout reference, and for cross-checking corner cases in MSF page
  reassembly.

- **The LLVM PDB test fixtures**
  ([`llvm/test/DebugInfo/PDB/Inputs`](https://github.com/llvm/llvm-project/tree/main/llvm/test/DebugInfo/PDB/Inputs))
  are used live by the LLVM-equivalence harness -- they are not copied
  into this repo; the test driver reads them out of a local LLVM
  checkout.

If you spot a misstatement about any of these projects, or an omission,
please open an issue.

## Related work

- **object** -- the underlying object-file library that provides PE/COFF
  parsing and the buffer abstraction we read against.
