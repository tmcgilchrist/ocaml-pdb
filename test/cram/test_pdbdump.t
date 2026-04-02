Test pdbdump basic functionality against simple.pdb fixture

  $ PDB=../fixtures/simple.pdb

Test help flag:
  $ pdbdump --help=plain
  NAME
         pdbdump - Dump PDB file contents
  
  SYNOPSIS
         pdbdump [OPTION]… FILE
  
  ARGUMENTS
         FILE (required)
             Path to PDB file.
  
  OPTIONS
         --all
             Dump everything.
  
         --dbi
             Dump DBI stream (modules, section contributions).
  
         --ids
             Dump IPI id records.
  
         --pdb-stream
             Dump PDB info stream (GUID, age, version).
  
         --summary
             Print MSF summary (stream count, sizes).
  
         --symbols
             Dump symbol records from all modules.
  
         --types
             Dump TPI type records.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         pdbdump exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
















Test MSF summary:
  $ pdbdump --summary $PDB
  MSF Summary:
    Block size:    4096
    Block count:   12
    Stream count:  9
    Stream  0:     0 bytes
    Stream  1:     93 bytes
    Stream  2:     252 bytes
    Stream  3:     207 bytes
    Stream  4:     96 bytes
    Stream  5:     0 bytes
    Stream  6:     8 bytes
    Stream  7:     25 bytes
    Stream  8:     8 bytes
  

Test PDB info stream:
  $ pdbdump --pdb-stream $PDB
  PDB Info Stream:
    Version:    VC70
    Signature:  0x00000000
    Age:        1
    GUID:       {00000000-0000-0000-FFFFFFFFFFFFFF7F}
    Named Streams:
      /names                         -> Stream 7
      /LinkInfo                      -> Stream 5
    Features:
      ContainsIdStream
  

Test TPI type records:
  $ pdbdump --types $PDB
  TPI Stream (7 records, index range 0x1000-0x1007):
    0x1000: LF_ARGLIST (0 args)
    0x1001: LF_PROCEDURE
    0x1002: LF_STRUCTURE "Point"
    0x1003: LF_FIELDLIST (2 members)
    0x1004: LF_STRUCTURE "Point"
    0x1005: LF_FIELDLIST (3 members)
    0x1006: LF_ENUM "Color"
  

Test IPI id records:
  $ pdbdump --ids $PDB
  IPI Stream (2 records, index range 0x1000-0x1002):
    0x1000: LF_FUNC_ID "main"
    0x1001: LF_STRING_ID "simple.c"
  

Test DBI stream:
  $ pdbdump --dbi $PDB
  DBI Stream:
    Machine:       0x014C
    Age:           1
    Modules:       1
    Section Contribs: 0
  
    Module 0:
      Name:        simple.obj
      Obj:         simple.obj
      Sym Stream:  65535
      Sym Bytes:   0
      C13 Bytes:   0
  


Test error handling with non-existent file:
  $ pdbdump nonexistent.pdb 2>&1
  pdbdump: internal error, uncaught exception:
           Unix.Unix_error(Unix.ENOENT, "open", "nonexistent.pdb")
           
  [125]


Test missing file argument:
  $ pdbdump 2>&1
  Usage: pdbdump [--help] [OPTION]… FILE
  pdbdump: required argument FILE is missing
  [124]
