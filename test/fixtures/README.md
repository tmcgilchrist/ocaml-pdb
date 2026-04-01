# Test Fixtures

## simple.pdb

Generated from `simple.yaml` using:

```sh
llvm-pdbutil yaml2pdb simple.yaml --pdb=simple.pdb
```

Contains:
- TPI: 7 type records (arglist, procedure, two structures for `Point` with
  forward ref + complete, two field lists, enum `Color`)
- IPI: 2 records (LF_FUNC_ID for `main`, LF_STRING_ID for `simple.c`)
- DBI: 1 module (`simple.obj`)

Regenerate when the YAML changes. Requires LLVM 18+.
