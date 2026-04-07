/*===-- hash_oracle.c - PDB hash reference implementation -----------------===*\
|*                                                                            *|
|* Derived from the LLVM Project, under the Apache License v2.0 with LLVM    *|
|* Exceptions. See https://llvm.org/LICENSE.txt for license information.      *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                   *|
|*                                                                            *|
|* Source files:                                                              *|
|*   llvm/lib/DebugInfo/PDB/Native/Hash.cpp (hashStringV1, hashBufferV8)     *|
|*   https://github.com/llvm/llvm-project/blob/main/llvm/lib/DebugInfo/PDB/Native/Hash.cpp
|*                                                                            *|
|* Used for differential testing of the OCaml PDB library's hash functions    *|
|* against the LLVM reference implementation.                                 *|
|*                                                                            *|
|* Build: cc -DSTANDALONE -o hash_oracle hash_oracle.c                        *|
\*===---------------------------------------------------------------------===*/

#include <stdint.h>
#include <string.h>
#include <stdio.h>

/* hashStringV1: Ported from llvm/lib/DebugInfo/PDB/Native/Hash.cpp
   Original: llvm::pdb::hashStringV1(StringRef Str)
   Corresponds to Hasher::lhashPbCb in PDB/include/misc.h */
uint32_t hashStringV1(const char *str, uint32_t size) {
    uint32_t result = 0;

    /* XOR full 32-bit little-endian words */
    uint32_t num_longs = size / 4;
    for (uint32_t i = 0; i < num_longs; i++) {
        uint32_t v;
        memcpy(&v, str + i * 4, 4);
        result ^= v;
    }

    const uint8_t *remainder = (const uint8_t *)str + num_longs * 4;
    uint32_t remainder_size = size % 4;

    if (remainder_size >= 2) {
        uint16_t v;
        memcpy(&v, remainder, 2);
        result ^= (uint32_t)v;
        remainder += 2;
        remainder_size -= 2;
    }

    if (remainder_size == 1) {
        result ^= *remainder;
    }

    const uint32_t to_lower_mask = 0x20202020;
    result |= to_lower_mask;
    result ^= (result >> 11);
    result ^= (result >> 16);
    return result;
}

/* hashBufferV8 / JamCRC: CRC32 with init=0
   Ported from llvm/lib/DebugInfo/PDB/Native/Hash.cpp
   Original: llvm::pdb::hashBufferV8(ArrayRef<uint8_t> Buf)
   Uses llvm::JamCRC (llvm/include/llvm/Support/CRC.h) internally */
uint32_t hashBufferV8(const uint8_t *buf, uint32_t size) {
    static uint32_t table[256];
    static int table_init = 0;
    if (!table_init) {
        for (uint32_t i = 0; i < 256; i++) {
            uint32_t crc = i;
            for (int j = 0; j < 8; j++) {
                if (crc & 1) crc = (crc >> 1) ^ 0xEDB88320;
                else crc >>= 1;
            }
            table[i] = crc;
        }
        table_init = 1;
    }

    uint32_t crc = 0;
    for (uint32_t i = 0; i < size; i++) {
        crc = (crc >> 8) ^ table[(crc ^ buf[i]) & 0xFF];
    }
    return crc;
}

#ifdef STANDALONE
/* Standalone test mode: reads hex-encoded bytes from stdin, prints hashes */
int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: hash_oracle <v1|v8> <string>\n");
        return 1;
    }
    const char *mode = argv[1];
    const char *input = argv[2];
    uint32_t len = strlen(input);

    if (strcmp(mode, "v1") == 0) {
        printf("%u\n", hashStringV1(input, len));
    } else if (strcmp(mode, "v8") == 0) {
        printf("%u\n", hashBufferV8((const uint8_t *)input, len));
    } else {
        fprintf(stderr, "Unknown mode: %s\n", mode);
        return 1;
    }
    return 0;
}
#endif
