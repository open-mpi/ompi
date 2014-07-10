/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * shmem (shared memory backing facility) framework types, convenience macros,
 * etc.
 */

#ifndef MCA_SSHMEM_TYPES_H
#define MCA_SSHMEM_TYPES_H

#include "oshmem_config.h"

BEGIN_C_DECLS

/**
 * flag indicating the state (valid/invalid) of the sshmem data structure
 * 0x0* - reserved for non-internal flags
 */
#define MAP_SEGMENT_FLAGS_VALID 0x01

/**
 * invalid id value
 */
#define MAP_SEGMENT_SHM_INVALID         (-1)

/**
 * macro that sets all bits in flags to 0
 */
#define MAP_SEGMENT_RESET_FLAGS(ds_buf)                                      \
do {                                                                         \
    (ds_buf)->flags = 0x00;                                                  \
} while (0)

/**
 * sets valid bit in flags to 1
 */
#define MAP_SEGMENT_SET_VALID(ds_buf)                                        \
do {                                                                         \
    (ds_buf)->flags |= MAP_SEGMENT_FLAGS_VALID;                              \
} while (0)

/**
 * sets valid bit in flags to 0
 */
#define MAP_SEGMENT_INVALIDATE(ds_buf)                                       \
do {                                                                         \
    (ds_buf)->flags &= ~MAP_SEGMENT_FLAGS_VALID;                             \
} while (0)

/**
 * evaluates to 1 if the valid bit in flags is set to 1.  evaluates to 0
 * otherwise.
 */
#define MAP_SEGMENT_IS_VALID(ds_buf)                                         \
    ( (ds_buf)->flags & MAP_SEGMENT_FLAGS_VALID )

typedef uint8_t segment_flag_t;

typedef enum {
    MAP_SEGMENT_STATIC = 0,
    MAP_SEGMENT_ALLOC_MMAP,
    MAP_SEGMENT_ALLOC_SHM,
    MAP_SEGMENT_ALLOC_IBV,
    MAP_SEGMENT_ALLOC_IBV_NOSHMR, 
    MAP_SEGMENT_UNKNOWN
} segment_type_t;

/**
 * memory key
 * There are following types of keys:
 * 1. 'shared memory' keys. Memory segment must be attached before access
 *   such keys use va_base = 0, len = 0 and key != MAP_SEGMENT_SHM_INVALID
 *   va_base will be set once segment is attached. 
 * 2. empty key: len = 0, key == MAP_SEGMENT_SHM_INVALID
 * 3. generic key: Key is passed with each put/get op.
 *    use va_base = <remote vaddr>, key is stored in mkey struct:
 *    len > 0, data = &<key_blob>
 */
typedef struct sshmem_mkey {
    void* va_base;
    uint16_t len;
    union {
        void *data;
        uint64_t key;
    } u;
    void *spml_context;       /* spml module can attach internal structures here */
} sshmem_mkey_t;

typedef struct map_segment_t {
    sshmem_mkey_t  **mkeys_cache;    /* includes remote segment bases in va_base */
    sshmem_mkey_t   *mkeys;          /* includes local segment bases in va_base */
    segment_flag_t   flags;          /* enable/disable flag */
    int              seg_id;
    void*            seg_base_addr;  /* base address of the segment */
    void*            end;            /* final address of the segment */
    char             seg_name[OPAL_PATH_MAX];
    size_t           seg_size;       /* length of the segment */
    segment_type_t   type;           /* type of the segment */
} map_segment_t;

END_C_DECLS

#endif /* MCA_SSHMEM_TYPES_H */
