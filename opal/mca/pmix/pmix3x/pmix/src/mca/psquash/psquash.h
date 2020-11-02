/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is for the encoding/decoding of basic types and the
 * compression/decompression of larger blobs of data (i.e., modex).
 *
 * Available plugins may be defined at runtime via the typical MCA parameter
 * syntax.
 */

#ifndef PMIX_PSQUASH_H
#define PMIX_PSQUASH_H

#include "src/include/pmix_config.h"

#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_framework.h"

BEGIN_C_DECLS

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module
 */
typedef pmix_status_t (*pmix_psquash_base_module_init_fn_t)(void);

/**
 * Finalize the module
 */
typedef void (*pmix_psquash_base_module_finalize_fn_t)(void);

/**
 *  Maximum size of the type.
 *
 * type - Type (PMIX_SIZE, PMIX_INT to PMIX_UINT64)
 * size - size of the type
 */
typedef pmix_status_t (*pmix_psquash_get_max_size_fn_t) (pmix_data_type_t type,
                                                         size_t *size);

/**
 * Encode a basic integer type into a contiguous destination buffer.
 *
 * type     - Type of the 'src' pointer (PMIX_SIZE, PMIX_INT to PMIX_UINT64)
 * src      - pointer to a single basic integer type
 * dest     - pointer to buffer to store data
 * dst_len  - pointer to the packed size of dest, in bytes
 */

typedef pmix_status_t (*pmix_psquash_encode_int_fn_t) (pmix_data_type_t type,
                                                       void *src, void *dest,
                                                       size_t *dst_len);

/**
 * Decode a basic a contiguous destination buffer into a basic integer type.
 *
 * type     - Type of the 'dest' pointer (PMIX_SIZE, PMIX_INT to PMIX_UINT64)
 * src      - pointer to buffer where data was stored
 * src_len  - length, in bytes, of the src buffer
 * dest     - pointer to a single basic integer type
 * dst_len  - pointer to the unpacked size of dest, in bytes
 */
typedef pmix_status_t (*pmix_psquash_decode_int_fn_t) (pmix_data_type_t type,
                                                       void *src, size_t src_len,
                                                       void *dest, size_t *dst_len);

/**
 * Base structure for a PSQUASH module
 */
typedef struct {
    const char *name;
    /* flag indicating if the type is encoded within the value, otherwise, it is necessary to further pack the type with the value. */
    bool int_type_is_encoded;

    /** init/finalize */
    pmix_psquash_base_module_init_fn_t     init;
    pmix_psquash_base_module_finalize_fn_t finalize;

    pmix_psquash_get_max_size_fn_t         get_max_size;

    /** Integer compression */
    pmix_psquash_encode_int_fn_t           encode_int;
    pmix_psquash_decode_int_fn_t           decode_int;
} pmix_psquash_base_module_t;

/**
 * Base structure for a PSQUASH component
 */
struct pmix_psquash_base_component_t {
    pmix_mca_base_component_t                        base;
    pmix_mca_base_component_data_t                   data;
    int                                              priority;
};
typedef struct pmix_psquash_base_component_t pmix_psquash_base_component_t;

PMIX_EXPORT extern pmix_psquash_base_module_t pmix_psquash;

/*
 * Macro for use in components that are of type psquash
 */
#define PMIX_PSQUASH_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("psquash", 1, 0, 0)

END_C_DECLS

#endif
