/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Compression Framework
 *
 * General Description:
 *
 * The PMIX Compress framework has been created to provide an abstract interface
 * to the compression agent library on the host machine. This framework is useful
 * when distributing files that can be compressed before sending to dimish the
 * load on the network.
 *
 */

#ifndef PMIX_MCA_COMPRESS_H
#define PMIX_MCA_COMPRESS_H

#include "pmix_config.h"
#include "src/class/pmix_object.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Module initialization function.
 * Returns PMIX_SUCCESS
 */
typedef int (*pmix_compress_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 * Returns PMIX_SUCCESS
 */
typedef int (*pmix_compress_base_module_finalize_fn_t)(void);

/**
 * Compress a string
 *
 * Arguments:
 *
 */
typedef bool (*pmix_compress_base_module_compress_string_fn_t)(char *instring, uint8_t **outbytes,
                                                               size_t *nbytes);
typedef bool (*pmix_compress_base_module_decompress_string_fn_t)(char **outstring, uint8_t *inbytes,
                                                                 size_t len);
typedef size_t (*pmix_compress_base_module_get_decompressed_strlen_fn_t)(const pmix_byte_object_t *bo);

/**
 * Compress a block
 *
 * Arguments:
 *
 */
typedef bool (*pmix_compress_base_module_compress_fn_t)(const uint8_t *inbytes, size_t size,
                                                        uint8_t **outbytes, size_t *nbytes);

typedef bool (*pmix_compress_base_module_decompress_fn_t)(uint8_t **outbytes, size_t *outlen,
                                                          const uint8_t *inbytes, size_t len);
typedef size_t (*pmix_compress_base_module_get_decompressed_size_fn_t)(const pmix_byte_object_t *bo);

/**
 * Structure for COMPRESS components.
 */
typedef  pmix_mca_base_component_t pmix_compress_base_component_t;

/**
 * Structure for COMPRESS modules
 */
struct pmix_compress_base_module_1_0_0_t {
    /** Initialization Function */
    pmix_compress_base_module_init_fn_t                     init;
    /** Finalization Function */
    pmix_compress_base_module_finalize_fn_t                 finalize;

    /** Compress interface */
    pmix_compress_base_module_compress_fn_t                 compress;

    /** Decompress Interface */
    pmix_compress_base_module_decompress_fn_t               decompress;
    pmix_compress_base_module_get_decompressed_size_fn_t    get_decompressed_size;
    /* COMPRESS STRING */
    pmix_compress_base_module_compress_string_fn_t          compress_string;
    pmix_compress_base_module_decompress_string_fn_t        decompress_string;
    pmix_compress_base_module_get_decompressed_strlen_fn_t  get_decompressed_strlen;
};
typedef struct pmix_compress_base_module_1_0_0_t pmix_compress_base_module_1_0_0_t;
typedef struct pmix_compress_base_module_1_0_0_t pmix_compress_base_module_t;

PMIX_EXPORT extern pmix_compress_base_module_t pmix_compress;

/**
 * Macro for use in components that are of type COMPRESS
 */
#define PMIX_COMPRESS_BASE_VERSION_2_0_0 PMIX_MCA_BASE_VERSION_1_0_0("pcompress", 2, 0, 0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* PMIX_COMPRESS_H */
