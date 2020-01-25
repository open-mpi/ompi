/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
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
 * to the compression agent library on the host machine. This fromework is useful
 * when distributing files that can be compressed before sending to dimish the
 * load on the network.
 *
 */

#ifndef PMIX_MCA_COMPRESS_H
#define PMIX_MCA_COMPRESS_H

#include "pmix_config.h"
#include "src/mca/mca.h"
#include "src/mca/base/base.h"
#include "src/class/pmix_object.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Module initialization function.
 * Returns PMIX_SUCCESS
 */
typedef int (*pmix_compress_base_module_init_fn_t)
     (void);

/**
 * Module finalization function.
 * Returns PMIX_SUCCESS
 */
typedef int (*pmix_compress_base_module_finalize_fn_t)
     (void);

/**
 * Compress the file provided
 *
 * Arguments:
 *   fname   = Filename to compress
 *   cname   = Compressed filename
 *   postfix = postfix added to filename to create compressed filename
 * Returns:
 *   PMIX_SUCCESS on success, ow PMIX_ERROR
 */
typedef int (*pmix_compress_base_module_compress_fn_t)
    (char * fname, char **cname, char **postfix);

typedef int (*pmix_compress_base_module_compress_nb_fn_t)
    (char * fname, char **cname, char **postfix, pid_t *child_pid);

/**
 * Decompress the file provided
 *
 * Arguments:
 *   fname = Filename to compress
 *   cname = Compressed filename
 * Returns:
 *   PMIX_SUCCESS on success, ow PMIX_ERROR
 */
typedef int (*pmix_compress_base_module_decompress_fn_t)
    (char * cname, char **fname);
typedef int (*pmix_compress_base_module_decompress_nb_fn_t)
    (char * cname, char **fname, pid_t *child_pid);

/**
 * Compress a string
 *
 * Arguments:
 *
 */
typedef bool (*pmix_compress_base_module_compress_string_fn_t)(char *instring,
                                                               uint8_t **outbytes,
                                                               size_t *nbytes);
typedef bool (*pmix_compress_base_module_decompress_string_fn_t)(char **outstring,
                                                                 uint8_t *inbytes, size_t len);


/**
 * Structure for COMPRESS components.
 */
struct pmix_compress_base_component_2_0_0_t {
    /** PMIX_MCA base component */
    pmix_mca_base_component_t base_version;
    /** PMIX_MCA base data */
    pmix_mca_base_component_data_t base_data;

    /** Verbosity Level */
    int verbose;
    /** Output Handle for pmix_output */
    int output_handle;
    /** Default Priority */
    int priority;
};
typedef struct pmix_compress_base_component_2_0_0_t pmix_compress_base_component_2_0_0_t;
typedef struct pmix_compress_base_component_2_0_0_t pmix_compress_base_component_t;

/**
 * Structure for COMPRESS modules
 */
struct pmix_compress_base_module_1_0_0_t {
    /** Initialization Function */
    pmix_compress_base_module_init_fn_t           init;
    /** Finalization Function */
    pmix_compress_base_module_finalize_fn_t       finalize;

    /** Compress interface */
    pmix_compress_base_module_compress_fn_t       compress;
    pmix_compress_base_module_compress_nb_fn_t    compress_nb;

    /** Decompress Interface */
    pmix_compress_base_module_decompress_fn_t     decompress;
    pmix_compress_base_module_decompress_nb_fn_t  decompress_nb;

    /* COMPRESS STRING */
    pmix_compress_base_module_compress_string_fn_t      compress_string;
    pmix_compress_base_module_decompress_string_fn_t    decompress_string;
};
typedef struct pmix_compress_base_module_1_0_0_t pmix_compress_base_module_1_0_0_t;
typedef struct pmix_compress_base_module_1_0_0_t pmix_compress_base_module_t;

PMIX_EXPORT extern pmix_compress_base_module_t pmix_compress;

/**
 * Macro for use in components that are of type COMPRESS
 */
#define PMIX_COMPRESS_BASE_VERSION_2_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pcompress", 2, 0, 0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* PMIX_COMPRESS_H */

