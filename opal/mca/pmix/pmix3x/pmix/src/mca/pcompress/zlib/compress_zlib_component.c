/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "include/pmix_common.h"
#include "src/mca/pcompress/base/base.h"
#include "compress_zlib.h"

/*
 * Public string for version number
 */
const char *pmix_compress_zlib_component_version_string =
"PMIX COMPRESS zlib MCA component version " PMIX_VERSION;

/*
 * Local functionality
 */
static int compress_zlib_open(void);
static int compress_zlib_close(void);
static int compress_zlib_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
PMIX_EXPORT pmix_mca_base_component_t mca_pcompress_zlib_component = {
    /* Handle the general mca_component_t struct containing
     *  meta information about the component zlib
     */
    PMIX_COMPRESS_BASE_VERSION_2_0_0,

    /* Component name and version */
    .pmix_mca_component_name = "zlib",
    PMIX_MCA_BASE_MAKE_VERSION(component, PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                               PMIX_RELEASE_VERSION),

    /* Component open and close functions */
    .pmix_mca_open_component = compress_zlib_open,
    .pmix_mca_close_component = compress_zlib_close,
    .pmix_mca_query_component = compress_zlib_query
};

/*
 * Zlib module
 */
static pmix_compress_base_module_t loc_module = {
    /** Initialization Function */
    .init = pmix_compress_zlib_module_init,
    /** Finalization Function */
    .finalize = pmix_compress_zlib_module_finalize,

    /** Compress Function */
    .compress_string = pmix_compress_zlib_compress_block,

    /** Decompress Function */
    .decompress_string = pmix_compress_zlib_uncompress_block,
};

static int compress_zlib_open(void)
{
    return PMIX_SUCCESS;
}

static int compress_zlib_close(void)
{
    return PMIX_SUCCESS;
}

static int compress_zlib_query(pmix_mca_base_module_t **module, int *priority)
{
    *module   = (pmix_mca_base_module_t *)&loc_module;
    *priority = 50;

    return PMIX_SUCCESS;
}

