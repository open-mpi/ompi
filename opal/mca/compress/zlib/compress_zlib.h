/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
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
 * ZLIB COMPRESS component
 *
 * Uses the zlib library
 */

#ifndef MCA_COMPRESS_ZLIB_EXPORT_H
#define MCA_COMPRESS_ZLIB_EXPORT_H

#include "opal_config.h"

#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/compress/compress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Local Component structures
     */
    struct opal_compress_zlib_component_t {
        opal_compress_base_component_t super;  /** Base COMPRESS component */

    };
    typedef struct opal_compress_zlib_component_t opal_compress_zlib_component_t;
    extern opal_compress_zlib_component_t mca_compress_zlib_component;

    int opal_compress_zlib_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int opal_compress_zlib_module_init(void);
    int opal_compress_zlib_module_finalize(void);

    /*
     * Actual funcationality
     */
    bool opal_compress_zlib_compress_block(uint8_t *inbytes,
                                           size_t inlen,
                                           uint8_t **outbytes,
                                           size_t *olen);
    bool opal_compress_zlib_uncompress_block(uint8_t **outbytes, size_t olen,
                                             uint8_t *inbytes, size_t len);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_COMPRESS_ZLIB_EXPORT_H */
