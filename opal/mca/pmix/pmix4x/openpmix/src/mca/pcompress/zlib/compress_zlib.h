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

#include "pmix_config.h"

#include "src/util/output.h"

#include "src/mca/mca.h"
#include "src/mca/pcompress/pcompress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    extern pmix_mca_base_component_t mca_pcompress_zlib_component;

    /*
     * Module functions
     */
    int pmix_compress_zlib_module_init(void);
    int pmix_compress_zlib_module_finalize(void);

    /*
     * Actual funcationality
     */
    bool pmix_compress_zlib_compress_block(char *instring,
                                           uint8_t **outbytes,
                                           size_t *nbytes);
    bool pmix_compress_zlib_uncompress_block(char **outstring,
                                             uint8_t *inbytes, size_t len);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_COMPRESS_ZLIB_EXPORT_H */
