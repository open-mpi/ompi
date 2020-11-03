/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 *
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef PMIX_COMPRESS_BASE_H
#define PMIX_COMPRESS_BASE_H

#include "pmix_config.h"
#include "src/mca/pcompress/pcompress.h"
#include "src/util/pmix_environ.h"

#include "src/mca/base/base.h"

/*
 * Global functions for MCA overall COMPRESS
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* define a macro for quickly checking if a string exceeds the
 * compression limit */
#define PMIX_STRING_SIZE_CHECK(s) \
    (PMIX_STRING == (s)->type && NULL != (s)->data.string && pmix_compress_base.compress_limit < strlen((s)->data.string))

#define PMIX_VALUE_COMPRESSED_STRING_UNPACK(s)                              \
    do {                                                                    \
        char *tmp;                                                          \
        /* if this is a compressed string, then uncompress it */            \
        if (PMIX_COMPRESSED_STRING == (s)->type) {                          \
            pmix_compress.decompress_string(&tmp, (uint8_t*)(s)->data.bo.bytes, \
                (s)->data.bo.size);                                         \
            if (NULL == tmp) {                                              \
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);                             \
                rc = PMIX_ERR_NOMEM;                                        \
                PMIX_VALUE_RELEASE(s);                                      \
                val = NULL;                                                 \
            } else {                                                        \
                PMIX_VALUE_DESTRUCT(s);                                     \
                (s)->data.string = tmp;                                     \
                (s)->type = PMIX_STRING;                                    \
            }                                                               \
        }                                                                   \
    } while(0)

typedef struct {
    size_t compress_limit;
    bool selected;
} pmix_compress_base_t;

PMIX_EXPORT extern pmix_compress_base_t pmix_compress_base;

    /**
     * Select an available component.
     *
     * @retval OPAL_SUCCESS Upon Success
     * @retval OPAL_NOT_FOUND If no component can be selected
     * @retval OPAL_ERROR Upon other failure
     *
     */
    PMIX_EXPORT int pmix_compress_base_select(void);

    /**
     * Globals
     */
    PMIX_EXPORT extern pmix_mca_base_framework_t pmix_pcompress_base_framework;
    PMIX_EXPORT extern pmix_compress_base_module_t pmix_compress;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* PMIX_COMPRESS_BASE_H */
