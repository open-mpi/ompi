/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BML_R2_BTL_H
#define MCA_BML_R2_BTL_H

#include "opal/util/output.h"
#include "ompi/mca/btl/btl.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Allocate a descriptor for control message
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_BML_R2_BTL_DES_ALLOC(btl, descriptor, size)                       \
do {                                                                          \
    if(NULL != (descriptor = btl->btl_cache)) {                               \
        /* atomically acquire the cached descriptor */                        \
        if(opal_atomic_cmpset_ptr(&btl->btl_cache, descriptor, NULL) == 0) {  \
            descriptor = btl->btl_alloc(btl->btl, sizeof(mca_bml_r2_hdr_t) +  \
               MCA_BTL_DES_MAX_SEGMENTS * sizeof(mca_btl_base_segment_t));    \
        }                                                                     \
    } else {                                                                  \
            descriptor = btl->btl_alloc(btl->btl, sizeof(mca_bml_r2_hdr_t) +  \
                MCA_BTL_DES_MAX_SEGMENTS * sizeof(mca_btl_base_segment_t));   \
    }                                                                         \
    descriptor->des_src->seg_len = size;                                      \
} while(0)
#else
#define MCA_BML_R2_BTL_DES_ALLOC(btl, descriptor, size)                       \
do {                                                                          \
    if(NULL != (descriptor = btl->btl_cache)) {                               \
        btl->btl_cache = NULL;                                                \
    } else {                                                                  \
        descriptor = btl->btl_alloc(btl->btl, sizeof(mca_bml_r2_hdr_t) +      \
            MCA_BTL_DES_MAX_SEGMENTS * sizeof(mca_btl_base_segment_t));       \
    }                                                                         \
    descriptor->des_src->seg_len = size;                                      \
} while(0)
#endif

/**
 * Return a descriptor
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define MCA_BML_R2_BTL_DES_RETURN(btl, descriptor)                            \
do {                                                                          \
    if(opal_atomic_cmpset_ptr(&btl->btl_cache,NULL,descriptor) == 0) {        \
        btl->btl_free(btl->btl,descriptor);                                   \
    }                                                                         \
} while(0)
#else
#define MCA_BML_R2_BTL_DES_RETURN(btl, descriptor)                            \
do {                                                                          \
    if(NULL == btl->btl_cache) {                                              \
        btl->btl_cache = descriptor;                                          \
    } else {                                                                  \
        btl->btl_free(endpoint->btl,descriptor);                              \
    }                                                                         \
} while(0)
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

