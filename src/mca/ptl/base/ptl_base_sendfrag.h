/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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
#ifndef MCA_PTL_BASE_SEND_FRAG_H
#define MCA_PTL_BASE_SEND_FRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_fragment.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC extern ompi_class_t mca_ptl_base_send_frag_t_class;

/**
 * Base type for send fragment descriptors 
 */
struct mca_ptl_base_send_frag_t {
    mca_ptl_base_frag_t frag_base;  /**< base fragment descriptor */
    struct mca_pml_base_send_request_t *frag_request;  /**< pointer to send request */
};
typedef struct mca_ptl_base_send_frag_t mca_ptl_base_send_frag_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

