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
#ifndef MCA_PTL_SM_SEND_FRAG_H
#define MCA_PTL_SM_SEND_FRAG_H

#include <sys/types.h>
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_sm.h"


OBJ_CLASS_DECLARATION(mca_ptl_sm_send_frag_t);


/**
 * shared memory send fragment derived type.
 */
struct mca_ptl_sm_send_frag_t {
   mca_ptl_base_send_frag_t super;  /**< base send fragment descriptor */
};
typedef struct mca_ptl_sm_send_frag_t mca_ptl_sm_send_frag_t;


#define MCA_PTL_SM_SEND_FRAG_ALLOC(item, rc)  \
    OMPI_FREE_LIST_GET(&mca_ptl_sm_module.sm_send_frags, item, rc);


/**
 * Initialize a fragment descriptor.
 *
 * frag (IN)      Fragment
 * peer (IN)      PTL peer addressing information 
 * request (IN)   Send request
 * offset (IN)    Current offset into packed buffer
 * size (IN/OUT)  Requested size / actual size returned
 * flags (IN)
 */

int mca_ptl_sm_send_frag_init(
    mca_ptl_sm_send_frag_t*, 
    struct mca_ptl_base_peer_t*, 
    struct mca_pml_base_send_request_t*, 
    size_t offset,
    size_t* size,
    int flags);


#endif

