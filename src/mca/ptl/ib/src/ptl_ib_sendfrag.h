/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PTL_IB_SEND_FRAG_H
#define MCA_PTL_IB_SEND_FRAG_H

#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"

#include "ptl_ib_priv.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_ptl_ib_send_frag_t);

/**
 * IB send fragment derived type.
 */
struct mca_ptl_ib_send_frag_t {
   mca_ptl_base_send_frag_t                 frag_send;
   /**< base send fragment descriptor */

   ib_buffer_t                              ib_buf;
   /**< IB buffer attached to this frag */

   volatile int                             frag_progressed;
   bool                                     frag_ack_pending;
};
typedef struct mca_ptl_ib_send_frag_t mca_ptl_ib_send_frag_t;

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

int mca_ptl_ib_send_frag_init(
        mca_ptl_ib_send_frag_t*,
        struct mca_ptl_base_peer_t*,
        struct mca_pml_base_send_request_t*,
        size_t offset,
        size_t* size,
        int flags);

/**
 * Initialize a fragment descriptor.
 *
 * request (IN)      PML base send request
 * ptl (IN)          PTL module
 * RETURN            mca_ptl_ib_send_frag_t*
 *
 */

mca_ptl_ib_send_frag_t* mca_ptl_ib_alloc_send_frag(
        mca_ptl_base_module_t*       ptl,
        mca_pml_base_send_request_t* request);

int mca_ptl_ib_register_send_frags(mca_ptl_base_module_t *ptl);

void mca_ptl_ib_process_send_comp(mca_ptl_base_module_t *, 
        void*);
void mca_ptl_ib_process_rdma_w_comp(mca_ptl_base_module_t *module,
        void* comp_addr);

int mca_ptl_ib_put_frag_init(mca_ptl_ib_send_frag_t *sendfrag, 
        struct mca_ptl_base_peer_t *ptl_peer,
        struct mca_pml_base_send_request_t *req, 
        size_t offset, size_t *size, int flags);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
