#ifndef MCA_PTL_IB_SEND_FRAG_H
#define MCA_PTL_IB_SEND_FRAG_H

#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"

#include "ptl_ib_priv.h"

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

void mca_ptl_ib_process_send_comp(mca_ptl_base_module_t *, void*);

#endif
