#ifndef MCA_PTL_IB_RECV_FRAG_H
#define MCA_PTL_IB_RECV_FRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"

#define MCA_PTL_IB_UNEX_BUF_SIZE    (4096)

OBJ_CLASS_DECLARATION(mca_ptl_ib_recv_frag_t);

/**
 *  IB received fragment derived type.
 */
struct mca_ptl_ib_recv_frag_t {
    mca_ptl_base_recv_frag_t        super; 
    /**< base receive fragment descriptor */
    char                            unex_buf[MCA_PTL_IB_UNEX_BUF_SIZE];
    /**< Unexpected buffer */
};
typedef struct mca_ptl_ib_recv_frag_t mca_ptl_ib_recv_frag_t;


void mca_ptl_ib_recv_frag_done (mca_ptl_base_header_t*,
        mca_ptl_base_recv_frag_t*, mca_pml_base_recv_request_t*);

void mca_ptl_ib_process_recv(mca_ptl_base_module_t* , void*);
#endif
