#ifndef MCA_PTL_IB_RECV_FRAG_H
#define MCA_PTL_IB_RECV_FRAG_H

#include "include/sys/atomic.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"

OBJ_CLASS_DECLARATION(mca_ptl_ib_recv_frag_t);

/**
 *  IB received fragment derived type.
 */
struct mca_ptl_ib_recv_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
};
typedef struct mca_ptl_ib_recv_frag_t mca_ptl_ib_recv_frag_t;

#endif
