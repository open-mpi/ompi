/* 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_SM_RECV_FRAG_H
#define MCA_PTL_SM_RECV_FRAG_H

#include <string.h>
#include <sys/types.h>
#include "include/sys/atomic.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_sm.h"


OBJ_CLASS_DECLARATION(mca_ptl_sm_frag_t);


/**
 *  shared memory received fragment derived type.
 */
struct mca_ptl_sm_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
};
typedef struct mca_ptl_sm_frag_t mca_ptl_sm_frag_t;

#endif

