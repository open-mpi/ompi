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

OBJ_CLASS_DECLARATION(mca_ptl_sm_second_frag_t);


/**
 *  shared memory received fragment derived type - because of
 *  the way lists are initialized in Open MPI, this is good
 *  only for the first fragment.
 */
struct mca_ptl_sm_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
    size_t buff_length;   /**< size of buffer */
    int queue_index;      /**< local process index, cached for fast
                            acking */
    void *buff;           /**< pointer to buffer */
    void *buff_offset_from_segment_base;   /**< pointer to buffer,
                                             relative to base of the
                                             shared memory segment */
};
typedef struct mca_ptl_sm_frag_t mca_ptl_sm_frag_t;

/**
 *  shared memory received fragment derived type - because of
 *  the way lists are initialized in Open MPI, this is good
 *  only for the second and beyond fragments.
 */
typedef struct mca_ptl_sm_frag_t mca_ptl_sm_second_frag_t;

#endif

