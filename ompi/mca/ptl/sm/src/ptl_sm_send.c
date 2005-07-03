/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>

#include "opal/util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "ptl_sm.h"
#include "util/sys_info.h"
#include "mca/ptl/sm/src/ptl_sm_peer.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "util/proc_info.h"
#include "util/printf.h"
#include "mca/ptl/sm/src/ptl_sm_sendreq.h"
#include "class/ompi_fifo.h"
#include "class/ompi_free_list.h"
#include "opal/threads/mutex.h"
#include "datatype/datatype.h"


/*
 * If we're "recompiling" (see the Makefile.am for an explanation),
 * change the function names.
 */

#ifdef SM_COMMON_BASE_ADDR
#define mca_ptl_sm_matched mca_ptl_sm_matched_same_base_addr
#endif

/*
 *  A posted receive has been matched:
 *    - deliver data to user buffers
 *    - update receive request data
 *    - ack
 *
 *  fragment lists are NOT manipulated.
 */

void mca_ptl_sm_matched(
    mca_ptl_base_module_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
    mca_ptl_base_recv_request_t* recv_desc;
    mca_ptl_sm_frag_t *sm_frag_desc;
    struct iovec iov; 
    ompi_convertor_t frag_convertor;
    int  free_after,my_local_smp_rank,peer_local_smp_rank, return_status;
    unsigned int iov_count; 
    size_t max_data;
    ompi_fifo_t *send_fifo;

    /* copy data from shared memory buffer to user buffer */
    /* get pointer to the matched receive descriptor */
    recv_desc = frag->frag_request;
    sm_frag_desc = (mca_ptl_sm_frag_t *)frag;

    my_local_smp_rank=mca_ptl_sm_component.my_smp_rank;
    peer_local_smp_rank=sm_frag_desc->queue_index;

    /* copy, only if there is data to copy */
    max_data=0;
    if( 0 <  sm_frag_desc->super.frag_base.frag_size ) {
 
        /* 
         * Initialize convertor and use it to unpack data  
         */ 
        ompi_convertor_clone_with_position( &(recv_desc->req_recv.req_convertor), &frag_convertor,
                                            1, &(sm_frag_desc->send_offset) );

        /* convert address from sender's address space to my virtual
         * address space */
#ifdef SM_COMMON_BASE_ADDR
        iov.iov_base = (void *)( (char *)sm_frag_desc->buff);
#else
        iov.iov_base = (void *)( (char *)sm_frag_desc->buff+
                mca_ptl_sm_component.sm_offset[peer_local_smp_rank]);
#endif
        iov.iov_len = sm_frag_desc->super.frag_base.frag_size;
        iov_count = 1;
        max_data = iov.iov_len;
        ompi_convertor_unpack( &frag_convertor, &iov, &iov_count, &max_data, &free_after ); 
    }

    /* update receive request information */
    frag->frag_base.frag_owner->ptl_recv_progress(
            ptl,
            recv_desc, 
            sm_frag_desc->super.frag_base.frag_size,
            max_data);

    /* ack - ack recycles shared memory fragment resources, so
     *       don't agragate */

    send_fifo=&(mca_ptl_sm_component.fifo
            [my_local_smp_rank][peer_local_smp_rank]);

    /* lock as multiple processes can attempt to init the head */
    if(opal_using_threads())
        opal_atomic_lock(&send_fifo->head_lock);

    /* check to see if fifo is allocated */
    if(OMPI_CB_FREE == send_fifo->head) {
        /* no queues have been allocated - allocate now */
        return_status=ompi_fifo_init_same_base_addr(
                mca_ptl_sm_component.size_of_cb_queue,
                mca_ptl_sm_component.cb_lazy_free_freq,
                /* at this stage we are not doing anything with memory
                 * locality */
                0,0,0,
                send_fifo, mca_ptl_sm_component.sm_mpool);
        if( return_status != OMPI_SUCCESS ) {
            if(opal_using_threads())
                opal_atomic_unlock(&send_fifo->head_lock);
            return;
        }
    }

    /* change address to be relative to offset from base of shared
     *   memory segment 
     */

    /* set the fragment type to be an ack */
    sm_frag_desc->super.frag_base.frag_header.hdr_common.hdr_type=
        MCA_PTL_HDR_TYPE_ACK;
    return_status=ompi_fifo_write_to_head_same_base_addr(sm_frag_desc,
            send_fifo, mca_ptl_sm_component.sm_mpool);

    if(opal_using_threads())
        opal_atomic_unlock(&send_fifo->head_lock);

    /* if can't ack, put on list for later delivery */
    if( 0 > return_status ) {
        OPAL_THREAD_LOCK(&(mca_ptl_sm_component.sm_pending_ack_lock));
        opal_list_append(&(mca_ptl_sm_component.sm_pending_ack),
                (opal_list_item_t *)sm_frag_desc);
        OPAL_THREAD_UNLOCK(&(mca_ptl_sm_component.sm_pending_ack_lock));
    } else {
        MCA_PTL_SM_SIGNAL_PEER(mca_ptl_sm_component.sm_peers[peer_local_smp_rank]);
    }


    /* return */
    return;
}
