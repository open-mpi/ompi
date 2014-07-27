/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "pml_ob1.h"
#include "pml_ob1_sendreq.h"
#include "ompi/mca/bml/base/base.h" 
#if OPAL_CUDA_SUPPORT
#include "opal/mca/common/cuda/common_cuda.h"
#include "pml_ob1_recvreq.h"
#include "opal/runtime/opal_params.h"
static void mca_pml_ob1_process_pending_cuda_async_copies(void);
#endif /* OPAL_CUDA_SUPPORT */

int mca_pml_ob1_progress(void)
{
    int i, queue_length = opal_list_get_size(&mca_pml_ob1.send_pending);
    int j, completed_requests = 0;
    bool send_succedded;

#if OPAL_CUDA_SUPPORT
    mca_pml_ob1_process_pending_cuda_async_copies();
#endif /* OPAL_CUDA_SUPPORT */

    if( OPAL_LIKELY(0 == queue_length) )
        return 0;

    for( i = 0; i < queue_length; i++ ) {
        mca_pml_ob1_send_pending_t pending_type = MCA_PML_OB1_SEND_PENDING_NONE;
        mca_pml_ob1_send_request_t* sendreq;
        mca_bml_base_endpoint_t* endpoint;

        sendreq = get_request_from_send_pending(&pending_type);
        if(OPAL_UNLIKELY(NULL == sendreq))
            break;

        switch(pending_type) {
        case MCA_PML_OB1_SEND_PENDING_NONE:
            assert(0);
            return 0;
        case MCA_PML_OB1_SEND_PENDING_SCHEDULE:
            if( mca_pml_ob1_send_request_schedule_exclusive(sendreq) ==
                OMPI_ERR_OUT_OF_RESOURCE ) {
                return 0;
            }
            completed_requests++;
            break;
        case MCA_PML_OB1_SEND_PENDING_START:
            MCA_PML_OB1_SEND_REQUEST_RESET(sendreq);
            endpoint = sendreq->req_endpoint;
            send_succedded = false;
            for(j = 0; j < (int)mca_bml_base_btl_array_get_size(&endpoint->btl_eager); j++) {
                mca_bml_base_btl_t* bml_btl;
                int rc;
                
                /* select a btl */
                bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
                rc = mca_pml_ob1_send_request_start_btl(sendreq, bml_btl);
                if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
                    send_succedded = true;
                    completed_requests++;
                    break;
                }
            }
            if( false == send_succedded ) {
                add_request_to_send_pending(sendreq, MCA_PML_OB1_SEND_PENDING_START, true);
            }
        }
    }
    return completed_requests;
}

#if OPAL_CUDA_SUPPORT
static void mca_pml_ob1_process_pending_cuda_async_copies(void)
{
    mca_btl_base_descriptor_t *frag;
    int progress;

    if (!opal_cuda_support)
        return;

    do {
        progress = progress_one_cuda_htod_event(&frag);
        if (1 == progress) {
            /* Call the finish function to make progress. */
            mca_pml_ob1_recv_request_frag_copy_finished(NULL, NULL, frag, 0);
        }
    } while (progress > 0);
    /* Consider progressing dtoh events here in future */

}
#endif /* OPAL_CUDA_SUPPORT */
