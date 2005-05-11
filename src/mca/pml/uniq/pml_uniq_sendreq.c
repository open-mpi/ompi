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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "pml_uniq.h"
#include "pml_uniq_proc.h"
#include "pml_uniq_sendreq.h"
#include "pml_uniq_recvreq.h"


                                                                                                         
static int mca_pml_uniq_send_request_fini(struct ompi_request_t** request)
{
    MCA_PML_UNIQ_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_uniq_send_request_free(struct ompi_request_t** request)
{
    MCA_PML_UNIQ_FREE(request);
    return OMPI_SUCCESS;
}

static int mca_pml_uniq_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}
                                                                                                             

static void mca_pml_uniq_send_request_construct(mca_pml_base_send_request_t* req)
{
    req->req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_base.req_ompi.req_fini = mca_pml_uniq_send_request_fini;
    req->req_base.req_ompi.req_free = mca_pml_uniq_send_request_free;
    req->req_base.req_ompi.req_cancel = mca_pml_uniq_send_request_cancel;
}


static void mca_pml_uniq_send_request_destruct(mca_pml_base_send_request_t* req)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_uniq_send_request_t,
    mca_pml_base_send_request_t,
    mca_pml_uniq_send_request_construct,
    mca_pml_uniq_send_request_destruct);
                                                                                                                                                     


/**
 *  Schedule message delivery across potentially multiple PTLs. 
 *
 *  @param request (IN)    Request to schedule
 *  @return status         Error status
 *
 */


int mca_pml_uniq_send_request_schedule(mca_ptl_base_send_request_t* req)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_send.req_base.req_comm, req->req_send.req_base.req_peer);
    mca_pml_proc_t* proc_pml = proc->proc_pml;
    int send_count = 0, rc;
    size_t bytes_remaining;

    /*
     * Only allow one thread in this routine for a given request. 
     * However, we cannot block callers on a mutex, so simply keep track 
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
    */
    if(OMPI_THREAD_ADD32(&req->req_lock,1) == 1) {
#if PML_UNIQ_ACCEPT_NEXT_PTL
        mca_ptl_proc_t* ptl_proc = &(proc_pml->proc_ptl_next);
#else
        mca_ptl_proc_t* ptl_proc = &(proc_pml->proc_ptl_first);
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
        mca_ptl_base_module_t* ptl = ptl_proc->ptl;
        /* allocate remaining bytes to PTLs */
        bytes_remaining = req->req_send.req_bytes_packed - req->req_offset;
        /* The rest of the message will be scheduled over the same PTL (the one in the next field). We try
         * to be PTL friendly here so we will respect the maximum size accepted by the PTL.
         */
        if( bytes_remaining > ptl->ptl_max_frag_size) {
            bytes_remaining = ptl->ptl_max_frag_size;
        }
    
        rc = ptl->ptl_put(ptl, ptl_proc->ptl_peer, req, req->req_offset, bytes_remaining, 0);
        if(rc == OMPI_SUCCESS) {
            send_count++;
            bytes_remaining = req->req_send.req_bytes_packed - req->req_offset;
        }
    
        /* unable to complete send - queue for later */
        if(send_count == 0) {
            OMPI_THREAD_LOCK(&mca_pml_uniq.uniq_lock);
            ompi_list_append(&mca_pml_uniq.uniq_send_pending, (ompi_list_item_t*)req);
            OMPI_THREAD_UNLOCK(&mca_pml_uniq.uniq_lock);
            req->req_lock = 0;
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* free the request if completed while in the scheduler */
        if (req->req_send.req_base.req_free_called && req->req_send.req_base.req_pml_complete) {
            MCA_PML_UNIQ_FREE((ompi_request_t**)&req);
        }
    }
    return OMPI_SUCCESS;
}


/**
 *  Update the status of the send request to reflect the number of bytes
 *  "actually" sent (and acknowledged). This should be called by the
 *  lower layer PTL after the fragment is actually delivered and has been
 *  acknowledged (if required). Note that this routine should NOT be called
 *  directly by the PTL, a function pointer is setup on the PTL at init to 
 *  enable upcalls into the PML w/out directly linking to a specific PML 
 *  implementation.
 */

void mca_pml_uniq_send_request_progress(
    struct mca_ptl_base_module_t* ptl,
    mca_ptl_base_send_request_t* req,
    size_t bytes_sent)
{
    bool schedule = false;

    OMPI_THREAD_LOCK(&ompi_request_lock);
    req->req_bytes_sent += bytes_sent;
    if (req->req_bytes_sent >= req->req_send.req_bytes_packed) {
        req->req_send.req_base.req_pml_complete = true;
        if (req->req_send.req_base.req_ompi.req_complete == false) {
            req->req_send.req_base.req_ompi.req_status.MPI_SOURCE = req->req_send.req_base.req_comm->c_my_rank;
            req->req_send.req_base.req_ompi.req_status.MPI_TAG = req->req_send.req_base.req_tag;
            req->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
            req->req_send.req_base.req_ompi.req_status._count = req->req_bytes_sent;
            req->req_send.req_base.req_ompi.req_complete = true;
            if(ompi_request_waiting) {
                ompi_condition_broadcast(&ompi_request_cond);
            }
        } else if(req->req_send.req_base.req_free_called) {
            /* don't free the request if in the scheduler */
            if(req->req_lock == 0) {
                MCA_PML_UNIQ_FREE((ompi_request_t**)&req);
            }
        } else if (req->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
            mca_pml_base_bsend_request_fini((ompi_request_t*)req);
        }
    /* test to see if we have scheduled the entire request */
    } else if (req->req_offset < req->req_send.req_bytes_packed) {
        schedule = true;
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* schedule remaining fragments of this request */
    if(schedule) {
        mca_pml_uniq_send_request_schedule(req);
    }

    /* check for pending requests that need to be progressed */
    while(ompi_list_get_size(&mca_pml_uniq.uniq_send_pending) != 0) {
        OMPI_THREAD_LOCK(&mca_pml_uniq.uniq_lock);
        req = (mca_ptl_base_send_request_t*)ompi_list_remove_first(&mca_pml_uniq.uniq_send_pending);
        OMPI_THREAD_UNLOCK(&mca_pml_uniq.uniq_lock);
        if(req == NULL)
            break;
        if(mca_pml_uniq_send_request_schedule(req) != OMPI_SUCCESS)
            break;
    }
}

