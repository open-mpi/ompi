/*
 * $HEADER$
 */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/ptl/ptl.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvreq.h"


#define OMPI_THREAD_ADD(x,y) \
   (ompi_using_threads() ? ompi_atomic_add_32(x,y) : (*x += y))

                                                                                                         
static int mca_pml_teg_send_request_fini(struct ompi_request_t** request)
{
    MCA_PML_TEG_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_teg_send_request_free(struct ompi_request_t** request)
{
    MCA_PML_TEG_FREE(request);
    return OMPI_SUCCESS;
}

static int mca_pml_teg_send_request_cancel(struct ompi_request_t* request, int complete)
{
    return OMPI_SUCCESS;
}
                                                                                                             

static void mca_pml_teg_send_request_construct(mca_pml_base_send_request_t* req)
{
    req->req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_base.req_ompi.req_fini = mca_pml_teg_send_request_fini;
    req->req_base.req_ompi.req_free = mca_pml_teg_send_request_free;
    req->req_base.req_ompi.req_cancel = mca_pml_teg_send_request_cancel;
}


static void mca_pml_teg_send_request_destruct(mca_pml_base_send_request_t* req)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_teg_send_request_t,
    mca_pml_base_send_request_t,
    mca_pml_teg_send_request_construct,
    mca_pml_teg_send_request_destruct);
                                                                                                                                                     


/**
 *  Schedule message delivery across potentially multiple PTLs. 
 *
 *  @param request (IN)    Request to schedule
 *  @return status         Error status
 *
 */


int mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_base.req_comm, req->req_base.req_peer);
    mca_pml_proc_t* proc_pml = proc->proc_pml;
    int send_count = 0;
    size_t bytes_remaining;
    size_t num_ptl_avail;
    size_t num_ptl;

    if(OMPI_THREAD_ADD(&req->req_lock,1) == 1) {
        do {
            /* allocate remaining bytes to PTLs */
            bytes_remaining = req->req_bytes_packed - req->req_offset;
            num_ptl_avail = proc_pml->proc_ptl_next.ptl_size;
            num_ptl = 0;
            while(bytes_remaining > 0 && num_ptl++ < num_ptl_avail) {
                mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_next(&proc_pml->proc_ptl_next);
                mca_ptl_base_module_t* ptl = ptl_proc->ptl;
                int rc;
        
                /* if this is the last PTL that is available to use, or the number of 
                 * bytes remaining in the message is less than the PTLs minimum fragment 
                 * size, then go ahead and give the rest of the message to this PTL.
                 */
                size_t bytes_to_frag;
                if(num_ptl == num_ptl_avail || bytes_remaining < ptl->ptl_min_frag_size) {
                    bytes_to_frag = bytes_remaining;
        
                /* otherwise attempt to give the PTL a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as  
                 * a percentage of the overall message length (regardless of amount 
                 * previously assigned)
                 */
                } else {
                    bytes_to_frag = (ptl_proc->ptl_weight * bytes_remaining) / 100;
                }
    
                /* makes sure that we don't exceed ptl_max_frag_size */
                if(ptl->ptl_max_frag_size != 0 && bytes_to_frag > ptl->ptl_max_frag_size)
                    bytes_to_frag = ptl->ptl_max_frag_size;
        
                rc = ptl->ptl_put(ptl, ptl_proc->ptl_peer, req, req->req_offset, bytes_to_frag, 0);
                if(rc == OMPI_SUCCESS) {
                    send_count++;
                    bytes_remaining = req->req_bytes_packed - req->req_offset;
                }
            }
    
            /* unable to complete send - queue for later */
            if(send_count == 0) {
                OMPI_THREAD_LOCK(&mca_pml_teg.teg_lock);
                ompi_list_append(&mca_pml_teg.teg_send_pending, (ompi_list_item_t*)req);
                OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_lock);
                req->req_lock = 0;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

        /* fragments completed while scheduling - so retry */
        } while(OMPI_THREAD_ADD(&req->req_lock,-1) > 0);
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
 * implementation.
 */

void mca_pml_teg_send_request_progress(
    struct mca_ptl_base_module_t* ptl,
    mca_pml_base_send_request_t* req,
    size_t bytes_sent)
{
    bool schedule = false;
    OMPI_THREAD_LOCK(&ompi_request_lock);
    req->req_bytes_sent += bytes_sent;
    if (req->req_bytes_sent >= req->req_bytes_packed) {
        req->req_base.req_pml_complete = true;
        if (req->req_base.req_ompi.req_complete == false) {
            req->req_base.req_ompi.req_status.MPI_SOURCE = req->req_base.req_comm->c_my_rank;
            req->req_base.req_ompi.req_status.MPI_TAG = req->req_base.req_tag;
            req->req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
            req->req_base.req_ompi.req_status._count = req->req_bytes_sent;
            req->req_base.req_ompi.req_complete = true;
            if(ompi_request_waiting) {
                ompi_condition_broadcast(&ompi_request_cond);
            }
        } else if (req->req_base.req_free_called) {
            MCA_PML_TEG_FREE((ompi_request_t**)&req);
        } 
    /* test to see if we have scheduled the entire request */
    } else if (req->req_offset < req->req_bytes_packed)
        schedule = true;
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* schedule remaining fragments of this request */
    if(schedule) {
        mca_pml_teg_send_request_schedule(req);
    }

    /* check for pending requests that need to be progressed */
    while(ompi_list_get_size(&mca_pml_teg.teg_send_pending) != 0) {
        OMPI_THREAD_LOCK(&mca_pml_teg.teg_lock);
        req = (mca_pml_base_send_request_t*)ompi_list_remove_first(&mca_pml_teg.teg_send_pending);
        OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_lock);
        if(req == NULL)
            break;
        if(mca_pml_teg_send_request_schedule(req) != OMPI_SUCCESS)
            break;
    }
}

