/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_MODULE_H
#define MCA_PTL_MX_MODULE_H

#include "ompi_config.h"
#include "ptl_mx.h"
#include "ptl_mx_recvfrag.h"
#include "ptl_mx_sendfrag.h"



/**
 * Prepost recv buffers
 */

static inline int mca_ptl_mx_post(mca_ptl_mx_module_t* ptl)
{
    mca_ptl_mx_recv_frag_t* frag;
    mx_return_t status;
    int rc;
    /* post an additional recv */
    MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_thread: unable to allocate recv frag\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_ptl_mx_recv_frag_init(frag, ptl);
    status = mx_irecv(
        ptl->mx_endpoint,
        frag->frag_segments,
        frag->frag_segment_count,
        1,
        MX_MATCH_MASK_NONE,
        frag,
        &frag->frag_request);
    if(status != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_post: mx_irecv() failed with status=%d\n", status);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


/**
 *  Routine to process complete request(s).
 */
                                                                                                  
static inline void mca_ptl_mx_progress(mca_ptl_mx_module_t* ptl, mx_request_t mx_request)
{
    mx_return_t mx_return;
    mx_status_t mx_status;
    uint32_t mx_result;
    mca_ptl_base_frag_t* frag;
                                                                                                  
    mx_return = mx_test(
        ptl->mx_endpoint,
        &mx_request,
        &mx_status,
        &mx_result);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_progress: mx_test() failed with status=%d\n", mx_return);
        return;
    }
                                                                                                  
    frag = (mca_ptl_base_frag_t*)mx_status.context;
    switch(frag->frag_type) {
        case MCA_PTL_FRAGMENT_SEND:
        {
            mca_ptl_mx_send_frag_handler((mca_ptl_mx_send_frag_t*)frag, ptl);
            break;
        }
        case MCA_PTL_FRAGMENT_RECV:
        {
            mca_ptl_mx_recv_frag_handler((mca_ptl_mx_recv_frag_t*)frag, ptl);
            mca_ptl_mx_post(ptl);
            break;
        }
        default:
        {
            ompi_output(0, "mca_ptl_mx_progress: invalid request type: %d\n", frag->frag_type);
            break;
        }
    }
}
                                                                                                  

#endif

