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

#define MCA_PTL_MX_POST(ptl, rc)                                                  \
do {                                                                              \
    mca_ptl_mx_recv_frag_t* frag;                                                 \
    mx_return_t mx_return;                                                        \
    /* post an additional recv */                                                 \
    MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc);                                         \
    if(rc != OMPI_SUCCESS) {                                                      \
        ompi_output(0, "mca_ptl_mx_post: unable to allocate recv fragn");         \
        rc = OMPI_ERR_OUT_OF_RESOURCE;                                            \
        break;                                                                    \
    }                                                                             \
    frag->frag_recv.frag_base.frag_owner = &ptl->super;                           \
    frag->frag_recv.frag_base.frag_peer = NULL;                                   \
    frag->frag_segment_count = 2;                                                 \
    frag->frag_segments[1].segment_ptr = frag->frag_data;                         \
    frag->frag_segments[1].segment_length = sizeof(frag->frag_data);              \
                                                                                  \
    mx_return = mx_irecv(                                                         \
        ptl->mx_endpoint,                                                         \
        frag->frag_segments,                                                      \
        frag->frag_segment_count,                                                 \
        1,                                                                        \
        MX_MATCH_MASK_NONE,                                                       \
        frag,                                                                     \
        &frag->frag_request);                                                     \
    if(mx_return != MX_SUCCESS) {                                                 \
        ompi_output(0, "mca_ptl_mx_post: mx_irecv() failed with status=%dn",      \
            mx_return);                                                           \
        rc = OMPI_ERROR;                                                          \
    }                                                                             \
    rc = OMPI_SUCCESS;                                                            \
} while(0)


/**
 *  Routine to process complete request(s).
 */
                                                                                                  
#define MCA_PTL_MX_PROGRESS(ptl, mx_request)                                        \
{                                                                                   \
    mx_return_t mx_return;                                                          \
    mx_status_t mx_status;                                                          \
    uint32_t mx_result;                                                             \
    mca_ptl_base_frag_t* frag;                                                      \
                                                                                    \
    mx_return = mx_test(                                                            \
        ptl->mx_endpoint,                                                           \
        &mx_request,                                                                \
        &mx_status,                                                                 \
        &mx_result);                                                                \
    if(mx_return != MX_SUCCESS) {                                                   \
        ompi_output(0, "mca_ptl_mx_progress: mx_test() failed with status=%dn",     \
            mx_return);                                                             \
        break;                                                                      \
    }                                                                               \
                                                                                    \
    frag = (mca_ptl_base_frag_t*)mx_status.context;                                 \
    switch(frag->frag_type) {                                                       \
        case MCA_PTL_FRAGMENT_SEND:                                                 \
        {                                                                           \
            mca_ptl_mx_send_frag_t* sendfrag = (mca_ptl_mx_send_frag_t*)frag;       \
            mca_pml_base_send_request_t* sendreq =                                  \
                sendfrag->frag_send.frag_request;                                   \
            bool req_cached = sendreq->req_cached;                                  \
            ptl->super.ptl_send_progress(                                           \
                &ptl->super,                                                        \
                sendreq,                                                            \
                sendfrag->frag_send.frag_base.frag_size);                           \
            if(req_cached == false)                                                 \
                MCA_PTL_MX_SEND_FRAG_RETURN(sendfrag);                              \
            break;                                                                  \
        }                                                                           \
        case MCA_PTL_FRAGMENT_RECV:                                                 \
        {                                                                           \
            mca_ptl_mx_recv_frag_t* recvfrag = (mca_ptl_mx_recv_frag_t*)frag;       \
            mca_ptl_base_header_t* hdr =                                            \
                &recvfrag->frag_recv.frag_base.frag_header;                         \
            int rc;                                                                 \
            switch(hdr->hdr_common.hdr_type) {                                      \
                case MCA_PTL_HDR_TYPE_MATCH:                                        \
                {                                                                   \
                    if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_NBO) {             \
                        MCA_PTL_BASE_MATCH_HDR_NTOH(hdr->hdr_match);                \
                    }                                                               \
                    ptl->super.ptl_match(&ptl->super, &recvfrag->frag_recv,         \
                        &hdr->hdr_match);                                           \
                    break;                                                          \
                }                                                                   \
                case MCA_PTL_HDR_TYPE_FRAG:                                         \
                    break;                                                          \
                case MCA_PTL_HDR_TYPE_ACK:                                          \
                    break;                                                          \
            }                                                                       \
            MCA_PTL_MX_POST(ptl, rc);                                               \
            break;                                                                  \
        }                                                                           \
        default:                                                                    \
        {                                                                           \
            ompi_output(0, "mca_ptl_mx_progress: invalid request type: %dn",        \
                frag->frag_type);                                                   \
            break;                                                                  \
        }                                                                           \
    }                                                                               \
}

#endif

