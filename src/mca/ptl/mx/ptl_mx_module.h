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
 *  Routine to process complete request(s).
 */
                                                                                                  
#define MCA_PTL_MX_PROGRESS(ptl, mx_request)                                        \
do {                                                                                \
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
            MCA_PTL_MX_SEND_FRAG_PROGRESS(sendfrag);                                \
            break;                                                                  \
        }                                                                           \
        case MCA_PTL_FRAGMENT_RECV:                                                 \
        {                                                                           \
            mca_ptl_mx_recv_frag_t* recvfrag = (mca_ptl_mx_recv_frag_t*)frag;       \
            mca_ptl_base_header_t* hdr =                                            \
                &recvfrag->frag_recv.frag_base.frag_header;                         \
            switch(hdr->hdr_common.hdr_type) {                                      \
                case MCA_PTL_HDR_TYPE_MATCH:                                        \
                {                                                                   \
                    MCA_PTL_MX_RECV_FRAG_MATCH(recvfrag,hdr);                       \
                    break;                                                          \
                }                                                                   \
                case MCA_PTL_HDR_TYPE_FRAG:                                         \
                {                                                                   \
                    MCA_PTL_MX_RECV_FRAG_FRAG(recvfrag);                            \
                    break;                                                          \
                }                                                                   \
                case MCA_PTL_HDR_TYPE_ACK:                                          \
                {                                                                   \
                    mca_ptl_mx_send_frag_t* sendfrag;                               \
                    mca_pml_base_send_request_t* sendreq;                           \
                    sendfrag = (mca_ptl_mx_send_frag_t*)                            \
                        hdr->hdr_ack.hdr_src_ptr.pval;                              \
                    sendreq = sendfrag->frag_send.frag_request;                     \
                    sendreq->req_peer_match = hdr->hdr_ack.hdr_dst_match;           \
                    MCA_PTL_MX_SEND_FRAG_PROGRESS(sendfrag);                        \
                    MCA_PTL_MX_RECV_FRAG_RETURN(recvfrag);                          \
                    break;                                                          \
                }                                                                   \
            }                                                                       \
            break;                                                                  \
        }                                                                           \
        default:                                                                    \
        {                                                                           \
            ompi_output(0, "mca_ptl_mx_progress: invalid request type: %dn",        \
                frag->frag_type);                                                   \
            break;                                                                  \
        }                                                                           \
    }                                                                               \
} while(0)

#endif

