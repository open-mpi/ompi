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
 *  Post a receive for short messages (<32 K)
 */
                                                                                                  
#define MCA_PTL_MX_POST(ptl) \
do { \
    mca_ptl_mx_recv_frag_t *frag; \
    mx_return_t mx_return; \
    int rc; \
 \
    MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc); \
    if(rc != OMPI_SUCCESS) { \
        ompi_output(0, "mca_ptl_mx_match: unable to allocate resources.\n"); \
        return; \
    } \
    frag->frag_recv.frag_base.frag_owner = &ptl->super; \
    frag->frag_recv.frag_base.frag_peer = NULL; \
 \
    frag->frag_size = 0; \
    frag->frag_recv.frag_base.frag_size = 0; \
    frag->frag_recv.frag_base.frag_addr = frag->frag_data; \
    frag->frag_recv.frag_is_buffered = true; \
    frag->frag_recv.frag_request = NULL; \
    frag->frag_segment_count = 2; \
    OMPI_THREAD_ADD32(&ptl->mx_recvs_posted, 1); \
    mx_return = mx_irecv( \
        ptl->mx_endpoint, \
        frag->frag_segments, \
        frag->frag_segment_count, \
        0, \
        MX_MATCH_MASK_NONE, \
        frag, \
        &frag->frag_request); \
    if(mx_return != MX_SUCCESS) { \
        ompi_output(0, "mca_ptl_mx_match: mx_irecv() failed with status=%dn", mx_return); \
        MCA_PTL_MX_RECV_FRAG_RETURN(frag); \
    } \
} while(0)
                                                                                                          

/**
 *  Routine to process complete request(s).
 */
                                                                                                  
#define MCA_PTL_MX_PROGRESS(ptl, mx_status)                                         \
do {                                                                                \
    mca_ptl_base_frag_t* frag;                                                      \
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
                    OMPI_THREAD_ADD32(&ptl->mx_recvs_posted, -1);                   \
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
                    OMPI_THREAD_ADD32(&ptl->mx_recvs_posted, -1);                   \
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

void mca_ptl_mx_enable(void);
void mca_ptl_mx_disable(void);

#endif

