/* 
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_PTL_MX_RECV_FRAG_H
#define MCA_PTL_MX_RECV_FRAG_H

#include "ptl_mx.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_mx_sendfrag.h"

/**
 *  MX received fragment derived type.
 */
struct mca_ptl_mx_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;  /**< base receive fragment descriptor */
    mx_request_t frag_request;
    mx_segment_t frag_segments[2];
    uint32_t frag_segment_count;
    unsigned char frag_data[32*1024];
    size_t frag_size;
};
typedef struct mca_ptl_mx_recv_frag_t mca_ptl_mx_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_ptl_mx_recv_frag_t);


#define MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_ptl_mx_component.mx_recv_frags, item, rc); \
    frag = (mca_ptl_mx_recv_frag_t*)item; \
    }

#define MCA_PTL_MX_RECV_FRAG_RETURN(frag) \
{ \
    if(frag->frag_recv.frag_is_buffered && \
       frag->frag_data != frag->frag_recv.frag_base.frag_addr) { \
        free(frag->frag_recv.frag_base.frag_addr); \
    } \
    OMPI_FREE_LIST_RETURN(&mca_ptl_mx_component.mx_recv_frags, (ompi_list_item_t*)frag); \
}
    

/**
 *  Callback on receipt of a match fragment.
 */

#define MCA_PTL_MX_RECV_FRAG_MATCH(frag, hdr)                              \
do {                                                                       \
    if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_NBO) {                    \
         MCA_PTL_BASE_MATCH_HDR_NTOH(hdr->hdr_match);                      \
    }                                                                      \
    ptl->super.ptl_match(&ptl->super, &frag->frag_recv, &hdr->hdr_match);  \
} while(0) 


/**
 * Process a fragment that completed.
 */

static inline void MCA_PTL_MX_RECV_FRAG_FRAG(mca_ptl_mx_recv_frag_t* frag)
{
    /* copy into user space */
    if(frag->frag_recv.frag_is_buffered) {
        struct iovec iov;
        unsigned int iov_count;
        unsigned int max_data;
        int free_after;
 
        iov.iov_base = frag->frag_recv.frag_base.frag_addr;
        iov.iov_len = frag->frag_recv.frag_base.frag_size;
        iov_count = 1;
        max_data = iov.iov_len;
        ompi_convertor_unpack( &frag->frag_recv.frag_base.frag_convertor,
                               &iov, &iov_count, &max_data, &free_after );
        frag->frag_recv.frag_base.frag_size = max_data;
    }
                                                                                                          
    /* progress the request */
    frag->frag_recv.frag_base.frag_owner->ptl_recv_progress(
        frag->frag_recv.frag_base.frag_owner,
        frag->frag_recv.frag_request,
        frag->frag_size,
        frag->frag_recv.frag_base.frag_size);

    MCA_PTL_MX_RECV_FRAG_RETURN(frag);
}

/**
 * Process an acknowledgment.
 */

static inline void MCA_PTL_MX_RECV_FRAG_ACK(
    mca_ptl_mx_recv_frag_t* frag,
    mca_ptl_base_header_t* hdr)
{
    mca_ptl_mx_send_frag_t* sendfrag;
    mca_pml_base_send_request_t* sendreq;
    sendfrag = (mca_ptl_mx_send_frag_t*)frag->frag_recv.frag_base.frag_header.hdr_ack.hdr_src_ptr.pval;
    sendreq = sendfrag->frag_send.frag_request;
    sendreq->req_peer_match = frag->frag_recv.frag_base.frag_header.hdr_ack.hdr_dst_match;
    MCA_PTL_MX_SEND_FRAG_PROGRESS(sendfrag);
    MCA_PTL_MX_RECV_FRAG_RETURN(frag);
}


#endif

