/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_SEND_FRAG_H
#define MCA_PTL_MX_SEND_FRAG_H

#include "ompi_config.h"
#include "include/sys/atomic.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_mx.h"
#include "ptl_mx_peer.h"


/**
 * MX send fragment derived type.
 */
struct mca_ptl_mx_send_frag_t {
   mca_ptl_base_send_frag_t frag_send;  /**< base send fragment descriptor */
   int frag_free;
   mx_request_t frag_request;
   mx_segment_t frag_segments[2];
   size_t frag_segment_count;
};
typedef struct mca_ptl_mx_send_frag_t mca_ptl_mx_send_frag_t;

#define MCA_PTL_MX_SEND_FRAG_ALLOC(sendfrag, rc)  \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_ptl_mx_component.mx_send_frags, item, rc); \
    sendfrag = (mca_ptl_mx_send_frag_t*)item; \
    }

#define MCA_PTL_MX_SEND_FRAG_RETURN(sendfrag)  \
    { \
    int seg_free = sendfrag->frag_free; \
    mx_segment_t *seg_ptr = sendfrag->frag_segments+1; \
    while(seg_free) { \
        if(seg_free & 1) { \
            free(seg_ptr->segment_ptr); \
        } \
        seg_free >>= 1; \
        seg_ptr++; \
    } \
    OMPI_FREE_LIST_RETURN(&mca_ptl_mx_component.mx_send_frags, (ompi_list_item_t*)sendfrag); \
    }

OBJ_CLASS_DECLARATION(mca_ptl_mx_send_frag_t);


#define MCA_PTL_MX_SEND_FRAG_INIT_ACK(ack,ptl,frag) \
{ \
    mca_ptl_base_header_t* hdr = &(ack)->frag_send.frag_base.frag_header; \
    mca_pml_base_recv_request_t* request = frag->frag_recv.frag_request; \
    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK; \
    hdr->hdr_common.hdr_flags = 0; \
    hdr->hdr_common.hdr_size = sizeof(mca_ptl_base_ack_header_t); \
    hdr->hdr_ack.hdr_src_ptr = frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_src_ptr; \
    hdr->hdr_ack.hdr_dst_match.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */ \
    hdr->hdr_ack.hdr_dst_match.pval = request; \
    hdr->hdr_ack.hdr_dst_addr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */ \
    hdr->hdr_ack.hdr_dst_addr.pval = request->req_base.req_addr; \
    hdr->hdr_ack.hdr_dst_size = request->req_bytes_packed; \
    (ack)->frag_send.frag_request = NULL; \
    (ack)->frag_send.frag_base.frag_peer = NULL; \
    (ack)->frag_send.frag_base.frag_owner = ptl; \
    (ack)->frag_send.frag_base.frag_addr = NULL; \
    (ack)->frag_send.frag_base.frag_size = 0; \
    (ack)->frag_segment_count = 1; \
    (ack)->frag_free = 0; \
}
                                                                                                                  

#endif

