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

/**
 *  MX received fragment derived type.
 */
struct mca_ptl_mx_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;  /**< base receive fragment descriptor */
    mx_request_t frag_request;
    mx_segment_t frag_segments[2];
    uint32_t frag_segment_count;
    unsigned char frag_data[32768];
};
typedef struct mca_ptl_mx_recv_frag_t mca_ptl_mx_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_ptl_mx_recv_frag_t);


#define MCA_PTL_MX_RECV_FRAG_ALLOC(recvfrag, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_ptl_mx_component.mx_recv_frags, item, rc); \
    recvfrag = (mca_ptl_mx_recv_frag_t*)item; \
    }

#define MCA_PTL_MX_RECV_FRAG_RETURN(recvfrag) \
    OMPI_FREE_LIST_RETURN(&mca_ptl_mx_component.mx_recv_frags, (ompi_list_item_t*)recvfrag);
    

/**
 *
 */

static inline void mca_ptl_mx_recv_frag_init(
    mca_ptl_mx_recv_frag_t* frag, 
    mca_ptl_mx_module_t* ptl)
{
    frag->frag_recv.frag_base.frag_owner = &ptl->super;
    frag->frag_recv.frag_base.frag_peer = NULL;
    frag->frag_segment_count = 2;
    frag->frag_segments[0].segment_ptr = &frag->frag_recv.frag_base.frag_header;
    frag->frag_segments[0].segment_length = sizeof(frag->frag_recv.frag_base.frag_header);
    frag->frag_segments[1].segment_ptr = frag->frag_data;
    frag->frag_segments[1].segment_length = sizeof(frag->frag_data);
}


/**
 *
 */

static inline void mca_ptl_mx_recv_frag_handler(
    mca_ptl_mx_recv_frag_t* frag, 
    mca_ptl_mx_module_t* ptl)
{
    mca_ptl_base_header_t* hdr = &frag->frag_recv.frag_base.frag_header;
    switch(hdr->hdr_common.hdr_type) {
        case MCA_PTL_HDR_TYPE_MATCH:
        {
            if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_NBO) {
                MCA_PTL_BASE_MATCH_HDR_NTOH(hdr->hdr_match);
            }
            ptl->super.ptl_match(&ptl->super, &frag->frag_recv, &hdr->hdr_match);
            break;
        }
        case MCA_PTL_HDR_TYPE_FRAG:
            break;
        case MCA_PTL_HDR_TYPE_ACK:
            break;
    }
}

/**
 *
 */

static inline void mca_ptl_mx_recv_frag_progress(
    mca_ptl_mx_recv_frag_t* frag,
    mca_ptl_mx_module_t* ptl)
{
    /* copy data into user buffer */

    /* update request status */
    ptl->super.ptl_recv_progress(
        &ptl->super,
        frag->frag_recv.frag_request,
        frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length,
        frag->frag_recv.frag_base.frag_header.hdr_frag.hdr_frag_length);
}


#endif

