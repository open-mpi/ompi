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
    OMPI_FREE_LIST_RETURN(&mca_ptl_mx_component.mx_send_frags, (ompi_list_item_t*)sendfrag);

OBJ_CLASS_DECLARATION(mca_ptl_mx_send_frag_t);

/*
 *  Initialize the fragment based on the current offset into the users
 *  data buffer, and the indicated size.
 */
                                                                                                          
int mca_ptl_mx_send_frag_init(
    mca_ptl_mx_send_frag_t* sendfrag,
    struct mca_ptl_base_peer_t* ptl_peer,
    mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t* size,
    int flags);

/*
 *  Start the MX send for the fragment.
 */

static inline int mca_ptl_mx_send_frag_start(
    mca_ptl_mx_send_frag_t* sendfrag,
    mca_ptl_mx_module_t* ptl)
{
    mx_return_t mx_return = mx_isend(
        ptl->mx_endpoint,
        sendfrag->frag_segments,
        sendfrag->frag_segment_count,
        sendfrag->frag_send.frag_base.frag_peer->peer_addr,
        1,
        sendfrag,
        &sendfrag->frag_request);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_send_frag_start: mx_isend() failed with return value=%d\n", mx_return);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


/**
 *  Callback on MX send completion.
 */

static inline void mca_ptl_mx_send_frag_handler(
    mca_ptl_mx_send_frag_t* sendfrag,
    mca_ptl_mx_module_t* ptl)
{
    ptl->super.ptl_send_progress(
        &ptl->super,
        sendfrag->frag_send.frag_request,
        sendfrag->frag_send.frag_base.frag_size);
    if(sendfrag->frag_send.frag_base.frag_header.hdr_frag.hdr_frag_offset != 0)
        MCA_PTL_MX_SEND_FRAG_RETURN(sendfrag);
}


void mca_ptl_mx_send_frag_init_ack(
    mca_ptl_mx_send_frag_t* ack, 
    mca_ptl_mx_module_t* ptl, 
    struct mca_ptl_mx_recv_frag_t* recv_frag);


#endif

