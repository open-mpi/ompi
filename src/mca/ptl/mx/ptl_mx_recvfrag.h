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
    
#endif

