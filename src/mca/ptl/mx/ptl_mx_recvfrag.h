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
};
typedef struct mca_ptl_mx_recv_frag_t mca_ptl_mx_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_ptl_mx_recv_frag_t);

#define MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc) \
    { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_ptl_mx_component.mx_recv_frags, item, rc); \
    frag = (mca_ptl_mx_recv_frag_t*)item; \
    }

#endif

