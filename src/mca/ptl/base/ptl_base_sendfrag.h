/* 
 * $HEADER$
 */
#ifndef MCA_PTL_BASE_SEND_FRAG_H
#define MCA_PTL_BASE_SEND_FRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_fragment.h"

extern lam_class_t mca_ptl_base_send_frag_t_class;


struct mca_ptl_base_send_frag_t {
    mca_ptl_base_frag_t super;
    struct mca_ptl_base_send_request_t *frag_request;
};
typedef struct mca_ptl_base_send_frag_t mca_ptl_base_send_frag_t;


#endif

