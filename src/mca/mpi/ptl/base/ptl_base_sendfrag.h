/* 
 * $HEADER$
 */
#ifndef MCA_PML_BASE_SEND_FRAG_H
#define MCA_PML_BASE_SEND_FRAG_H

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_fragment.h"

extern lam_class_info_t mca_ptl_base_send_frag_cls;


struct mca_ptl_base_send_frag_t {
    mca_ptl_base_frag_t super;
    struct mca_ptl_base_send_request_t *frag_request;
    unsigned char* frag_data;
    size_t frag_size;
};
typedef struct mca_ptl_base_send_frag_t mca_ptl_base_send_frag_t;


void mca_ptl_base_send_frag_init(mca_ptl_base_send_frag_t*);
void mca_ptl_base_send_frag_destroy(mca_ptl_base_send_frag_t*);

#endif

