/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_FRAGMENT_H
#define MCA_PML_BASE_FRAGMENT_H

#include "lam/lfc/list.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_header.h"


extern lam_class_info_t mca_ptl_base_frag_cls;

struct mca_ptl_base_frag_t {
    lam_list_item_t super;
    mca_ptl_base_header_t frag_header;
    struct mca_ptl_t* frag_owner; /**< PTL that allocated this fragment */
};
typedef struct mca_ptl_base_frag_t mca_ptl_base_frag_t;


void mca_ptl_base_frag_init(mca_ptl_base_frag_t*);
void mca_ptl_base_frag_destroy(mca_ptl_base_frag_t*);

#endif

