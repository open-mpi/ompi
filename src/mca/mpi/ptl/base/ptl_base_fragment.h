/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MCA_PML_BASE_FRAGMENT_H
#define MCA_PML_BASE_FRAGMENT_H

#include "lam/lfc/lam_list.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_header.h"


extern lam_class_info_t mca_ptl_base_frag_t_class_info;

struct mca_ptl_base_frag_t {
    lam_list_item_t super;
    mca_ptl_base_header_t frag_header;
    struct mca_ptl_t* frag_owner; /**< PTL that allocated this fragment */
    struct mca_ptl_base_peer_t* frag_peer; /**< PTL specific addressing info */
    void  *frag_addr; /* pointer into request buffer at fragment offset */
    size_t frag_size; /* number of bytes available in request buffer */
};
typedef struct mca_ptl_base_frag_t mca_ptl_base_frag_t;


#endif

