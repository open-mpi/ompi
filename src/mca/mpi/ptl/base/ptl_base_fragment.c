/*
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "lam/lfc/list.h"
#include "mca/mpi/ptl/base/ptl_base_fragment.h"

static void mca_ptl_base_frag_construct(mca_ptl_base_frag_t* frag);
static void mca_ptl_base_frag_destruct(mca_ptl_base_frag_t* frag);


lam_class_info_t mca_ptl_base_frag_t_class_info = { 
    "mca_ptl_base_frag_t", 
    CLASS_INFO(lam_list_item_t),
    (lam_construct_t) mca_ptl_base_frag_construct, 
    (lam_destruct_t) mca_ptl_base_frag_destruct 
};

static void mca_ptl_base_frag_construct(mca_ptl_base_frag_t* frag)
{
    OBJ_CONSTRUCT_SUPER(frag, lam_list_item_t);
}

static void mca_ptl_base_frag_destruct(mca_ptl_base_frag_t* frag)
{
    OBJ_DESTRUCT_SUPER(frag, lam_list_item_t);
}

