/*
 * $HEADER$
 */

#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"

static void mca_ptl_base_send_frag_init(mca_ptl_base_send_frag_t* frag);
static void mca_ptl_base_send_frag_destroy(mca_ptl_base_send_frag_t* frag);


lam_class_info_t mca_ptl_base_send_frag_cls = { 
    "mca_ptl_base_send_frag_t", 
    &mca_ptl_base_frag_cls,
    (class_init_t) mca_ptl_base_send_frag_init, 
    (class_destroy_t) mca_ptl_base_send_frag_destroy 
};
                                                                                                 

static void mca_ptl_base_send_frag_init(mca_ptl_base_send_frag_t* frag)
{
    SUPER_INIT(frag, &mca_ptl_base_frag_cls);
}

static void mca_ptl_base_send_frag_destroy(mca_ptl_base_send_frag_t* frag)
{
    SUPER_DESTROY(frag, &mca_ptl_base_frag_cls);
}

