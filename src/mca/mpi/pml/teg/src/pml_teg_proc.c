/*
 * $HEADER$
 */

#include "mca/mpi/pml/teg/proc.h"
#include "lam/atomic.h"
#include "mca/mpi/pml/teg/ptl_array.h"

lam_class_info_t mca_pml_teg_proc_cls = { 
    "mca_pml_teg_proc_t", 
    &lam_list_item_cls,
    (class_init_t) mca_pml_teg_proc_init, 
    (class_destroy_t) mca_pml_teg_proc_destroy 
};

static int mca_pml_teg_procs_init = 0;
lam_list_t mca_pml_teg_procs;


void mca_pml_teg_proc_init(mca_pml_proc_t* proc)
{
    if(fetchNset(&mca_pml_teg_procs_init,1) == 0) 
        lam_list_init(&mca_pml_teg_procs);
    SUPER_INIT(proc, &lam_list_item_cls);
    mca_ptl_array_init(&proc->proc_ptl_first);
    mca_ptl_array_init(&proc->proc_ptl_next);
    lam_list_append(&mca_pml_teg_procs, &proc->super);
}


void mca_pml_teg_proc_destroy(mca_pml_proc_t* proc)
{
    SUPER_DESTROY(proc, &lam_list_item_cls);
}

