/*
 * $HEADER$
 */

#include "lam/atomic.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_ptl_array.h"

lam_class_info_t mca_pml_teg_proc_cls = { 
    "mca_pml_teg_proc_t", 
    &lam_list_item_cls,
    (class_init_t) mca_pml_teg_proc_init, 
    (class_destroy_t) mca_pml_teg_proc_destroy 
};


void mca_pml_teg_proc_init(mca_pml_proc_t* proc)
{
    SUPER_INIT(proc, &lam_list_item_cls);
    mca_ptl_array_init(&proc->proc_ptl_first);
    mca_ptl_array_init(&proc->proc_ptl_next);

    THREAD_LOCK(&mca_pml_teg.teg_lock);
    lam_list_append(&mca_pml_teg.teg_procs, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);
}


void mca_pml_teg_proc_destroy(mca_pml_proc_t* proc)
{
    THREAD_LOCK(&mca_pml_teg.teg_lock);
    lam_list_remove_item(&mca_pml_teg.teg_procs, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);

    SUPER_DESTROY(proc, &lam_list_item_cls);
}

