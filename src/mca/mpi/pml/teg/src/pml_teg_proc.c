/*
 * $HEADER$
 */

#include "lam/atomic.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_ptl_array.h"

lam_class_info_t mca_pml_teg_proc_t_class_info = { 
    "mca_pml_teg_proc_t", 
    CLASS_INFO(lam_list_item_t),
    (lam_construct_t) mca_pml_teg_proc_construct, 
    (lam_destruct_t) mca_pml_teg_proc_destruct 
};


void mca_pml_teg_proc_construct(mca_pml_proc_t* proc)
{
    OBJ_CONSTRUCT_SUPER(proc, lam_list_item_t);
    mca_ptl_array_construct(&proc->proc_ptl_first);
    mca_ptl_array_construct(&proc->proc_ptl_next);

    THREAD_LOCK(&mca_pml_teg.teg_lock);
    lam_list_append(&mca_pml_teg.teg_procs, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);
}


void mca_pml_teg_proc_destruct(mca_pml_proc_t* proc)
{
    THREAD_LOCK(&mca_pml_teg.teg_lock);
    lam_list_remove_item(&mca_pml_teg.teg_procs, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&mca_pml_teg.teg_lock);

    OBJ_DESTRUCT_SUPER(proc, lam_list_item_t);
}

