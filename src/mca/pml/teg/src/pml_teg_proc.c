/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/sys/atomic.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_ptl_array.h"


static void mca_pml_teg_proc_construct(mca_pml_proc_t* proc)
{
    proc->proc_ompi = NULL;
    proc->proc_ptl_flags = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&proc->proc_ptl_first, mca_pml_teg_ptl_array_t);
    OBJ_CONSTRUCT(&proc->proc_ptl_next,  mca_pml_teg_ptl_array_t);

    OMPI_THREAD_LOCK(&mca_pml_teg.teg_lock);
    ompi_list_append(&mca_pml_teg.teg_procs, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_lock);
}


static void mca_pml_teg_proc_destruct(mca_pml_proc_t* proc)
{
    OMPI_THREAD_LOCK(&mca_pml_teg.teg_lock);
    ompi_list_remove_item(&mca_pml_teg.teg_procs, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&mca_pml_teg.teg_lock);

    OBJ_DESTRUCT(&proc->proc_lock);
    OBJ_DESTRUCT(&proc->proc_ptl_first);
    OBJ_DESTRUCT(&proc->proc_ptl_next);
}

OBJ_CLASS_INSTANCE(
    mca_pml_teg_proc_t,
    ompi_list_item_t,
    mca_pml_teg_proc_construct, 
    mca_pml_teg_proc_destruct 
);

