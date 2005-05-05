/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/sys/atomic.h"
#include "pml_uniq.h"
#include "pml_uniq_proc.h"


static void mca_pml_uniq_proc_construct(mca_pml_proc_t* proc)
{
    proc->proc_ompi = NULL;
    proc->proc_ptl_flags = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);

    proc->proc_ptl_first.ptl_peer = NULL;
    proc->proc_ptl_first.ptl_base = NULL;
    proc->proc_ptl_first.ptl = NULL;

    proc->proc_ptl_next.ptl_peer = NULL;
    proc->proc_ptl_next.ptl_base = NULL;
    proc->proc_ptl_next.ptl = NULL;

    OMPI_THREAD_LOCK(&mca_pml_uniq.uniq_lock);
    ompi_list_append(&mca_pml_uniq.uniq_procs, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&mca_pml_uniq.uniq_lock);
}


static void mca_pml_uniq_proc_destruct(mca_pml_proc_t* proc)
{
    OMPI_THREAD_LOCK(&mca_pml_uniq.uniq_lock);
    ompi_list_remove_item(&mca_pml_uniq.uniq_procs, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&mca_pml_uniq.uniq_lock);

    OBJ_DESTRUCT(&proc->proc_lock);
}

OBJ_CLASS_INSTANCE(
    mca_pml_uniq_proc_t,
    ompi_list_item_t,
    mca_pml_uniq_proc_construct, 
    mca_pml_uniq_proc_destruct 
);

