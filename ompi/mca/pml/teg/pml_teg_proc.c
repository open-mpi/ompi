/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "opal/sys/atomic.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_ptl_array.h"


static void mca_pml_teg_proc_construct(mca_pml_teg_proc_t* proc)
{
    proc->base.proc_ompi = NULL;
    proc->proc_ptl_flags = 0;
    OBJ_CONSTRUCT(&proc->base.proc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&proc->proc_ptl_first, mca_pml_teg_ptl_array_t);
    OBJ_CONSTRUCT(&proc->proc_ptl_next,  mca_pml_teg_ptl_array_t);

    OPAL_THREAD_LOCK(&mca_pml_teg.teg_lock);
    opal_list_append(&mca_pml_teg.teg_procs, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&mca_pml_teg.teg_lock);
}


static void mca_pml_teg_proc_destruct(mca_pml_teg_proc_t* proc)
{
    OPAL_THREAD_LOCK(&mca_pml_teg.teg_lock);
    opal_list_remove_item(&mca_pml_teg.teg_procs, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&mca_pml_teg.teg_lock);

    OBJ_DESTRUCT(&proc->base.proc_lock);
    OBJ_DESTRUCT(&proc->proc_ptl_first);
    OBJ_DESTRUCT(&proc->proc_ptl_next);
}

OBJ_CLASS_INSTANCE(
    mca_pml_teg_proc_t,
    opal_list_item_t,
    mca_pml_teg_proc_construct, 
    mca_pml_teg_proc_destruct 
);

