/*
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
#include "pml_cm.h"
#include "pml_cm_proc.h"


static void mca_pml_cm_proc_construct(mca_pml_cm_proc_t* proc)
{
    proc->base.proc_ompi = NULL;
    OBJ_CONSTRUCT(&proc->base.proc_lock, opal_mutex_t);
}


static void mca_pml_cm_proc_destruct(mca_pml_cm_proc_t* proc)
{
    OBJ_DESTRUCT(&proc->base.proc_lock);
}


OBJ_CLASS_INSTANCE(
    mca_pml_cm_proc_t,
    opal_list_item_t,
    mca_pml_cm_proc_construct,
    mca_pml_cm_proc_destruct
);

