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

#include "include/sys/atomic.h"
#include "mca/bml/bml.h"
#include "bml_base_endpoint.h"
#include "mca/pml/pml.h" 



static void mca_bml_base_endpoint_construct(mca_bml_base_endpoint_t* proc)
{
    proc->btl_proc = NULL;
    proc->btl_rdma_offset = 0;
    proc->btl_flags = 0;
    
    OBJ_CONSTRUCT(&proc->btl_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&proc->btl_eager, mca_bml_base_btl_array_t);
    OBJ_CONSTRUCT(&proc->btl_send,  mca_bml_base_btl_array_t);
    OBJ_CONSTRUCT(&proc->btl_rdma,  mca_bml_base_btl_array_t);
}


static void mca_bml_base_endpoint_destruct(mca_bml_base_endpoint_t* proc)
{
    OBJ_DESTRUCT(&proc->btl_lock);
    OBJ_DESTRUCT(&proc->btl_eager);
    OBJ_DESTRUCT(&proc->btl_send);
    OBJ_DESTRUCT(&proc->btl_rdma);
}


OBJ_CLASS_INSTANCE(
    mca_bml_base_endpoint_t,
    opal_object_t,
    mca_bml_base_endpoint_construct, 
    mca_bml_base_endpoint_destruct 
);

