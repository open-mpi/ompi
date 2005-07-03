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

#include "mca/pml/pml.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"

static void mca_ptl_base_send_frag_construct(mca_ptl_base_send_frag_t* frag);
static void mca_ptl_base_send_frag_destruct(mca_ptl_base_send_frag_t* frag);


opal_class_t mca_ptl_base_send_frag_t_class = { 
    "mca_ptl_base_send_frag_t", 
    OBJ_CLASS(mca_ptl_base_frag_t),
    (opal_construct_t) mca_ptl_base_send_frag_construct, 
    (opal_destruct_t) mca_ptl_base_send_frag_destruct 
};
                                                                                                 

static void mca_ptl_base_send_frag_construct(mca_ptl_base_send_frag_t* frag)
{
    frag->frag_base.frag_type = MCA_PTL_FRAGMENT_SEND;
}

static void mca_ptl_base_send_frag_destruct(mca_ptl_base_send_frag_t* frag)
{
}

