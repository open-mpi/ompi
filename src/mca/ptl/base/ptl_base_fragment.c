/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "mca/ptl/base/ptl_base_fragment.h"

static void mca_ptl_base_frag_construct(mca_ptl_base_frag_t* frag);
static void mca_ptl_base_frag_destruct(mca_ptl_base_frag_t* frag);


ompi_class_t mca_ptl_base_frag_t_class = { 
    "mca_ptl_base_frag_t", 
    OBJ_CLASS(ompi_list_item_t),
    (ompi_construct_t) mca_ptl_base_frag_construct, 
    (ompi_destruct_t) mca_ptl_base_frag_destruct 
};

static void mca_ptl_base_frag_construct(mca_ptl_base_frag_t* frag)
{
    OBJ_CONSTRUCT(&frag->frag_convertor, ompi_convertor_t);
}

static void mca_ptl_base_frag_destruct(mca_ptl_base_frag_t* frag)
{
}

