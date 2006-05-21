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
#include "pml_dr.h"
#include "pml_dr_endpoint.h"
#include "orte/mca/ns/ns.h"


static void mca_pml_dr_endpoint_copy(mca_pml_dr_endpoint_t* dst, mca_pml_dr_endpoint_t* src)
{
    dst->local = src->local;
    dst->src = src->src;
    dst->dst = src->dst;
    ompi_seq_tracker_copy(&dst->seq_sends, &src->seq_sends);
    ompi_seq_tracker_copy(&dst->seq_recvs, &src->seq_recvs);
    ompi_seq_tracker_copy(&dst->seq_recvs_matched, &src->seq_recvs_matched);
    dst->vfrag_seq = src->vfrag_seq;
    /* this won't work for comm spawn and other dynamic 
       processes, but will work for initial job start */
    /* dst->local = dst->dst  = ompi_pointer_array_add(&mca_pml_dr.endpoints,  */
/*                                                     (void*) dst); */
    
    
    
}


static void mca_pml_dr_endpoint_construct(mca_pml_dr_endpoint_t* ep)
{    
    OBJ_CONSTRUCT(&ep->seq_sends, ompi_seq_tracker_t);
    OBJ_CONSTRUCT(&ep->seq_recvs, ompi_seq_tracker_t);
    OBJ_CONSTRUCT(&ep->seq_recvs_matched, ompi_seq_tracker_t);
    ep->vfrag_seq = 0;
    ep->base.copy = (mca_bml_base_endpoint_copy_fn_t)mca_pml_dr_endpoint_copy;
}


static void mca_pml_dr_endpoint_destruct(mca_pml_dr_endpoint_t* ep)
{
    OBJ_DESTRUCT(&ep->seq_sends);
    OBJ_DESTRUCT(&ep->seq_recvs);
    OBJ_DESTRUCT(&ep->seq_recvs_matched);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_endpoint_t,
    mca_bml_base_endpoint_t,
    mca_pml_dr_endpoint_construct,
    mca_pml_dr_endpoint_destruct);
