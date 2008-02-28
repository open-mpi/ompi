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



static void mca_pml_dr_endpoint_construct(mca_pml_dr_endpoint_t* ep)
{    
    OBJ_CONSTRUCT(&ep->seq_sends, ompi_seq_tracker_t);
    OBJ_CONSTRUCT(&ep->seq_recvs, ompi_seq_tracker_t);
    OBJ_CONSTRUCT(&ep->seq_recvs_matched, ompi_seq_tracker_t);
    ep->vfrag_seq = 0;
    ep->bml_endpoint = NULL;
}


static void mca_pml_dr_endpoint_destruct(mca_pml_dr_endpoint_t* ep)
{
    OBJ_DESTRUCT(&ep->seq_sends);
    OBJ_DESTRUCT(&ep->seq_recvs);
    OBJ_DESTRUCT(&ep->seq_recvs_matched);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_endpoint_t,
    opal_object_t,
    mca_pml_dr_endpoint_construct,
    mca_pml_dr_endpoint_destruct);
