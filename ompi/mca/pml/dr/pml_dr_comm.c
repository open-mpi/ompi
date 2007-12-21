/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>

#include "pml_dr.h"
#include "pml_dr_comm.h"
#include "pml_dr_endpoint.h"

static void mca_pml_dr_comm_proc_construct(mca_pml_dr_comm_proc_t* proc)
{
    proc->expected_sequence = 1;
    proc->send_sequence = 0;
    OBJ_CONSTRUCT(&proc->frags_cant_match, opal_list_t);
    OBJ_CONSTRUCT(&proc->specific_receives, opal_list_t);
    OBJ_CONSTRUCT(&proc->matched_receives, opal_list_t);
    OBJ_CONSTRUCT(&proc->unexpected_frags, opal_list_t);
   
}


static void mca_pml_dr_comm_proc_destruct(mca_pml_dr_comm_proc_t* proc)
{
    OBJ_DESTRUCT(&proc->frags_cant_match);
    OBJ_DESTRUCT(&proc->matched_receives);
    OBJ_DESTRUCT(&proc->specific_receives);
    OBJ_DESTRUCT(&proc->unexpected_frags);
}


static OBJ_CLASS_INSTANCE(
    mca_pml_dr_comm_proc_t,
    opal_object_t,
    mca_pml_dr_comm_proc_construct,
    mca_pml_dr_comm_proc_destruct);

static void mca_pml_dr_comm_construct(mca_pml_dr_comm_t* comm)
{
    OBJ_CONSTRUCT(&comm->wild_receives, opal_list_t);
    OBJ_CONSTRUCT(&comm->matching_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&comm->sparse_procs, opal_pointer_array_t);
    comm->recv_sequence = 0;
    comm->procs = NULL;
    comm->num_procs = 0;
}


static void mca_pml_dr_comm_destruct(mca_pml_dr_comm_t* comm)
{
    size_t i;
    for(i=0; i<comm->num_procs; i++) { 
        OBJ_DESTRUCT((&comm->procs[i]));
    }
    if(NULL != comm->procs) { 
        free(comm->procs);
    }

    OBJ_DESTRUCT(&comm->wild_receives);
    OBJ_DESTRUCT(&comm->matching_lock);
    OBJ_DESTRUCT(&comm->sparse_procs);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_comm_t,
    opal_object_t,
    mca_pml_dr_comm_construct,
    mca_pml_dr_comm_destruct);


int mca_pml_dr_comm_init(mca_pml_dr_comm_t* dr_comm, ompi_communicator_t* ompi_comm)
{
    size_t i;
    size_t size = ompi_comm->c_remote_group->grp_proc_count;
    
    /* send message sequence-number support - sender side */
    dr_comm->procs = (mca_pml_dr_comm_proc_t*)malloc(sizeof(mca_pml_dr_comm_proc_t)*size);
    if(NULL == dr_comm->procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for(i=0; i<size; i++) {
        mca_pml_dr_comm_proc_t* proc;
        mca_pml_dr_endpoint_t* pml_ep;
        ompi_proc_t* ompi_proc;
        proc = dr_comm->procs+i;
        OBJ_CONSTRUCT(proc, mca_pml_dr_comm_proc_t);
        proc->comm_rank = i;
        ompi_proc = ompi_group_peer_lookup(ompi_comm->c_remote_group,i);
        proc->ompi_proc = ompi_proc;
        pml_ep = (mca_pml_dr_endpoint_t*) ompi_proc->proc_pml;
        opal_pointer_array_set_item(&dr_comm->sparse_procs, 
                                    pml_ep->dst,  /* from our view this is the 
                                                  peers source 'global rank' */
                                    proc); 
        proc->pml_endpoint = pml_ep;
        proc->bml_endpoint = ompi_proc->proc_bml;
    }
    dr_comm->num_procs = size;
    return OMPI_SUCCESS;
}


