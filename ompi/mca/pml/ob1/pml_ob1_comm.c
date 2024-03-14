/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 *
 * Copyright (c) 2018      Sandia National Laboratories
 * 			   All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>

#include "pml_ob1.h"
#include "pml_ob1_comm.h"



static void mca_pml_ob1_comm_proc_construct(mca_pml_ob1_comm_proc_t* proc)
{
    proc->ompi_proc = NULL;
    proc->expected_sequence = 1;
    proc->send_sequence = 0;
    proc->frags_cant_match = NULL;
    /* don't know the index of this communicator yet */
    proc->comm_index = -1;
#if !MCA_PML_OB1_CUSTOM_MATCH
    OBJ_CONSTRUCT(&proc->specific_receives, opal_list_t);
    OBJ_CONSTRUCT(&proc->unexpected_frags, opal_list_t);
#endif
}


static void mca_pml_ob1_comm_proc_destruct(mca_pml_ob1_comm_proc_t* proc)
{
    assert(NULL == proc->frags_cant_match);
#if !MCA_PML_OB1_CUSTOM_MATCH
    OBJ_DESTRUCT(&proc->specific_receives);
    OBJ_DESTRUCT(&proc->unexpected_frags);
#endif
    if (proc->ompi_proc) {
        OBJ_RELEASE(proc->ompi_proc);
    }
}


OBJ_CLASS_INSTANCE(mca_pml_ob1_comm_proc_t, opal_object_t,
                   mca_pml_ob1_comm_proc_construct,
                   mca_pml_ob1_comm_proc_destruct);


static void mca_pml_ob1_comm_construct(mca_pml_ob1_comm_t* comm)
{
#if !MCA_PML_OB1_CUSTOM_MATCH
    OBJ_CONSTRUCT(&comm->wild_receives, opal_list_t);
#else
    comm->prq = custom_match_prq_init();
    comm->umq = custom_match_umq_init();
#endif
    OBJ_CONSTRUCT(&comm->matching_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&comm->proc_lock, opal_mutex_t);
    comm->recv_sequence = 0;
    comm->procs = NULL;
    comm->last_probed = 0;
    comm->num_procs = 0;
}


static void mca_pml_ob1_comm_destruct(mca_pml_ob1_comm_t* comm)
{
    if (NULL != comm->procs) {
        for (size_t i = 0; i < comm->num_procs; ++i) {
            if (comm->procs[i]) {
                OBJ_RELEASE(comm->procs[i]);
            }
        }

        free ((void *) comm->procs);
    }

#if !MCA_PML_OB1_CUSTOM_MATCH
    OBJ_DESTRUCT(&comm->wild_receives);
#else
    custom_match_prq_destroy(comm->prq);
    custom_match_umq_destroy(comm->umq);
#endif
    OBJ_DESTRUCT(&comm->matching_lock);
    OBJ_DESTRUCT(&comm->proc_lock);
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_comm_t,
    opal_object_t,
    mca_pml_ob1_comm_construct,
    mca_pml_ob1_comm_destruct);


int mca_pml_ob1_comm_init_size (mca_pml_ob1_comm_t* comm, size_t size)
{
    /* send message sequence-number support - sender side */
    comm->procs = (mca_pml_ob1_comm_proc_t **) calloc(size, sizeof (mca_pml_ob1_comm_proc_t *));
    if(NULL == comm->procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    comm->num_procs = size;
    return OMPI_SUCCESS;
}

mca_pml_ob1_comm_proc_t *mca_pml_ob1_peer_create (ompi_communicator_t *comm, mca_pml_ob1_comm_t *pml_comm, int rank)
{
    mca_pml_ob1_comm_proc_t *proc = OBJ_NEW(mca_pml_ob1_comm_proc_t);
    uintptr_t old_proc = 0;

    proc->ompi_proc = ompi_comm_peer_lookup (comm, rank);
    if (OMPI_COMM_IS_GLOBAL_INDEX (comm)) {
	/* the index is global so we can save it on the proc now */
	proc->comm_index = comm->c_index;
    }
    OBJ_RETAIN(proc->ompi_proc);
    /* make sure proc structure is filled in before adding it to the array */
    opal_atomic_wmb ();

    if (!OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_PTR((opal_atomic_intptr_t *) pml_comm->procs + rank, &old_proc,
						(uintptr_t) proc)) {
	/* proc was created by a competing thread. go ahead and throw this one away. */
	OBJ_RELEASE(proc);
	return (mca_pml_ob1_comm_proc_t *) old_proc;
    }

    return proc;
}
