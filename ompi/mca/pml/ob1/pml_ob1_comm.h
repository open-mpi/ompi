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
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Sandia National Laboratories
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_OB1_COMM_H
#define MCA_PML_OB1_COMM_H

#include "opal/mca/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"

/* NTH: at some point we need to untangle the headers. this declaration is needed
 * for headers included by the custom match code. */
typedef struct mca_pml_ob1_comm_proc_t mca_pml_ob1_comm_proc_t;

#include "custommatch/pml_ob1_custom_match.h"

BEGIN_C_DECLS


struct mca_pml_ob1_comm_proc_t {
    opal_object_t super;
    struct ompi_proc_t* ompi_proc;
    uint16_t expected_sequence;    /**< send message sequence number - receiver side */
    int16_t comm_index;           /**< index of this communicator on the receiver size (-1 - not set) */
    opal_atomic_int32_t send_sequence; /**< send side sequence number */
    struct mca_pml_ob1_recv_frag_t* frags_cant_match;  /**< out-of-order fragment queues */
#if !MCA_PML_OB1_CUSTOM_MATCH
    opal_list_t specific_receives; /**< queues of unmatched specific receives */
    opal_list_t unexpected_frags;  /**< unexpected fragment queues */
#endif
};

OBJ_CLASS_DECLARATION(mca_pml_ob1_comm_proc_t);

#define MCA_PML_OB1_PROC_REQUIRES_EXT_MATCH(proc) (-1 == (proc)->comm_index)

/**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic.
 */
struct mca_pml_comm_t {
    opal_object_t super;
    volatile uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
    opal_mutex_t matching_lock;   /**< matching lock */
#if !MCA_PML_OB1_CUSTOM_MATCH
    opal_list_t wild_receives;    /**< queue of unmatched wild (source process not specified) receives */
#endif
    opal_mutex_t proc_lock;
    mca_pml_ob1_comm_proc_t * volatile * procs;
    size_t num_procs;
    size_t last_probed;
#if MCA_PML_OB1_CUSTOM_MATCH
    custom_match_prq* prq;
    custom_match_umq* umq;
#endif
};
typedef struct mca_pml_comm_t mca_pml_ob1_comm_t;

OBJ_CLASS_DECLARATION(mca_pml_ob1_comm_t);

/**
 * @brief Helper function to allocate/fill in ob1 proc for a comm/rank
 */
mca_pml_ob1_comm_proc_t *mca_pml_ob1_peer_create (ompi_communicator_t *comm, mca_pml_ob1_comm_t *pml_comm, int rank);

static inline mca_pml_ob1_comm_proc_t *mca_pml_ob1_peer_lookup (struct ompi_communicator_t *comm, int rank)
{
    mca_pml_ob1_comm_t *pml_comm = (mca_pml_ob1_comm_t *)comm->c_pml_comm;

    /**
     * We have very few ways to validate the correct, and collective, creation of
     * the communicator, and ensure all processes have the same cid. The least we
     * can do is to check that we are not using a rank that is outside the scope
     * of the communicator.
     */
    if( OPAL_UNLIKELY(rank >= (int)pml_comm->num_procs) ) {
        ompi_rte_abort(-1, "PML OB1 received a message from a rank outside the"
                       " valid range of the communicator. Please submit a bug request!");
    }
    if (OPAL_UNLIKELY(NULL == pml_comm->procs[rank])) {
        mca_pml_ob1_peer_create (comm, pml_comm, rank);
    }

    return pml_comm->procs[rank];
}

/**
 * Initialize an instance of mca_pml_ob1_comm_t based on the communicator size.
 *
 * @param  comm   Instance of mca_pml_ob1_comm_t
 * @param  size   Size of communicator
 * @return        OMPI_SUCCESS or error status on failure.
 */

extern int mca_pml_ob1_comm_init_size(mca_pml_ob1_comm_t* comm, size_t size);

END_C_DECLS
#endif

