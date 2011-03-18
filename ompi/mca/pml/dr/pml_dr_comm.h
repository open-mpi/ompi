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
/**
 * @file
 */
#ifndef MCA_PML_DR_COMM_H
#define MCA_PML_DR_COMM_H

#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "pml_dr_endpoint.h"
BEGIN_C_DECLS


struct mca_pml_dr_comm_proc_t {
    opal_object_t super;
    uint16_t expected_sequence;    /**< send message sequence number - receiver side */
#if OPAL_ENABLE_MULTI_THREADS
    volatile int32_t send_sequence; /**< send side sequence number */
#else
    int32_t send_sequence; /**< send side sequence number */
#endif
    opal_list_t frags_cant_match;  /**< out-of-order fragment queues */
    opal_list_t specific_receives; /**< queues of unmatched specific receives */
    opal_list_t unexpected_frags;  /**< unexpected fragment queues */
    opal_list_t matched_receives;  /**< list of in-progress matched receives */
    ompi_proc_t* ompi_proc;        /**< back pointer to ompi_proc_t */
    mca_pml_dr_endpoint_t* pml_endpoint; /**< back pointer to the PML endpoint */
    mca_bml_base_endpoint_t* bml_endpoint; /**< back pointer to the BML endpoint */
    int32_t comm_rank;               /**< rank in the communicator */
};
typedef struct mca_pml_dr_comm_proc_t mca_pml_dr_comm_proc_t;

    /**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic. 
 */
struct mca_pml_comm_t {
    opal_object_t super;
#if OPAL_ENABLE_MULTI_THREADS
    volatile uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#else
    uint32_t recv_sequence;  /**< recv request sequence number - receiver side */
#endif
    opal_mutex_t matching_lock;   /**< matching lock */
    opal_list_t wild_receives;    /**< queue of unmatched wild (source process not specified) receives */
    opal_pointer_array_t sparse_procs;   /**< sparse array, allows lookup of comm_proc using a global rank */  
    mca_pml_dr_comm_proc_t* procs;
    size_t num_procs;
};
typedef struct mca_pml_comm_t mca_pml_dr_comm_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_comm_t);


/**
 * Initialize an instance of mca_pml_dr_comm_t based on the communicator size.
 *
 * @param  dr_comm   Instance of mca_pml_dr_comm_t
 * @param  pml_comm  Communicator 
 * @return           OMPI_SUCCESS or error status on failure.
 */

extern int mca_pml_dr_comm_init(mca_pml_dr_comm_t* dr_comm, ompi_communicator_t* ompi_comm);


END_C_DECLS
#endif

