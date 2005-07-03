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
/**
 * @file
 */
#ifndef MCA_PML_OB1_COMM_H
#define MCA_PML_OB1_COMM_H

#include "threads/mutex.h"
#include "threads/condition.h"
#include "mca/ptl/ptl.h"
#include "class/ompi_list.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_ob1_comm_proc_t {
     opal_object_t super;
     uint16_t expected_sequence;    /**< send message sequence number - receiver side */
     ompi_list_t frags_cant_match;  /**< out-of-order fragment queues */
     ompi_list_t specific_receives; /**< queues of unmatched specific receives */
     ompi_list_t unexpected_frags;  /**< unexpected fragment queues */
};
typedef struct mca_pml_ob1_comm_proc_t mca_pml_ob1_comm_proc_t;


/**
 *  Cached on ompi_communicator_t to hold queues/state
 *  used by the PML<->PTL interface for matching logic. 
 */
struct mca_pml_comm_t {
    opal_object_t super;
    mca_ptl_sequence_t recv_sequence;  /**< recv request sequence number - receiver side */
    ompi_mutex_t matching_lock;   /**< matching lock */
    ompi_list_t wild_receives;    /**< queue of unmatched wild (source process not specified) receives */
    mca_pml_ob1_comm_proc_t* procs;
    size_t num_procs;
};
typedef struct mca_pml_comm_t mca_pml_ob1_comm_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_pml_ob1_comm_t);


/**
 * Initialize an instance of mca_pml_ob1_comm_t based on the communicator size.
 *
 * @param  comm   Instance of mca_pml_ob1_comm_t
 * @param  size   Size of communicator 
 * @return        OMPI_SUCCESS or error status on failure.
 */

OMPI_DECLSPEC extern int mca_pml_ob1_comm_init_size(mca_pml_ob1_comm_t* comm, size_t size);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

