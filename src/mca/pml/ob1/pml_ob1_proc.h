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
#ifndef MCA_PML_PROC_H
#define MCA_PML_PROC_H

#include "threads/mutex.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "proc/proc.h"
#include "pml_ob1_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 *  Structure associated w/ ompi_proc_t that contains data specific
 *  to the PML.
 */
struct mca_pml_proc_t {
   ompi_object_t super;
   ompi_proc_t *proc_ompi;             /**< back-pointer to ompi_proc_t */
   ompi_mutex_t proc_lock;             /**< lock to protect against concurrent access */
   int proc_flags;                     /**< prefered method of accessing this peer */
   volatile uint32_t proc_sequence;    /**< sequence number for send */
   mca_pml_ob1_ep_array_t bmi_eager;   /**< array of endpoints to use for first fragments */
   mca_pml_ob1_ep_array_t bmi_send;    /**< array of endpoints to use for remaining fragments */
   mca_pml_ob1_ep_array_t bmi_rdma;    /**< array of endpoints that support (prefer) rdma */
};
typedef struct mca_pml_proc_t mca_pml_ob1_proc_t;

OMPI_COMP_EXPORT OBJ_CLASS_DECLARATION(mca_pml_ob1_proc_t);


/**
 * Return the mca_pml_proc_t instance cached in the communicators local group.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline mca_pml_ob1_proc_t* mca_pml_ob1_proc_lookup_local(ompi_communicator_t* comm, int rank)
{
    return comm->c_local_group->grp_proc_pointers[rank]->proc_pml;
}

/**
 * Return the mca_pml_proc_t instance cached on the communicators remote group.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline mca_pml_ob1_proc_t* mca_pml_ob1_proc_lookup_remote(ompi_communicator_t* comm, int rank)
{
    return comm->c_pml_procs[rank];
}

/**
 * Return the mca_bmi_peer_t instance corresponding to the process/bmi combination.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline struct mca_bmi_base_endpoint_t* mca_pml_ob1_proc_lookup_remote_endpoint(
    ompi_communicator_t* comm, 
    int rank, 
    struct mca_bmi_base_module_t* bmi)
{
    mca_pml_ob1_proc_t* proc = comm->c_pml_procs[rank];
    size_t i, size = mca_pml_ob1_ep_array_get_size(&proc->bmi_eager);
    mca_pml_ob1_endpoint_t* endpoint = proc->bmi_eager.arr_endpoints;
    for(i = 0; i < size; i++) {
        if(endpoint->bmi == bmi) {
            return endpoint->bmi_endpoint;
        }
        endpoint++;
    }
    return NULL;
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

