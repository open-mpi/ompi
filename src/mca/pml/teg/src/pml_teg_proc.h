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
/**
 * @file
 */
#ifndef MCA_PML_PROC_H
#define MCA_PML_PROC_H

#include "threads/mutex.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "proc/proc.h"
#include "pml_ptl_array.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 *  Structure associated w/ ompi_proc_t that contains data specific
 *  to the PML. Note that this name is not PML specific.
 */
struct mca_pml_proc_t {
   ompi_list_item_t super;
   ompi_proc_t *proc_ompi;           /**< back-pointer to ompi_proc_t */
   ompi_mutex_t proc_lock;           /**< lock to protect against concurrent access */
   mca_ptl_array_t proc_ptl_first;   /**< array of ptls to use for first fragments */
   mca_ptl_array_t proc_ptl_next;    /**< array of ptls to use for remaining fragments */
   uint32_t proc_ptl_flags;          /**< aggregate ptl flags */
};
typedef struct mca_pml_proc_t mca_pml_proc_t;


OMPI_COMP_EXPORT extern ompi_class_t mca_pml_teg_proc_t_class;
typedef struct mca_pml_proc_t mca_pml_teg_proc_t;

/**
 * Return the mca_pml_proc_t instance cached in the communicators local group.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline mca_pml_proc_t* mca_pml_teg_proc_lookup_local(ompi_communicator_t* comm, int rank)
{
    ompi_proc_t* proc = comm->c_local_group->grp_proc_pointers[rank];
    return proc->proc_pml;
}

/**
 * Return the mca_pml_proc_t instance cached on the communicators remote group.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline mca_pml_proc_t* mca_pml_teg_proc_lookup_remote(ompi_communicator_t* comm, int rank)
{
    ompi_proc_t* proc = comm->c_remote_group->grp_proc_pointers[rank];
    return proc->proc_pml;
}

/**
 * Return the mca_ptl_peer_t instance corresponding to the process/ptl combination.
 * 
 * @param comm   Communicator
 * @param rank   Peer rank
 * @return       mca_pml_proc_t instance
 */

static inline struct mca_ptl_base_peer_t* mca_pml_teg_proc_lookup_remote_peer(
    ompi_communicator_t* comm, 
    int rank, 
    struct mca_ptl_base_module_t* ptl)
{
    ompi_proc_t* proc = comm->c_local_group->grp_proc_pointers[rank];
    mca_pml_proc_t* proc_pml = proc->proc_pml;
    size_t i, size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
    mca_ptl_proc_t* proc_ptl = proc_pml->proc_ptl_first.ptl_procs;
    for(i = 0; i < size; i++) {
        if(proc_ptl->ptl == ptl) {
            return proc_ptl->ptl_peer;
        }
        proc_ptl++;
    }
    return NULL;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

