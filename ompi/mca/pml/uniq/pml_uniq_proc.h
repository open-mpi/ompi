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

#include "opal/threads/mutex.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "proc/proc.h"

/* This define has to move outside of this file. Maybe on some configuration file.
 * Anyway by for, for the debugging purpose, here it's quite a safe place.
 */
#define PML_UNIQ_ACCEPT_NEXT_PTL 0

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

   /**
    * A data structure associated with a ompi_proc_t that caches
    * addressing/scheduling attributes for a specific PTL instance
    * that can be used to reach the process.
    */
   struct mca_ptl_proc_t {
      struct mca_ptl_base_peer_t* ptl_peer; /**< PTL addressing info */
      struct mca_pml_base_ptl_t* ptl_base;  /**< PML specific PTL info */
      mca_ptl_base_module_t *ptl;           /**< PTL module */
   };
   typedef struct mca_ptl_proc_t mca_ptl_proc_t;

   /**
    *  Structure associated w/ ompi_proc_t that contains data specific
    *  to the PML. Note that this name is not PML specific.
    */
   struct mca_pml_proc_t {
      opal_list_item_t super;
      ompi_proc_t *proc_ompi;           /**< back-pointer to ompi_proc_t */
      opal_mutex_t proc_lock;           /**< lock to protect against concurrent access */
      mca_ptl_proc_t proc_ptl_first;    /**< ptl for the first fragment */
#if PML_UNIQ_ACCEPT_NEXT_PTL
      mca_ptl_proc_t proc_ptl_next;     /**< ptl for the remaining fragments */
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
      uint32_t proc_ptl_flags;          /**< aggregate ptl flags */
   };
   typedef struct mca_pml_proc_t mca_pml_proc_t;


   OMPI_COMP_EXPORT extern opal_class_t mca_pml_uniq_proc_t_class;
   typedef struct mca_pml_proc_t mca_pml_uniq_proc_t;

   /**
    * Return the mca_pml_proc_t instance cached in the communicators local group.
    * 
    * @param comm   Communicator
    * @param rank   Peer rank
    * @return       mca_pml_proc_t instance
    */

   static inline mca_pml_proc_t* mca_pml_uniq_proc_lookup_local(ompi_communicator_t* comm, int rank)
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

   static inline mca_pml_proc_t* mca_pml_uniq_proc_lookup_remote(ompi_communicator_t* comm, int rank)
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

   static inline struct mca_ptl_base_peer_t*
      mca_pml_uniq_proc_lookup_remote_peer( ompi_communicator_t* comm, 
                                            int rank, 
                                            struct mca_ptl_base_module_t* ptl)
   {
      ompi_proc_t* proc = comm->c_remote_group->grp_proc_pointers[rank];
      mca_pml_proc_t* proc_pml = proc->proc_pml;
      if( proc_pml->proc_ptl_first.ptl == ptl )
         return proc_pml->proc_ptl_first.ptl_peer;
#if PML_UNIQ_ACCEPT_NEXT_PTL
      if( proc_pml->proc_ptl_next.ptl == ptl )
         return proc_pml->proc_ptl_next.ptl_peer;
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
      return NULL;
   }

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

