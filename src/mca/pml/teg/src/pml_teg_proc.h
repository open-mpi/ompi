/*
 * $HEADER$
 */

#ifndef MCA_PML_PROC_H
#define MCA_PML_PROC_H

#include "lam/threads/mutex.h"
#include "mpi/communicator/communicator.h"
#include "mpi/group/group.h"
#include "mpi/proc/proc.h"
#include "pml_ptl_array.h"

/*
 *  Structure associated w/ lam_proc_t that contains data specific
 *  to the PML. Note that this name is not PML specific.
 */

struct mca_pml_proc_t {
   lam_list_item_t super;
   lam_proc_t *proc_lam;
   lam_mutex_t proc_lock;
   mca_ptl_array_t proc_ptl_first;
   mca_ptl_array_t proc_ptl_next;
};
typedef struct mca_pml_proc_t mca_pml_proc_t;


extern lam_class_t mca_pml_teg_proc_t_class;
typedef struct mca_pml_proc_t mca_pml_teg_proc_t;


static inline mca_pml_proc_t* mca_pml_teg_proc_lookup_local(lam_communicator_t* comm, int rank)
{
    lam_proc_t* proc = comm->c_local_group->grp_proc_pointers[rank];
    return proc->proc_pml;
}

static inline mca_pml_proc_t* mca_pml_teg_proc_lookup_remote(lam_communicator_t* comm, int rank)
{
    lam_proc_t* proc = comm->c_remote_group->grp_proc_pointers[rank];
    return proc->proc_pml;
}

#endif

