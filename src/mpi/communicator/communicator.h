/*
 * $HEADER$
 */

#ifndef LAM_COMMUNICATOR_H
#define LAM_COMMUNICATOR_H

#include "lam/stdint.h"
#include "lam/threads/mutex.h"
#include "mpi.h"
#include "mpi/group/group.h"
#include "mca/mpi/coll/coll.h"


struct lam_communicator_t {
    char c_name[MPI_MAX_OBJECT_NAME];
    uint32_t c_contextid;
    int c_refcount;
    int c_flags;
    size_t c_rank; /* local rank */

    lam_group_t *c_local_group;
    lam_group_t *c_remote_group;
  
    /* Queues */

    /* Attributes */

    /* Topology information */

    /* Error handling */

    MPI_Errhandler c_error_handler;

    /* Hooks for PML to hang things */
    struct mca_pml_comm_t* c_pml_comm;

    /* Hooks for collectives to hang things */

    mca_coll_1_0_0_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;
};
typedef struct lam_communicator_t lam_communicator_t;


/* return pointer to communicator associated with context id cid,
 * No error checking is done*/
static inline lam_communicator_t *lam_comm_lookup(uint32_t cid) 
{ 
    /* array of pointers to communicators, indexed by context ID */
    extern lam_communicator_t **lam_mpi_comm_array;
#ifdef LAM_ENABLE_DEBUG
    extern uint32_t lam_mpi_comm_array_size;
    if(cid >= lam_mpi_comm_array_size) {
        lam_output(0, "lam_comm_lookup: invalid communicator index (%d)", cid);
        return (lam_communicator_t *) NULL;
    }
#endif
    return lam_mpi_comm_array[cid]; 
}

static inline lam_proc_t* lam_comm_lookup_peer(lam_communicator_t* comm, size_t peer_id)
{
#ifdef LAM_ENABLE_DEBUG
    if(peer_id >= comm->c_remote_group->g_proc_count) {
        lam_output(0, "lam_comm_lookup_peer: invalid peer index (%d)", peer_id);
        return (lam_proc_t *) NULL;
    }
#endif
    return comm->c_remote_group->g_procs[peer_id];
}


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int lam_comm_init(lam_communicator_t *comm);
  int lam_comm_link_function(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_COMMUNICATOR_H */
