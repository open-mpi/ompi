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

    lam_group_t *c_local_group;
    lam_group_t *c_remote_group;
  
    /* Queues */

    /* Attributes */

    /* Topology information */

    /* Error handling */

    MPI_Errhandler c_error_handler;

    /* Hooks for PML to hang things */
    void* c_pml_comm;

    /* Hooks for collectives to hang things */

    mca_coll_1_0_0_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;
};
typedef struct lam_communicator_t lam_communicator_t;


/* return pointer to communicator associated with context id cid,
 * No error checking is done*/
static inline lam_communicator_t *get_comm_ptr(uint32_t cid) 
{ 
    /* array of pointers to communicators, indexed by context ID */
    extern lam_communicator_t **lam_cummunicator_ptrs;
    extern uint32_t len_lam_cummunicator_ptrs;
#ifdef LAM_ENABLE_DEBUG
    if(cid >= len_lam_cummunicator_ptrs)
        return (lam_communicator_t *) NULL;
#endif
    return lam_cummunicator_ptrs[cid]; 
}

#endif /* LAM_COMMUNICATOR_H */
