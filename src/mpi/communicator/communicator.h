/*
 * $HEADER$
 */

#ifndef LAM_COMMUNICATOR_H
#define LAM_COMMUNICATOR_H

#include "mpi.h"
#include "lam/group.h"
#include "lam/stdint.h"
#include "lam/threads/mutex.h"
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

    struct mca_pml_comm_t* c_pml_comm;

    /* Hooks for collectives to hang things */

    mca_coll_1_0_0_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;
};
typedef struct lam_communicator_t lam_communicator_t;

#endif /* LAM_COMMUNICATOR_H */
