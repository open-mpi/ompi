/*
 * $HEADER$
 */

#ifndef LAM_COMMUNICATOR
#define LAM_COMMUNICATOR

#include "mpi.h"
#include "lam/group.h"
#include "lam/threads/mutex.h"
#include "mca/mpi/coll/coll.h"

struct lam_communicator_t {
    char c_name[MPI_MAX_OBJECT_NAME];
    int  c_contextid;
    int  c_refcount;
    int  c_flags;

    lam_group_t c_local_group;
    lam_group_t c_remote_group;

  
  /* Queues */

  /* Attributes */

  /* Topology information */

  /* Error handling */

#if 0
  MPI_Errhandler c_error_handler;
#endif

   /* Hooks for PML to hang things */
    struct mca_pml_comm_t* c_pml_comm;

#if 0
   /* Hooks for collectives to hang things */
    mca_coll_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;
#endif
};

typedef struct lam_communicator_t lam_communicator_t;


#endif /* LAM_COMMUNICATOR_H */

