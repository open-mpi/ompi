/*
 * $HEADER$
 */

#include "lam_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_basic.h"


/*
 * Linear set of collective algorithms
 */
static const mca_coll_1_0_0_t linear = {

  /* Per-communicator initialization and finalization functions */

  mca_coll_basic_init,
  mca_coll_basic_finalize,

  /* Checkpoint / restart functions */

  NULL,
  NULL,
  NULL,
  NULL,

  /* Memory allocation / freeing */

  NULL,
  NULL,

  /* Collective function pointers */

  mca_coll_basic_allgather,
  NULL,

  mca_coll_basic_allgatherv,
  NULL,

  mca_coll_basic_allreduce,
  NULL,
  
  mca_coll_basic_alltoall,
  NULL,

  mca_coll_basic_alltoallv,
  NULL,

  mca_coll_basic_alltoallw,
  NULL, 

  mca_coll_basic_barrier_lin,
  NULL,

  true,
  mca_coll_basic_bcast_lin,
  NULL, 

  mca_coll_basic_exscan,
  NULL, 

  mca_coll_basic_gather,
  NULL,

  mca_coll_basic_gatherv,
  NULL,

  true,
  mca_coll_basic_reduce_lin,
  NULL,

  mca_coll_basic_reduce_scatter,
  NULL,

  mca_coll_basic_scan,
  NULL,

  mca_coll_basic_scatter,
  NULL,

  mca_coll_basic_scatterv,
  NULL
};


/*
 * Lograthmic set of collective algorithms.  Note that not all
 * collectives have lograthmic algorithms.  For example, scan will use
 * the same algorithm as in the linear set.
 */
static const mca_coll_1_0_0_t log = {

  /* Per-communicator initialization and finalization functions */

  mca_coll_basic_init,
  mca_coll_basic_finalize,

  /* Checkpoint / restart functions */

  NULL,
  NULL,
  NULL,
  NULL,

  /* Memory allocation / freeing */

  NULL,
  NULL,

  /* Collective function pointers */

  mca_coll_basic_allgather,
  NULL,

  mca_coll_basic_allgatherv,
  NULL,

  mca_coll_basic_allreduce,
  NULL,

  mca_coll_basic_alltoall,
  NULL,

  mca_coll_basic_alltoallv,
  NULL,

  mca_coll_basic_alltoallw,
  NULL, 

  mca_coll_basic_barrier_log,
  NULL,

  true,
  mca_coll_basic_bcast_log,
  NULL,

  mca_coll_basic_exscan,
  NULL, 

  mca_coll_basic_gather,
  NULL,

  mca_coll_basic_gatherv,
  NULL,

  true,
  mca_coll_basic_reduce_log,
  NULL,

  mca_coll_basic_reduce_scatter,
  NULL,

  mca_coll_basic_scan,
  NULL,

  mca_coll_basic_scatter,
  NULL,

  mca_coll_basic_scatterv,
  NULL
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_basic_init_query(int *thread_min, int *thread_max)
{
  *thread_min = MPI_THREAD_SINGLE;
  *thread_max = MPI_THREAD_MULTIPLE;
  
  return LAM_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_1_0_0_t *mca_coll_basic_comm_query(MPI_Comm comm, int *priority)
{
#if 1
  /* JMS fix me */
  *priority = 0;
  return &linear;
#else
  int size;

  /* This module should always have the lowest available priority */

  *priority = 0;

  /* Choose whether to use linear or log-based algorithms. */

  MPI_Comm_size(comm, &size);
  if (size <= mca_coll_base_crossover) {
    return &linear;
  } else {
    return &log;
  }
#endif
}


/*
 * Init on the communicator
 */
int mca_coll_basic_init(MPI_Comm comm, const mca_coll_1_0_0_t **new_coll)
{
  /* Nothing to init on the communicator */

  return LAM_SUCCESS;
}


/*
 * Finalize on the communicator
 */
int mca_coll_basic_finalize(MPI_Comm comm)
{
  /* Nothing to finalize on the communicator */

  return LAM_SUCCESS;
}
