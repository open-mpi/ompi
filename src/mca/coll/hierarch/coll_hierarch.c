/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_hierarch.h"


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t intra = {

  /* Initialization / finalization functions */

  mca_coll_hierarch_module_init,
  mca_coll_hierarch_module_finalize,

  /* Collective function pointers */
  /* function pointers marked with NULL are not yet implemented
     and will use the functions provided in the basic module */
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  mca_coll_hierarch_barrier_intra,
  mca_coll_hierarch_bcast_intra,
  NULL, 
  NULL, 
  NULL, 
  mca_coll_hierarch_reduce_intra,
  NULL, 
  NULL,
  NULL, 
  NULL
};


/*
 * Linear set of collective algorithms for intercommunicators
 */
static const mca_coll_base_module_1_0_0_t inter = {

  /* Initialization / finalization functions */

  mca_coll_hierarch_module_init,
  mca_coll_hierarch_module_finalize,

  /* Collective function pointers */
  /* No inter-communicator functions are provided at the moment */
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL, 
  NULL,
  NULL, 
  NULL
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_hierarch_init_query(bool *allow_hierarch_user_threads,
                             bool *have_hidden_user_threads)
{
  *allow_hierarch_user_threads = true;
  *have_hidden_user_threads = false;

  /* All done */
  
  return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_hierarch_comm_query(struct ompi_communicator_t *comm, int *priority)
{
  if (OMPI_SUCCESS != mca_base_param_lookup_int(mca_coll_hierarch_priority_param, 
                                                priority)) {
    return NULL;
  }

  return OMPI_COMM_IS_INTER(comm) ? &inter : &intra;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_hierarch_module_init(struct ompi_communicator_t *comm)
{
    mca_base_param_lookup_int(mca_coll_hierarch_verbose_param,
                              &mca_coll_hierarch_verbose);
    if (mca_coll_hierarch_verbose > 0) {
        printf("Hello!  This is the \"hierarch\" coll component.  I'll be your coll component\ntoday.  Please tip your waitresses well.\n");
    }

    return OMPI_COMM_IS_INTER(comm) ? &inter : &intra;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_hierarch_module_finalize(struct ompi_communicator_t *comm)
{
  return OMPI_SUCCESS;
}
