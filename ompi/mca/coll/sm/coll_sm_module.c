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

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_sm.h"


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t module = {

    /* Initialization / finalization functions */

    mca_coll_sm_module_init,
    mca_coll_sm_module_finalize,

    /* Collective function pointers */

    NULL,
    NULL,
    NULL, 
    NULL,
    NULL,
    NULL,
    mca_coll_sm_barrier_intra,
    mca_coll_sm_bcast_intra,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

int mca_coll_sm_param_priority = -1;


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_sm_init_query(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    /* Nothing to do */
  
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_sm_comm_query(struct ompi_communicator_t *comm, int *priority,
                       struct mca_coll_base_comm_t **data)
{
    /* If we're intercomm, or if there's only one process in the
       communicator, we don't want to run */

    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm)) {
	return NULL;
    }

    /* Get our priority */

    if (OMPI_SUCCESS != 
        mca_base_param_lookup_int(mca_coll_sm_param_priority,
                                  priority)) {
        return NULL;
    }
    
    /* We only want to run if all the processes in the communicator
       are on the same node */

    /* Can we get an mpool allocation? */

    /* JMS ... */

#if 1
    return NULL;
#else
    return &module;
#endif
}


/* 
 * Unquery the coll on comm
 */
int mca_coll_sm_comm_unquery(struct ompi_communicator_t *comm,
                             struct mca_coll_base_comm_t *data)
{
    /* JMS */
    /* Remove mpool query, if we got one */

    return OMPI_SUCCESS;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_sm_module_init(struct ompi_communicator_t *comm)
{
    /* JMS */

    return &module;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_sm_module_finalize(struct ompi_communicator_t *comm)
{
    /* JMS */

    return OMPI_SUCCESS;
}


