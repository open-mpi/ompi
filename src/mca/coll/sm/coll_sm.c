/*
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
static const mca_coll_base_module_1_0_0_t sm_func_ptrs = {

    /* Initialization / finalization functions */

    mca_coll_sm_module_init,
    mca_coll_sm_module_finalize,

    /* Collective function pointers */

    mca_coll_sm_allgather,
    NULL, /* Goto basic */
    NULL, 
    mca_coll_sm_alltoall,
    NULL,
    NULL,
    mca_coll_sm_barrier,
    mca_coll_sm_bcast,
    NULL,
    mca_coll_sm_gather,
    NULL,
    mca_coll_sm_reduce,
    NULL,
    NULL,
    mca_coll_sm_scatter,
    NULL
};


/* Open function to initialize a few parameters and return success for
 * selection 
 */
int mca_coll_sm_open(void)
{
    /* VPS: Initialize some run-time MCA parameters here - if required
       - such as "sm_message_pool_size" or "sm_number_of_segments",
       that kinda. For example: 
       mca_base_param_register_int("coll", "sm", "message_pool_size",
       NULL, 1024)
    */
    return 1;
}


/* 
 * Anything that needs to be done which is opposite of what was done
 * in open() should be done here. If nothing, just return OMPI_SUCCESS
 */
int mca_coll_sm_close(void)
{
    return OMPI_SUCCESS;
}


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_sm_init_query(bool *allow_multi_user_threads,
			   bool *have_hidden_user_threads)
{
    *allow_multi_user_threads = true;
    *have_hidden_user_threads = false;

    /* VPS: Nothing else needs to be done here */
  
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_sm_comm_query(struct ompi_communicator_t *comm, int *priority)
{

    /* Abort if intercomm */

    if (OMPI_COMM_IS_INTER(comm))
	return NULL;

    /* VPS: Find out whether all the processes in the comm are on the
       same node - if not, return NULL */

    /* VPS: Decide what needs to be done if only one process - if only
       one process, return NULL, so that "basic" module can be
       picked */

    /* VPS: Now check if enough shared memory can be allocated for
       this communicator. First allocate space for the local per
       communicator structure. Lowest ranked process will create the
       area and send the shmid to rest of the processes. All other
       processes will see if they can attach to it. If yes then
       success otherwise they will return NULL
    */

    /* Set the default priority */

    *priority = 50;

    /* VPS: Get "basic" coll module function pointers so that we can
       use them for some collective operations that we may need to do
       now */

    /* VPS: If required allreduce - using basic fn ptrs to confirm
       that all the processes have successfully attached the shared
       memory buffer */

    /* VPS: Return the function pointer struct for basic coll got
       above, the sm coll function pointers will be returned during
       "init" of comm below */
}


/* 
 * Unquery the coll on comm
 */
int 
mca_coll_sm_comm_unquery(struct ompi_communicator_t *comm)
{

    /* VPS: do the reverse of what was done in query. Can do the following
       things: 
       - free the sm coll specific data on the comm
       - remove the shared memory area that was allocated
    */

    return OMPI_SUCCESS;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_sm_module_init(struct ompi_communicator_t *comm)
{
    /* VPS: When this module has been selected, it's all about creating and
     * initializing the module-specific data that hangs off the
     * communicator.
     */

    /* VPS: Allocate and initialize the data that hangs off the
       communicator */

    /* VPS: Now return the SM coll specific function pointers */
}


/*
 * Finalize module on the communicator
 */
int mca_coll_sm_module_finalize(struct ompi_communicator_t *comm)
{
    /* VPS: Free data which was allocated during init */

    /* VPS: Can also inturn call mca_coll_sm_comm_unquery to finalize
       other things */
}


