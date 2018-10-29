/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/coll.h"
#include "coll_spacc.h"

static int spacc_module_enable(mca_coll_base_module_t *module,
                               struct ompi_communicator_t *comm);
/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int ompi_coll_spacc_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *ompi_coll_spacc_comm_query(
    struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_spacc_module_t *spacc_module;

    opal_output_verbose(30, mca_coll_spacc_stream, "coll:spacc:module_comm_query called");

    if (OMPI_COMM_IS_INTER(comm)) {
        opal_output_verbose(20, mca_coll_spacc_stream,
                            "coll:spacc:module_comm_query: spacc does not support inter-communicators");
        *priority = 0;
        return NULL;
    }

    if (OMPI_COMM_IS_INTRA(comm) && ompi_comm_size(comm) < 2) {
        *priority = 0;
        return NULL;
    }

    spacc_module = OBJ_NEW(mca_coll_spacc_module_t);
    if (NULL == spacc_module)
        return NULL;

    *priority = mca_coll_spacc_priority;

    spacc_module->super.coll_module_enable = spacc_module_enable;
    spacc_module->super.ft_event = NULL;
    spacc_module->super.coll_allgather  = NULL;
    spacc_module->super.coll_allgatherv = NULL;
    spacc_module->super.coll_allreduce  = mca_coll_spacc_allreduce_intra_redscat_allgather;
    spacc_module->super.coll_alltoall   = NULL;
    spacc_module->super.coll_alltoallv  = NULL;
    spacc_module->super.coll_alltoallw  = NULL;
    spacc_module->super.coll_barrier    = NULL;
    spacc_module->super.coll_bcast      = NULL;
    spacc_module->super.coll_exscan     = NULL;
    spacc_module->super.coll_gather     = NULL;
    spacc_module->super.coll_gatherv    = NULL;
    spacc_module->super.coll_reduce     = mca_coll_spacc_reduce_intra_redscat_gather;
    spacc_module->super.coll_reduce_scatter_block = NULL;
    spacc_module->super.coll_reduce_scatter = NULL;
    spacc_module->super.coll_scan       = NULL;
    spacc_module->super.coll_scatter    = NULL;
    spacc_module->super.coll_scatterv   = NULL;

    return &(spacc_module->super);
}

/*
 * Init module on the communicator
 */
static int spacc_module_enable(mca_coll_base_module_t *module,
                               struct ompi_communicator_t *comm)
{
    opal_output_verbose(30, mca_coll_spacc_stream, "coll:spacc:module_enable called");
    /* prepare the placeholder for the array of request* */
    module->base_data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == module->base_data) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

static void mca_coll_spacc_module_construct(mca_coll_spacc_module_t *module)
{
    /* mca_coll_spacc_module_t *spacc_module = (mca_coll_spacc_module_t*)module; */
}

OBJ_CLASS_INSTANCE(mca_coll_spacc_module_t, mca_coll_base_module_t,
                   mca_coll_spacc_module_construct, NULL);
