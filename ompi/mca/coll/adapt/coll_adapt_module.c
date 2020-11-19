/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif /* HAVE_SCHED_H */
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "mpi.h"
#include "opal_stdint.h"
#include "opal/util/os_path.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/proc/proc.h"
#include "coll_adapt.h"

#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_adapt_algorithms.h"
#include "coll_adapt_topocache.h"


/*
 * Local functions
 */

/*
 * Module constructor
 */
static void adapt_module_construct(mca_coll_adapt_module_t * module)
{
    module->topo_cache    = NULL;
    module->adapt_enabled = false;
}

/*
 * Module destructor
 */
static void adapt_module_destruct(mca_coll_adapt_module_t * module)
{
    if (NULL != module->topo_cache) {
        adapt_topology_cache_item_t *item;
        while (NULL != (item = (adapt_topology_cache_item_t*)opal_list_remove_first(module->topo_cache))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(module->topo_cache);
        module->topo_cache = NULL;
    }
    module->adapt_enabled = false;
}


OBJ_CLASS_INSTANCE(mca_coll_adapt_module_t,
            mca_coll_base_module_t,
            adapt_module_construct,
            adapt_module_destruct);

/*
 * In this macro, the following variables are supposed to have been declared
 * in the caller:
 * . ompi_communicator_t *comm
 * . mca_coll_adapt_module_t *adapt_module
 */
#define ADAPT_SAVE_PREV_COLL_API(__api)                                 \
    do {                                                                \
        adapt_module->previous_ ## __api            = comm->c_coll->coll_ ## __api; \
        adapt_module->previous_ ## __api ## _module = comm->c_coll->coll_ ## __api ## _module; \
        if (!comm->c_coll->coll_ ## __api || !comm->c_coll->coll_ ## __api ## _module) { \
            opal_output_verbose(1, ompi_coll_base_framework.framework_output, \
                                "(%d/%s): no underlying " # __api"; disqualifying myself", \
                                comm->c_contextid, comm->c_name); \
            return OMPI_ERROR;                                  \
        }                                                       \
        OBJ_RETAIN(adapt_module->previous_ ## __api ## _module);  \
    } while(0)


/*
 * Init module on the communicator
 */
static int adapt_module_enable(mca_coll_base_module_t * module,
            struct ompi_communicator_t *comm)
{
    mca_coll_adapt_module_t * adapt_module = (mca_coll_adapt_module_t*) module;

    ADAPT_SAVE_PREV_COLL_API(reduce);
    ADAPT_SAVE_PREV_COLL_API(ireduce);

    return OMPI_SUCCESS;
}

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int ompi_coll_adapt_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *ompi_coll_adapt_comm_query(struct ompi_communicator_t * comm,
            int *priority)
{
    mca_coll_adapt_module_t *adapt_module;

    /* If we're intercomm, or if there's only one process in the communicator */
    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:adapt:comm_query (%d/%s): intercomm, "
                            "comm is too small; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    /* Get the priority level attached to this module.
       If priority is less than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_adapt_component.adapt_priority;
    if (mca_coll_adapt_component.adapt_priority < 0) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:adapt:comm_query (%d/%s): priority too low; "
                            "disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    adapt_module = OBJ_NEW(mca_coll_adapt_module_t);
    if (NULL == adapt_module) {
        return NULL;
    }

    /* All is good -- return a module */
    adapt_module->super.coll_module_enable = adapt_module_enable;
    adapt_module->super.ft_event = NULL;
    adapt_module->super.coll_allgather = NULL;
    adapt_module->super.coll_allgatherv = NULL;
    adapt_module->super.coll_allreduce = NULL;
    adapt_module->super.coll_alltoall = NULL;
    adapt_module->super.coll_alltoallw = NULL;
    adapt_module->super.coll_barrier = NULL;
    adapt_module->super.coll_bcast = ompi_coll_adapt_bcast;
    adapt_module->super.coll_exscan = NULL;
    adapt_module->super.coll_gather = NULL;
    adapt_module->super.coll_gatherv = NULL;
    adapt_module->super.coll_reduce = ompi_coll_adapt_reduce;
    adapt_module->super.coll_reduce_scatter = NULL;
    adapt_module->super.coll_scan = NULL;
    adapt_module->super.coll_scatter = NULL;
    adapt_module->super.coll_scatterv = NULL;
    adapt_module->super.coll_ibcast = ompi_coll_adapt_ibcast;
    adapt_module->super.coll_ireduce = ompi_coll_adapt_ireduce;
    adapt_module->super.coll_iallreduce = NULL;

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:adapt:comm_query (%d/%s): pick me! pick me!",
                        comm->c_contextid, comm->c_name);
    return &(adapt_module->super);
}

/*
 * Free ADAPT request
 */
int ompi_coll_adapt_request_free(ompi_request_t ** request)
{
    OMPI_REQUEST_FINI(*request);
    (*request)->req_state = OMPI_REQUEST_INVALID;
    OBJ_RELEASE(*request);
    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}
