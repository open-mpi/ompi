/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "coll_han.h"
#include "coll_han_dynamic.h"


/*
 *@file
 * Coll han module management file. Used for each new communicator.
 */

/*
 * Local functions
 */
static int han_module_enable(mca_coll_base_module_t * module,
                             struct ompi_communicator_t *comm);
static int mca_coll_han_module_disable(mca_coll_base_module_t * module,
                                       struct ompi_communicator_t *comm);

#define CLEAN_PREV_COLL(HANDLE, NAME)                    \
    do {                                                 \
        (HANDLE)->fallback.NAME.module_fn.NAME = NULL;   \
        (HANDLE)->fallback.NAME.module = NULL;           \
    } while (0)

/*
 * Module constructor
 */
static void han_module_clear(mca_coll_han_module_t *han_module)
{
    CLEAN_PREV_COLL(han_module, allgather);
    CLEAN_PREV_COLL(han_module, allgatherv);
    CLEAN_PREV_COLL(han_module, allreduce);
    CLEAN_PREV_COLL(han_module, barrier);
    CLEAN_PREV_COLL(han_module, bcast);
    CLEAN_PREV_COLL(han_module, reduce);
    CLEAN_PREV_COLL(han_module, gather);
    CLEAN_PREV_COLL(han_module, gatherv);
    CLEAN_PREV_COLL(han_module, scatter);

    han_module->reproducible_reduce = NULL;
    han_module->reproducible_reduce_module = NULL;
    han_module->reproducible_allreduce = NULL;
    han_module->reproducible_allreduce_module = NULL;
}

/*
 * Module constructor
 */
static void mca_coll_han_module_construct(mca_coll_han_module_t * module)
{
    int i;

    module->enabled = true;
    module->recursive_free_depth = 0;
    module->super.coll_module_disable = mca_coll_han_module_disable;
    module->cached_low_comms = NULL;
    module->cached_up_comms = NULL;
    module->cached_vranks = NULL;
    module->cached_topo = NULL;
    module->is_mapbycore = false;
    module->storage_initialized = false;
    for( i = 0; i < NB_TOPO_LVL; i++ ) {
        module->sub_comm[i] = NULL;
    }
    for( i = SELF; i < COMPONENTS_COUNT; i++ ) {
        module->modules_storage.modules[i].module_handler = NULL;
    }

    module->dynamic_errors = 0;

    han_module_clear(module);
}


#define OBJ_RELEASE_IF_NOT_NULL(obj)            \
    do {                                        \
        if (NULL != (obj)) {                    \
            OBJ_RELEASE(obj);                   \
        }                                       \
    } while (0)

/*
 * Module destructor
 */
static void
mca_coll_han_module_destruct(mca_coll_han_module_t * module)
{
    int i;

    module->recursive_free_depth++;
    module->enabled = false;
    /* If the current module is in its caches during its destruction
     * (i.e. last collective used HAN on a subcomm with a fallback
     * on previous components)
     */
    if (module->recursive_free_depth > 1){
        return;
    }

    if (module->cached_low_comms != NULL) {
        for (i = 0; i < COLL_HAN_LOW_MODULES; i++) {
            ompi_comm_free(&(module->cached_low_comms[i]));
            module->cached_low_comms[i] = NULL;
        }
        free(module->cached_low_comms);
        module->cached_low_comms = NULL;
    }
    if (module->cached_up_comms != NULL) {
        for (i = 0; i < COLL_HAN_UP_MODULES; i++) {
            ompi_comm_free(&(module->cached_up_comms[i]));
            module->cached_up_comms[i] = NULL;
        }
        free(module->cached_up_comms);
        module->cached_up_comms = NULL;
    }
    if (module->cached_vranks != NULL) {
        free(module->cached_vranks);
        module->cached_vranks = NULL;
    }
    if (module->cached_topo != NULL) {
        free(module->cached_topo);
        module->cached_topo = NULL;
    }
    for(i=0 ; i<NB_TOPO_LVL ; i++) {
        if(NULL != module->sub_comm[i]) {
            ompi_comm_free(&(module->sub_comm[i]));
        }
    }

    OBJ_RELEASE_IF_NOT_NULL(module->previous_allgather_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_allreduce_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_bcast_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_gather_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_gatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_reduce_module);
    OBJ_RELEASE_IF_NOT_NULL(module->previous_scatter_module);

    han_module_clear(module);
}

OBJ_CLASS_INSTANCE(mca_coll_han_module_t,
                   mca_coll_base_module_t,
                   mca_coll_han_module_construct,
                   mca_coll_han_module_destruct);

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int mca_coll_han_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:han:init_query: pick me! pick me!");
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_han_comm_query(struct ompi_communicator_t * comm, int *priority)
{
    int flag;
    mca_coll_han_module_t *han_module;

    /*
     * If we're intercomm, or if there's only one process in the communicator
     */
    if (OMPI_COMM_IS_INTER(comm)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:han:comm_query (%s/%s): intercomm; disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        return NULL;
    }
    if (1 == ompi_comm_size(comm)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:han:comm_query (%s/%s): comm is too small; disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        return NULL;
    }
    /* Get the priority level attached to this module. If priority is less
     * than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_han_component.han_priority;
    if (mca_coll_han_component.han_priority < 0) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:han:comm_query (%s/%s): priority too low; disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        return NULL;
    }

    han_module = OBJ_NEW(mca_coll_han_module_t);
    if (NULL == han_module) {
        return NULL;
    }

    /* All is good -- return a module */
    han_module->topologic_level = GLOBAL_COMMUNICATOR;

    if (NULL != comm->super.s_info) {
        /* Get the info value disaqualifying coll components */
        opal_cstring_t *info_str;
        opal_info_get(comm->super.s_info, "ompi_comm_coll_han_topo_level",
                      &info_str, &flag);

        if (flag) {
            if (0 == strcmp(info_str->string, "INTER_NODE")) {
                han_module->topologic_level = INTER_NODE;
            } else {
                han_module->topologic_level = INTRA_NODE;
            }
            OBJ_RELEASE(info_str);
        }
    }

    if( !ompi_group_have_remote_peers(comm->c_local_group)
            && INTRA_NODE != han_module->topologic_level ) {
        /* The group only contains local processes, and this is not a
         * intra-node subcomm we created. Disable HAN for now */
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:han:comm_query (%s/%s): comm has only local processes; disqualifying myself",
                            ompi_comm_print_cid(comm), comm->c_name);
        OBJ_RELEASE(han_module);
        return NULL;
    }

    han_module->super.coll_module_enable = han_module_enable;
    han_module->super.coll_alltoall   = NULL;
    han_module->super.coll_alltoallv  = NULL;
    han_module->super.coll_alltoallw  = NULL;
    han_module->super.coll_exscan     = NULL;
    han_module->super.coll_reduce_scatter = NULL;
    han_module->super.coll_scan       = NULL;
    han_module->super.coll_scatterv   = NULL;
    han_module->super.coll_barrier    = mca_coll_han_barrier_intra_dynamic;
    han_module->super.coll_scatter    = mca_coll_han_scatter_intra_dynamic;
    han_module->super.coll_reduce     = mca_coll_han_reduce_intra_dynamic;
    han_module->super.coll_gather     = mca_coll_han_gather_intra_dynamic;
    han_module->super.coll_gatherv    = mca_coll_han_gatherv_intra_dynamic;
    han_module->super.coll_bcast      = mca_coll_han_bcast_intra_dynamic;
    han_module->super.coll_allreduce  = mca_coll_han_allreduce_intra_dynamic;
    han_module->super.coll_allgather  = mca_coll_han_allgather_intra_dynamic;

    if (GLOBAL_COMMUNICATOR == han_module->topologic_level) {
        /* We are on the global communicator, return topological algorithms */
        han_module->super.coll_allgatherv = NULL;
    } else {
        /* We are on a topologic sub-communicator, return only the selector */
        han_module->super.coll_allgatherv = mca_coll_han_allgatherv_intra_dynamic;
    }

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:han:comm_query (%s/%s): pick me! pick me!",
                        ompi_comm_print_cid(comm), comm->c_name);
    return &(han_module->super);
}


/*
 * In this macro, the following variables are supposed to have been declared
 * in the caller:
 * . ompi_communicator_t *comm
 * . mca_coll_han_module_t *han_module
 */
#define HAN_SAVE_PREV_COLL_API(__api)                                   \
    do {                                                                \
        if (!comm->c_coll->coll_ ## __api || !comm->c_coll->coll_ ## __api ## _module) { \
            opal_output_verbose(1, ompi_coll_base_framework.framework_output, \
                                "(%s/%s): no underlying " # __api"; disqualifying myself", \
                                ompi_comm_print_cid(comm), comm->c_name); \
            goto handle_error;                                  \
        }                                                       \
        han_module->previous_ ## __api            = comm->c_coll->coll_ ## __api; \
        han_module->previous_ ## __api ## _module = comm->c_coll->coll_ ## __api ## _module; \
        OBJ_RETAIN(han_module->previous_ ## __api ## _module);  \
    } while(0)

/*
 * Init module on the communicator
 */
static int
han_module_enable(mca_coll_base_module_t * module,
                  struct ompi_communicator_t *comm)
{
    mca_coll_han_module_t * han_module = (mca_coll_han_module_t*) module;

    HAN_SAVE_PREV_COLL_API(allgather);
    HAN_SAVE_PREV_COLL_API(allgatherv);
    HAN_SAVE_PREV_COLL_API(allreduce);
    HAN_SAVE_PREV_COLL_API(barrier);
    HAN_SAVE_PREV_COLL_API(bcast);
    HAN_SAVE_PREV_COLL_API(gather);
    HAN_SAVE_PREV_COLL_API(gatherv);
    HAN_SAVE_PREV_COLL_API(reduce);
    HAN_SAVE_PREV_COLL_API(scatter);

    /* set reproducible algos */
    mca_coll_han_reduce_reproducible_decision(comm, module);
    mca_coll_han_allreduce_reproducible_decision(comm, module);

    return OMPI_SUCCESS;

handle_error:
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allgather_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allgatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allreduce_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_bcast_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_gather_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_gatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_reduce_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_scatter_module);

    return OMPI_ERROR;
}

/*
 * Module disable
 */
static int
mca_coll_han_module_disable(mca_coll_base_module_t * module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_han_module_t * han_module = (mca_coll_han_module_t *) module;

    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allgather_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allgatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_allreduce_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_barrier_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_bcast_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_gather_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_gatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_reduce_module);
    OBJ_RELEASE_IF_NOT_NULL(han_module->previous_scatter_module);

    han_module_clear(han_module);

    return OMPI_SUCCESS;
}


/*
 * Free the han request
 */
int ompi_coll_han_request_free(ompi_request_t ** request)
{
    (*request)->req_state = OMPI_REQUEST_INVALID;
    OBJ_RELEASE(*request);
    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}
