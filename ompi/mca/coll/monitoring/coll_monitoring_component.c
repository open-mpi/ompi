/*
 * Copyright (c) 2016-2017 Inria.  All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include "coll_monitoring.h"
#include <ompi/constants.h>
#include <ompi/communicator/communicator.h>
#include <ompi/mca/coll/coll.h>
#include <opal/mca/base/mca_base_component_repository.h>

#define MONITORING_SAVE_PREV_COLL_API(__module, __comm, __api)          \
    do {                                                                \
        if( NULL != __comm->c_coll->coll_ ## __api ## _module ) {        \
            __module->real.coll_ ## __api = __comm->c_coll->coll_ ## __api; \
            __module->real.coll_ ## __api ## _module = __comm->c_coll->coll_ ## __api ## _module; \
            OBJ_RETAIN(__module->real.coll_ ## __api ## _module);       \
        } else {                                                        \
            /* If no function previously provided, do not monitor */    \
            __module->super.coll_ ## __api = NULL;                      \
            OPAL_MONITORING_PRINT_WARN("COMM \"%s\": No monitoring available for " \
                                       "coll_" # __api, __comm->c_name); \
        }                                                               \
        if( NULL != __comm->c_coll->coll_i ## __api ## _module ) {       \
            __module->real.coll_i ## __api = __comm->c_coll->coll_i ## __api; \
            __module->real.coll_i ## __api ## _module = __comm->c_coll->coll_i ## __api ## _module; \
            OBJ_RETAIN(__module->real.coll_i ## __api ## _module);      \
        } else {                                                        \
            /* If no function previously provided, do not monitor */    \
            __module->super.coll_i ## __api = NULL;                     \
            OPAL_MONITORING_PRINT_WARN("COMM \"%s\": No monitoring available for " \
                                       "coll_i" # __api, __comm->c_name); \
        }                                                               \
    } while(0)

#define MONITORING_RELEASE_PREV_COLL_API(__module, __comm, __api)       \
    do {                                                                \
        if( NULL != __module->real.coll_ ## __api ## _module ) {        \
            if( NULL != __module->real.coll_ ## __api ## _module->coll_module_disable ) { \
                __module->real.coll_ ## __api ## _module->coll_module_disable(__module->real.coll_ ## __api ## _module, __comm); \
            }                                                           \
            OBJ_RELEASE(__module->real.coll_ ## __api ## _module);      \
            __module->real.coll_ ## __api = NULL;                       \
            __module->real.coll_ ## __api ## _module = NULL;            \
        }                                                               \
        if( NULL != __module->real.coll_i ## __api ## _module ) {       \
            if( NULL != __module->real.coll_i ## __api ## _module->coll_module_disable ) { \
                __module->real.coll_i ## __api ## _module->coll_module_disable(__module->real.coll_i ## __api ## _module, __comm); \
            }                                                           \
            OBJ_RELEASE(__module->real.coll_i ## __api ## _module);     \
            __module->real.coll_i ## __api = NULL;                      \
            __module->real.coll_i ## __api ## _module = NULL;           \
        }                                                               \
    } while(0)

#define MONITORING_SET_FULL_PREV_COLL_API(m, c, operation)      \
    do {                                                        \
        operation(m, c, allgather);                             \
        operation(m, c, allgatherv);                            \
        operation(m, c, allreduce);                             \
        operation(m, c, alltoall);                              \
        operation(m, c, alltoallv);                             \
        operation(m, c, alltoallw);                             \
        operation(m, c, barrier);                               \
        operation(m, c, bcast);                                 \
        operation(m, c, exscan);                                \
        operation(m, c, gather);                                \
        operation(m, c, gatherv);                               \
        operation(m, c, reduce);                                \
        operation(m, c, reduce_scatter);                        \
        operation(m, c, reduce_scatter_block);                  \
        operation(m, c, scan);                                  \
        operation(m, c, scatter);                               \
        operation(m, c, scatterv);                              \
        operation(m, c, neighbor_allgather);                    \
        operation(m, c, neighbor_allgatherv);                   \
        operation(m, c, neighbor_alltoall);                     \
        operation(m, c, neighbor_alltoallv);                    \
        operation(m, c, neighbor_alltoallw);                    \
    } while(0)

#define MONITORING_SAVE_FULL_PREV_COLL_API(m, c)                        \
    MONITORING_SET_FULL_PREV_COLL_API((m), (c), MONITORING_SAVE_PREV_COLL_API)

#define MONITORING_RELEASE_FULL_PREV_COLL_API(m, c)                     \
    MONITORING_SET_FULL_PREV_COLL_API((m), (c), MONITORING_RELEASE_PREV_COLL_API)

static int mca_coll_monitoring_component_open(void)
{
    return OMPI_SUCCESS;
}

static int mca_coll_monitoring_component_close(void)
{
    OPAL_MONITORING_PRINT_INFO("coll_module_close");
    mca_common_monitoring_finalize();
    return OMPI_SUCCESS;
}

static int mca_coll_monitoring_component_init(bool enable_progress_threads,
                                              bool enable_mpi_threads)
{
    OPAL_MONITORING_PRINT_INFO("coll_module_init");
    return mca_common_monitoring_init();
}

static int mca_coll_monitoring_component_register(void)
{
    return OMPI_SUCCESS;
}

static int
mca_coll_monitoring_module_enable(mca_coll_base_module_t*module, struct ompi_communicator_t*comm)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    if( 1 == opal_atomic_add_fetch_32(&monitoring_module->is_initialized, 1) ) {
        MONITORING_SAVE_FULL_PREV_COLL_API(monitoring_module, comm);
        monitoring_module->data = mca_common_monitoring_coll_new(comm);
        OPAL_MONITORING_PRINT_INFO("coll_module_enabled");    
    }
    return OMPI_SUCCESS;
}

static int
mca_coll_monitoring_module_disable(mca_coll_base_module_t*module, struct ompi_communicator_t*comm)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    if( 0 == opal_atomic_sub_fetch_32(&monitoring_module->is_initialized, 1) ) {
        MONITORING_RELEASE_FULL_PREV_COLL_API(monitoring_module, comm);
        mca_common_monitoring_coll_release(monitoring_module->data);
        monitoring_module->data = NULL;
        OPAL_MONITORING_PRINT_INFO("coll_module_disabled");    
    }
    return OMPI_SUCCESS;
}

static int mca_coll_monitoring_ft_event(int state)
{
    switch(state) {
    case OPAL_CRS_CHECKPOINT:
    case OPAL_CRS_CONTINUE:
    case OPAL_CRS_RESTART:
    case OPAL_CRS_TERM:
    default:
        ;
    }
    return OMPI_SUCCESS;
}

static mca_coll_base_module_t*
mca_coll_monitoring_component_query(struct ompi_communicator_t*comm, int*priority)
{
    OPAL_MONITORING_PRINT_INFO("coll_module_query");
    mca_coll_monitoring_module_t*monitoring_module = OBJ_NEW(mca_coll_monitoring_module_t);
    if( NULL == monitoring_module ) return (*priority = -1, NULL);
    
    /* Initialize module functions */
    monitoring_module->super.coll_module_enable  = mca_coll_monitoring_module_enable;
    monitoring_module->super.coll_module_disable = mca_coll_monitoring_module_disable;
    monitoring_module->super.ft_event = mca_coll_monitoring_ft_event;

    /* Initialise module collectives functions */
    /* Blocking functions */
    monitoring_module->super.coll_allgather  = mca_coll_monitoring_allgather;
    monitoring_module->super.coll_allgatherv = mca_coll_monitoring_allgatherv;
    monitoring_module->super.coll_allreduce  = mca_coll_monitoring_allreduce;
    monitoring_module->super.coll_alltoall   = mca_coll_monitoring_alltoall;
    monitoring_module->super.coll_alltoallv  = mca_coll_monitoring_alltoallv;
    monitoring_module->super.coll_alltoallw  = mca_coll_monitoring_alltoallw;
    monitoring_module->super.coll_barrier    = mca_coll_monitoring_barrier;
    monitoring_module->super.coll_bcast      = mca_coll_monitoring_bcast;
    monitoring_module->super.coll_exscan     = mca_coll_monitoring_exscan;
    monitoring_module->super.coll_gather     = mca_coll_monitoring_gather;
    monitoring_module->super.coll_gatherv    = mca_coll_monitoring_gatherv;
    monitoring_module->super.coll_reduce     = mca_coll_monitoring_reduce;
    monitoring_module->super.coll_reduce_scatter = mca_coll_monitoring_reduce_scatter;
    monitoring_module->super.coll_reduce_scatter_block = mca_coll_monitoring_reduce_scatter_block;
    monitoring_module->super.coll_scan       = mca_coll_monitoring_scan;
    monitoring_module->super.coll_scatter    = mca_coll_monitoring_scatter;
    monitoring_module->super.coll_scatterv   = mca_coll_monitoring_scatterv;
    
    /* Nonblocking functions */
    monitoring_module->super.coll_iallgather  = mca_coll_monitoring_iallgather;
    monitoring_module->super.coll_iallgatherv = mca_coll_monitoring_iallgatherv;
    monitoring_module->super.coll_iallreduce  = mca_coll_monitoring_iallreduce;
    monitoring_module->super.coll_ialltoall   = mca_coll_monitoring_ialltoall;
    monitoring_module->super.coll_ialltoallv  = mca_coll_monitoring_ialltoallv;
    monitoring_module->super.coll_ialltoallw  = mca_coll_monitoring_ialltoallw;
    monitoring_module->super.coll_ibarrier    = mca_coll_monitoring_ibarrier;
    monitoring_module->super.coll_ibcast      = mca_coll_monitoring_ibcast;
    monitoring_module->super.coll_iexscan     = mca_coll_monitoring_iexscan;
    monitoring_module->super.coll_igather     = mca_coll_monitoring_igather;
    monitoring_module->super.coll_igatherv    = mca_coll_monitoring_igatherv;
    monitoring_module->super.coll_ireduce     = mca_coll_monitoring_ireduce;
    monitoring_module->super.coll_ireduce_scatter = mca_coll_monitoring_ireduce_scatter;
    monitoring_module->super.coll_ireduce_scatter_block = mca_coll_monitoring_ireduce_scatter_block;
    monitoring_module->super.coll_iscan       = mca_coll_monitoring_iscan;
    monitoring_module->super.coll_iscatter    = mca_coll_monitoring_iscatter;
    monitoring_module->super.coll_iscatterv   = mca_coll_monitoring_iscatterv;

    /* Neighborhood functions */
    monitoring_module->super.coll_neighbor_allgather  = mca_coll_monitoring_neighbor_allgather;
    monitoring_module->super.coll_neighbor_allgatherv = mca_coll_monitoring_neighbor_allgatherv;
    monitoring_module->super.coll_neighbor_alltoall   = mca_coll_monitoring_neighbor_alltoall;
    monitoring_module->super.coll_neighbor_alltoallv  = mca_coll_monitoring_neighbor_alltoallv;
    monitoring_module->super.coll_neighbor_alltoallw  = mca_coll_monitoring_neighbor_alltoallw;
    monitoring_module->super.coll_ineighbor_allgather  = mca_coll_monitoring_ineighbor_allgather;
    monitoring_module->super.coll_ineighbor_allgatherv = mca_coll_monitoring_ineighbor_allgatherv;
    monitoring_module->super.coll_ineighbor_alltoall   = mca_coll_monitoring_ineighbor_alltoall;
    monitoring_module->super.coll_ineighbor_alltoallv  = mca_coll_monitoring_ineighbor_alltoallv;
    monitoring_module->super.coll_ineighbor_alltoallw  = mca_coll_monitoring_ineighbor_alltoallw;

    /* Initialization flag */
    monitoring_module->is_initialized = 0;
    
    *priority = mca_coll_monitoring_component.priority;
    
    return &(monitoring_module->super);
}

mca_coll_monitoring_component_t mca_coll_monitoring_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,
            
            .mca_component_name = "monitoring", /* MCA component name */
            MCA_MONITORING_MAKE_VERSION,
            .mca_open_component = mca_coll_monitoring_component_open,  /* component open */
            .mca_close_component = mca_coll_monitoring_component_close, /* component close */
            .mca_register_component_params = mca_coll_monitoring_component_register
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .collm_init_query = mca_coll_monitoring_component_init,
        .collm_comm_query = mca_coll_monitoring_component_query
    },
    .priority = INT_MAX
};

OBJ_CLASS_INSTANCE(mca_coll_monitoring_module_t,
                   mca_coll_base_module_t,
                   NULL,
                   NULL);

