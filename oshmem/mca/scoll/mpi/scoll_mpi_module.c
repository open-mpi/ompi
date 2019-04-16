/*
 * Copyright (c) 2011-2018 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "scoll_mpi.h"
#include "opal/util/show_help.h"

#include "oshmem/proc/proc.h"
#include "oshmem/runtime/runtime.h"
#include "ompi/mca/coll/base/base.h"
#include "opal/util/timings.h"

int mca_scoll_mpi_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OSHMEM_SUCCESS;
}

static void mca_scoll_mpi_module_clear(mca_scoll_mpi_module_t *mpi_module)
{
    mpi_module->previous_barrier      = NULL;
    mpi_module->previous_broadcast    = NULL;
    mpi_module->previous_reduce       = NULL;
    mpi_module->previous_collect      = NULL;
    mpi_module->previous_alltoall     = NULL;
}

static void mca_scoll_mpi_module_construct(mca_scoll_mpi_module_t *mpi_module)
{
    mca_scoll_mpi_module_clear(mpi_module);
}

static void mca_scoll_mpi_module_destruct(mca_scoll_mpi_module_t *mpi_module)
{

    OBJ_RELEASE(mpi_module->previous_barrier_module);
    OBJ_RELEASE(mpi_module->previous_broadcast_module);
    OBJ_RELEASE(mpi_module->previous_reduce_module);
    OBJ_RELEASE(mpi_module->previous_collect_module);
    OBJ_RELEASE(mpi_module->previous_alltoall_module);

    mca_scoll_mpi_module_clear(mpi_module);
    /* Free ompi_comm */
    if (mpi_module->comm != &(ompi_mpi_comm_world.comm) && (NULL != mpi_module->comm)) {
        ompi_comm_free(&mpi_module->comm);
    }
}

#define MPI_SAVE_PREV_SCOLL_API(__api) do {\
    mpi_module->previous_ ## __api            = osh_group->g_scoll.scoll_ ## __api;\
    mpi_module->previous_ ## __api ## _module = osh_group->g_scoll.scoll_ ## __api ## _module;\
    if (!osh_group->g_scoll.scoll_ ## __api || !osh_group->g_scoll.scoll_ ## __api ## _module) {\
        MPI_COLL_VERBOSE(1, "no underlying " # __api"; disqualifying myself");\
        return OSHMEM_ERROR;\
    }\
    OBJ_RETAIN(mpi_module->previous_ ## __api ## _module);\
} while(0)

static int mca_scoll_mpi_save_coll_handlers(mca_scoll_base_module_t *module, oshmem_group_t *osh_group)
{
    mca_scoll_mpi_module_t* mpi_module = (mca_scoll_mpi_module_t*) module;
    MPI_SAVE_PREV_SCOLL_API(barrier);
    MPI_SAVE_PREV_SCOLL_API(broadcast);
    MPI_SAVE_PREV_SCOLL_API(reduce);
    MPI_SAVE_PREV_SCOLL_API(collect);
    MPI_SAVE_PREV_SCOLL_API(alltoall);
    return OSHMEM_SUCCESS;
}

/*
 * Initialize module on the communicator
 */
static int mca_scoll_mpi_module_enable(mca_scoll_base_module_t *module,
                                        oshmem_group_t *osh_group)
{

    if (OSHMEM_SUCCESS != mca_scoll_mpi_save_coll_handlers(module, osh_group)){
        MPI_COLL_ERROR("MPI module enable failed - aborting to prevent inconsistent application state");
        /* There's no modules available */
        opal_show_help("help-oshmem-scoll-mpi.txt",
                       "module_enable:fatal", true,
		       		   "MPI module enable failed - aborting to prevent inconsistent application state");

        oshmem_shmem_abort(-1);
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}



/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_scoll_base_module_t *
mca_scoll_mpi_comm_query(oshmem_group_t *osh_group, int *priority)
{
    mca_scoll_base_module_t *module;
    mca_scoll_mpi_module_t *mpi_module;
    int err, i;
    int tag;
    ompi_group_t* world_group, *new_group;
    ompi_communicator_t* newcomm = NULL;
    *priority = 0;
    mca_scoll_mpi_component_t *cm;
    cm = &mca_scoll_mpi_component;
    int* ranks;
    if (!cm->mpi_enable){
        return NULL;
    }
    if ((osh_group->proc_count < 2) || (osh_group->proc_count < cm->mpi_np)) {
        return NULL;
    }
    OPAL_TIMING_ENV_INIT(comm_query);

    /* Create OMPI_Comm object and store ptr to it in group obj*/
    if (NULL == oshmem_group_all) {
        osh_group->ompi_comm = &(ompi_mpi_comm_world.comm);
        OPAL_TIMING_ENV_NEXT(comm_query, "ompi_mpi_comm_world");
    } else {
        err = ompi_comm_group(&(ompi_mpi_comm_world.comm), &world_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            return NULL;
        }
        OPAL_TIMING_ENV_NEXT(comm_query, "ompi_comm_group");

        ranks = (int*) malloc(osh_group->proc_count * sizeof(int));
        if (OPAL_UNLIKELY(NULL == ranks)) {
            return NULL;
        }
        tag = 1;

        OPAL_TIMING_ENV_NEXT(comm_query, "malloc");

        /* Fill the map "group_rank-to-world_rank" in order to create a new proc group */
        for (i = 0; i < osh_group->proc_count; i++) {
            ranks[i] = osh_group->proc_array[i]->super.proc_name.vpid;
        }

        OPAL_TIMING_ENV_NEXT(comm_query, "build_ranks");

        err = ompi_group_incl(world_group, osh_group->proc_count, ranks, &new_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }
        OPAL_TIMING_ENV_NEXT(comm_query, "ompi_group_incl");

        err = ompi_comm_create_group(&(ompi_mpi_comm_world.comm), new_group, tag, &newcomm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }
        OPAL_TIMING_ENV_NEXT(comm_query, "ompi_comm_create_group");

        err = ompi_group_free(&new_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }
        OPAL_TIMING_ENV_NEXT(comm_query, "ompi_group_free");

        free(ranks);
        osh_group->ompi_comm = newcomm;
        OPAL_TIMING_ENV_NEXT(comm_query, "set_group_comm");
    }
    mpi_module = OBJ_NEW(mca_scoll_mpi_module_t);
    if (!mpi_module){
        return NULL;
    }
    mpi_module->comm = osh_group->ompi_comm;

    mpi_module->super.scoll_module_enable = mca_scoll_mpi_module_enable;
    mpi_module->super.scoll_barrier = mca_scoll_mpi_barrier;
    mpi_module->super.scoll_broadcast = mca_scoll_mpi_broadcast;
    mpi_module->super.scoll_reduce = mca_scoll_mpi_reduce;
    mpi_module->super.scoll_collect = mca_scoll_mpi_collect;
    mpi_module->super.scoll_alltoall = NULL;

    *priority = cm->mpi_priority;
    module = &mpi_module->super;

    return module;
}


OBJ_CLASS_INSTANCE(mca_scoll_mpi_module_t,
        mca_scoll_base_module_t,
        mca_scoll_mpi_module_construct,
        mca_scoll_mpi_module_destruct);



