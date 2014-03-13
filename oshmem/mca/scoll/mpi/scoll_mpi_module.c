/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

 $HEADER$
 */

#include "ompi_config.h"
#include "scoll_mpi.h"
#include "oshmem/proc/proc.h"
#include "ompi/mca/coll/base/base.h"

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
    return OSHMEM_SUCCESS;
}

/*
 * Initialize module on the communicator
 */
static int mca_scoll_mpi_module_enable(mca_scoll_base_module_t *module,
                                        oshmem_group_t *osh_group)
{

    if (OSHMEM_SUCCESS != mca_scoll_mpi_save_coll_handlers(module, osh_group)){
        MPI_COLL_ERROR("scoll_mpi: mca_coll_mpi_save_coll_handlers failed");
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
    ompi_group_t* parent_group, *new_group;
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
    /* Create OMPI_Comm object and store ptr to it in group obj*/
    if (NULL == oshmem_group_all) {
        osh_group->ompi_comm = &(ompi_mpi_comm_world.comm);
    } else {
        err = ompi_comm_group(&(ompi_mpi_comm_world.comm), &parent_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            return NULL;
        }
        ranks = (int*) malloc(osh_group->proc_count * sizeof(int));
        if (OPAL_UNLIKELY(NULL == ranks)) {
            return NULL;
        }
        tag = 1;

        for (i = 0; i < osh_group->proc_count; i++) {
            ranks[i] = osh_group->proc_array[i]->proc_name.vpid;
        }

        err = ompi_group_incl(parent_group, osh_group->proc_count, ranks, &new_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }
        err = ompi_comm_create_group(&(ompi_mpi_comm_world.comm), new_group, tag, &newcomm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }
        err = ompi_group_free(&new_group);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != err)) {
            free(ranks);
            return NULL;
        }

        free(ranks);
        osh_group->ompi_comm = newcomm;
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

    *priority = cm->mpi_priority;
    module = &mpi_module->super;

    return module;
}


OBJ_CLASS_INSTANCE(mca_scoll_mpi_module_t,
        mca_scoll_base_module_t,
        mca_scoll_mpi_module_construct,
        mca_scoll_mpi_module_destruct);



