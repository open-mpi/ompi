/**
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "scoll_fca.h"
#include <stdio.h>
#include <unistd.h>
#include "oshmem/constants.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/runtime/runtime.h"

/*
 *  * Initial query function that is invoked during MPI_INIT, allowing
 *   * this module to indicate what level of thread support it provides.
 *    */

static const int root_id = 0;

#define __INTERNAL_BARRIER_FROM_SCOLL_BASIC 1
static int _internal_barrier(mca_scoll_fca_module_t *fca_module)
{
#if !__INTERNAL_BARRIER_FROM_SCOLL_BASIC
    struct oshmem_group_t *group = fca_module->comm;
    int rc = OSHMEM_SUCCESS;
    int root_id = 0;
    int PE_root = oshmem_proc_pe(group->proc_array[root_id]);
    int i = 0;

    if (PE_root != group->my_pe)
    {
        rc = MCA_SPML_CALL(send(NULL, 0, PE_root, MCA_SPML_BASE_PUT_STANDARD));
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }

        rc = MCA_SPML_CALL(recv(NULL, 0, PE_root));
        if (OSHMEM_SUCCESS != rc) {
            return rc;
        }
    }

    /* The root collects and broadcasts the messages. */

    else
    {
        int pe_cur = 0;

        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++)
        {
            pe_cur = oshmem_proc_pe(group->proc_array[i]);
            if (pe_cur != PE_root)
            {
                rc = MCA_SPML_CALL(recv(NULL, 0, SHMEM_ANY_SOURCE));
            }
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }

        for (i = 0; (i < group->proc_count) && (rc == OSHMEM_SUCCESS); i++)
        {
            pe_cur = oshmem_proc_pe(group->proc_array[i]);
            if (pe_cur != PE_root)
            {
                rc = MCA_SPML_CALL(send(NULL, 0, pe_cur, MCA_SPML_BASE_PUT_STANDARD));
            }
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }
    }

    return rc;
#else
    long pSync = _SHMEM_SYNC_VALUE;
    /*we use 4th algorithm for barrier from scoll/basic.  It does not use pSync, 
     * so we pass to that function just regular long value in order to meet function defenition requirements*/
    return fca_module->previous_barrier(fca_module->comm,
            &pSync,
            SCOLL_ALG_BARRIER_BASIC);
#endif
}

int mca_scoll_fca_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    return OSHMEM_SUCCESS;
}

static int have_remote_peers(struct oshmem_group_t *group,
                             size_t size,
                             int *local_peers)
{
    struct oshmem_proc_t *proc;
    size_t i;
    int ret;

    *local_peers = 0;
    ret = 0;
    for (i = 0; i < size; ++i) {
        proc = group->proc_array[i];
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            ++*local_peers;
        } else {
            ret = 1;
        }
    }
    return ret;
}

/**
 *  * Fills local rank information in fca_module.
 *   */

static int _get_local_ranks(mca_scoll_fca_module_t *fca_module)
{
    struct oshmem_group_t *comm = fca_module->comm;
    oshmem_proc_t* proc;
    int i, rank;

    /* Count the local ranks */
    fca_module->num_local_procs = 0;
    for (rank = 0; rank < comm->proc_count; ++rank) {
        proc = comm->proc_array[rank];
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            if (proc->super.proc_name.vpid == (uint32_t) fca_module->rank) {
                fca_module->local_proc_idx = fca_module->num_local_procs;
            }
            ++fca_module->num_local_procs;
        }
    }
    /* Make a list of local ranks */
    fca_module->local_ranks = calloc(fca_module->num_local_procs,
                                     sizeof *fca_module->local_ranks);
    if (!fca_module->local_ranks) {
        FCA_ERROR("Failed to allocate memory for %d local ranks",
                  fca_module->num_local_procs);
        return OSHMEM_ERROR;
    }

    i = 0;
    for (rank = 0; rank < comm->proc_count; ++rank) {
        proc = comm->proc_array[rank];
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            fca_module->local_ranks[i++] = rank;
        }
    }

    FCA_MODULE_VERBOSE(fca_module,
                       3,
                       "i am %d/%d",
                       fca_module->local_proc_idx, fca_module->num_local_procs);

    return OSHMEM_SUCCESS;
}

static int _fca_comm_new(mca_scoll_fca_module_t *fca_module)
{
    struct oshmem_group_t *comm = fca_module->comm;
    fca_comm_new_spec_t spec;
    int info_size = 0, all_info_size = 0;
    void *all_info = NULL, *my_info = NULL;
    int *disps = NULL;
    int i;
    const int root_pe = oshmem_proc_pe(comm->proc_array[root_id]);
    const int my_id = oshmem_proc_group_find_id(comm, comm->my_pe);
    /* call fca_get_rank_info() on node managers only*/

    if (fca_module->local_proc_idx == 0) {
        my_info = fca_get_rank_info(mca_scoll_fca_component.fca_context,
                                    &info_size);
        if (!my_info) {
            FCA_ERROR("fca_get_rank_info returned NULL");
            return OSHMEM_ERROR;
        }

    } else {
        info_size = 0;
    }

    FCA_MODULE_VERBOSE(fca_module, 1, "Info size: %d", info_size);
    for (i = 0; i < comm->proc_count; i++) {
        mca_scoll_fca_component.rcounts[i] = -1;
    }
    _internal_barrier(fca_module);
    MCA_SPML_CALL(put((void *)&mca_scoll_fca_component.rcounts[my_id], (size_t)sizeof(info_size), (void *)&info_size, root_pe));

    if (root_pe == comm->my_pe) {
        int value = -1;
        for (i = 0; i < comm->proc_count; i++) {
            MCA_SPML_CALL(wait((void *)&mca_scoll_fca_component.rcounts[i], SHMEM_CMP_NE, &value, SHMEM_INT));
        }
    }

    /* Allocate buffer for gathering rank information on rank0 */
    if (root_pe == comm->my_pe) {
        all_info_size = 0;
        disps = calloc(comm->proc_count, sizeof *disps);
        for (i = 0; i < comm->proc_count; ++i) {
            disps[i] = all_info_size;
            all_info_size += mca_scoll_fca_component.rcounts[i];
        }
        all_info = NULL;
        FCA_MODULE_VERBOSE(fca_module,
                           1,
                           "Total rank_info size: %d",
                           all_info_size);
        all_info = malloc(all_info_size);
        memset(all_info, 0, all_info_size);
    }

    if (my_info) {
        memcpy(mca_scoll_fca_component.my_info_exchangeable,
               my_info,
               info_size);
    }
    _internal_barrier(fca_module);
    if (root_pe == comm->my_pe) {
        for (i = 0; i < comm->proc_count; i++) {
            if (mca_scoll_fca_component.rcounts[i] > 0) {
                MCA_SPML_CALL(get((void *)mca_scoll_fca_component.my_info_exchangeable, mca_scoll_fca_component.rcounts[i], (void*)(((char*)all_info)+disps[i]),comm->proc_array[i]->super.proc_name.vpid));
            }
        }
    }

    /* Rank0 calls fca_comm_new() and fills fca_comm_spec filed */
    if (root_pe == comm->my_pe) {
        spec.rank_info = all_info;
        spec.is_comm_world = comm == oshmem_group_all;
        spec.rank_count = 0;
        for (i = 0; i < comm->proc_count; ++i) {
            FCA_MODULE_VERBOSE(fca_module,
                               1,
                               "rcounts[%d]=%d disps[%d]=%d",
                               i, mca_scoll_fca_component.rcounts[i], i, disps[i]);
            if (mca_scoll_fca_component.rcounts[i] > 0)
                ++spec.rank_count;
        }

        FCA_MODULE_VERBOSE(fca_module,
                           1,
                           "starting fca_comm_new(), rank_count: %d",
                           spec.rank_count);

        *mca_scoll_fca_component.ret =
                fca_comm_new(mca_scoll_fca_component.fca_context,
                             &spec,
                             &fca_module->fca_comm_desc);

        free(disps);
        free(all_info);
    }

    _internal_barrier(fca_module);

    if (root_pe != comm->my_pe) {
        MCA_SPML_CALL(get((void *)mca_scoll_fca_component.ret,sizeof(int), (void *)mca_scoll_fca_component.ret, root_pe));
    }

    /* Examine comm_new return value */
    _internal_barrier(fca_module);
    if (*mca_scoll_fca_component.ret < 0) {
        FCA_ERROR("rank %i: COMM_NEW failed: %s",
                  fca_module->rank, fca_strerror(*mca_scoll_fca_component.ret));
        return OSHMEM_ERROR;
    }

    /* Release allocate rank_info on node managers */
    if (fca_module->local_proc_idx == 0) {
        fca_free_rank_info(my_info);
    }

    {
        if (root_pe == comm->my_pe) {
            memcpy(mca_scoll_fca_component.fca_comm_desc_exchangeable,
                   &fca_module->fca_comm_desc,
                   sizeof(fca_module->fca_comm_desc));
        }

        _internal_barrier(fca_module);
        if (root_pe != comm->my_pe) {
            MCA_SPML_CALL(get((void *)mca_scoll_fca_component.fca_comm_desc_exchangeable, sizeof(fca_module->fca_comm_desc), (void *)&fca_module->fca_comm_desc, root_pe));
        }

        _internal_barrier(fca_module);

    }
    FCA_MODULE_VERBOSE(fca_module,
                       1,
                       "Received FCA communicator spec, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
    return OSHMEM_SUCCESS;
}

static int _create_fca_comm(mca_scoll_fca_module_t *fca_module)
{
    int comm_size;
    int rc, ret;

    rc = _fca_comm_new(fca_module);
    if (rc != OSHMEM_SUCCESS)
        return rc;

    /* allocate comm_init_spec */
    FCA_MODULE_VERBOSE(fca_module,
                       1,
                       "Starting COMM_INIT comm_id %d proc_idx %d num_procs %d",
                       fca_module->fca_comm_desc.comm_id, fca_module->local_proc_idx, fca_module->num_local_procs);

    comm_size = fca_module->comm->proc_count;
    ret = mca_scoll_fca_comm_init(mca_scoll_fca_component.fca_context,
                                  oshmem_proc_group_find_id(fca_module->comm,
                                                            fca_module->rank),
                                  comm_size,
                                  fca_module->local_proc_idx,
                                  fca_module->num_local_procs,
                                  &fca_module->fca_comm_desc,
                                  &fca_module->fca_comm);
    if (ret < 0) {
        FCA_ERROR("COMM_INIT failed: %s", fca_strerror(ret));
        return OSHMEM_ERROR;
    }

    /* get communicator capabilities */
    ret = fca_comm_get_caps(fca_module->fca_comm, &fca_module->fca_comm_caps);
    if (ret < 0) {
        FCA_ERROR("GET_COMM_CAPS failed: %s", fca_strerror(ret));
        return OSHMEM_ERROR;
    }

    /* by this point every rank in the communicator is set up */
    FCA_MODULE_VERBOSE(fca_module,
                       1,
                       "Initialized FCA communicator, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);

    return OSHMEM_SUCCESS;
}

static void _destroy_fca_comm(mca_scoll_fca_module_t *fca_module)
{
    int ret;
    struct oshmem_group_t *comm = fca_module->comm;
    const int root_pe = oshmem_proc_pe(comm->proc_array[root_id]);

    fca_comm_destroy(fca_module->fca_comm);
    if (comm->my_pe == root_pe && mca_scoll_fca_component.fca_context) {
        ret = fca_comm_end(mca_scoll_fca_component.fca_context,
                           fca_module->fca_comm_desc.comm_id);
        if (ret < 0) {
            FCA_ERROR("COMM_END failed: %s", fca_strerror(ret));
        }
    }

    FCA_MODULE_VERBOSE(fca_module,
                       1,
                       "Destroyed FCA communicator, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
}

#define FCA_SAVE_PREV_SCOLL_API(__api) do {\
    fca_module->previous_ ## __api            = comm->g_scoll.scoll_ ## __api;\
    fca_module->previous_ ## __api ## _module = comm->g_scoll.scoll_ ## __api ## _module;\
    if (!comm->g_scoll.scoll_ ## __api || !comm->g_scoll.scoll_ ## __api ## _module) {\
        FCA_VERBOSE(1, "no underlying " # __api"; disqualifying myself");\
        return OSHMEM_ERROR;\
    }\
    OBJ_RETAIN(fca_module->previous_ ## __api ## _module);\
} while(0)

static int _save_coll_handlers(mca_scoll_fca_module_t *fca_module)
{
    struct oshmem_group_t *comm = fca_module->comm;

    FCA_SAVE_PREV_SCOLL_API(barrier);
    FCA_SAVE_PREV_SCOLL_API(broadcast);
    FCA_SAVE_PREV_SCOLL_API(collect);
    FCA_SAVE_PREV_SCOLL_API(reduce);

    return OSHMEM_SUCCESS;
}

/*
 *  * Initialize module on the communicator
 *   */
static int mca_scoll_fca_module_enable(mca_scoll_base_module_t *module,
                                       struct oshmem_group_t *comm)
{

    mca_scoll_fca_module_t *fca_module = (mca_scoll_fca_module_t*) module;
    int rc;

    fca_module->comm = comm;
    fca_module->rank = comm->my_pe;

    rc = mca_scoll_fca_get_fca_lib(comm);
    if (rc != OSHMEM_SUCCESS)
        goto exit_fatal;

    rc = _save_coll_handlers(fca_module);
    if (rc != OSHMEM_SUCCESS)
        goto exit_fatal;

    rc = _get_local_ranks(fca_module);
    if (rc != OSHMEM_SUCCESS)
        goto exit_fatal;

    rc = _create_fca_comm(fca_module);
    if (rc != OSHMEM_SUCCESS)
        goto exit_fatal;

    FCA_MODULE_VERBOSE(fca_module, 1, "FCA Module initialized");
    return OMPI_SUCCESS;

    exit_fatal:
    /* it is possible that other pe(s) succesfully enabled fca.
     * So differnt frameworks will be used for collective ops 
     */
    FCA_ERROR("FCA module enable failed - aborting to prevent inconsistent application state");
    oshmem_shmem_abort(-1);
    return OMPI_ERROR;
}

static void mca_scoll_fca_module_clear(mca_scoll_fca_module_t *fca_module)
{
    fca_module->num_local_procs = 0;
    fca_module->local_ranks = NULL;
    fca_module->fca_comm = NULL;

    fca_module->previous_barrier = NULL;
    fca_module->previous_broadcast = NULL;
    fca_module->previous_collect = NULL;
    fca_module->previous_reduce = NULL;
}

static void mca_scoll_fca_module_construct(mca_scoll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    mca_scoll_fca_module_clear(fca_module);
}

static void mca_scoll_fca_module_destruct(mca_scoll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    OBJ_RELEASE(fca_module->previous_barrier_module);
    OBJ_RELEASE(fca_module->previous_broadcast_module);
    OBJ_RELEASE(fca_module->previous_collect_module);
    OBJ_RELEASE(fca_module->previous_reduce_module);
    if (fca_module->fca_comm)
        _destroy_fca_comm(fca_module);
    free(fca_module->local_ranks);
    mca_scoll_fca_module_clear(fca_module);
}

/*
 *  * Invoked when there's a new communicator that has been created.
 *   * Look at the communicator and decide which set of functions and
 *    * priority we want to return.
 *     */
mca_scoll_base_module_t *
mca_scoll_fca_comm_query(struct oshmem_group_t *comm, int *priority)
{
    mca_scoll_base_module_t *module;
    int size = comm->proc_count;
    int local_peers = 0;

    mca_scoll_fca_module_t *fca_module;

    *priority = 0;
    module = NULL;

    if (!mca_scoll_fca_component.fca_enable) {
        FCA_VERBOSE(20, "FCA is disable on user request => exiting");
        goto exit;
    }

    if (mca_memheap.memheap_component == NULL ) {
        FCA_VERBOSE(20, "No memheap => exiting");
        goto exit;
    }

    if (NULL == mca_scoll_fca_component.ret) {
        MCA_MEMHEAP_CALL(private_alloc(sizeof(int),(void **)&mca_scoll_fca_component.ret));
        MCA_MEMHEAP_CALL(private_alloc(oshmem_group_all->proc_count*sizeof(*mca_scoll_fca_component.rcounts), (void **)&mca_scoll_fca_component.rcounts ));
        MCA_MEMHEAP_CALL(private_alloc(/*info_size*/20,&mca_scoll_fca_component.my_info_exchangeable));
        MCA_MEMHEAP_CALL(private_alloc(sizeof(fca_comm_desc_t), &mca_scoll_fca_component.fca_comm_desc_exchangeable));
    }
    if (size < mca_scoll_fca_component.fca_np) {
        FCA_VERBOSE(20,
                    "size(%d) < fca_np(%d)",
                    size, mca_scoll_fca_component.fca_np);
        goto exit;
    }

    if (size < 2) {
        FCA_VERBOSE(20, "size(%d) < 2", size);
        goto exit;
    }

    if (!have_remote_peers(comm,
                           size,
                           &local_peers) /* || OMPI_COMM_IS_INTER(comm)*/) {
        FCA_VERBOSE(1,
                    "all peers in group are on the same node, fca disabled\n");
        goto exit;
    }

    fca_module = OBJ_NEW(mca_scoll_fca_module_t);
    if (!fca_module) {
        goto exit_fatal;
    }
    fca_module->super.scoll_module_enable = mca_scoll_fca_module_enable;
    fca_module->super.scoll_collect =
            mca_scoll_fca_component.fca_enable_allgather ?
                    mca_scoll_fca_collect : NULL;
    fca_module->super.scoll_reduce =
            mca_scoll_fca_component.fca_enable_allreduce ?
                    mca_scoll_fca_reduce : NULL;
    fca_module->super.scoll_barrier =
            mca_scoll_fca_component.fca_enable_barrier ? mca_scoll_fca_barrier :
                                                         NULL;
    fca_module->super.scoll_broadcast =
            mca_scoll_fca_component.fca_enable_bcast ? mca_scoll_fca_broadcast :
                                                       NULL;

    *priority = mca_scoll_fca_component.fca_priority;
    module = &fca_module->super;

    exit:
    FCA_VERBOSE(4,
                "Query FCA module for comm %p size %d rank %d local_peers=%d: priority=%d %s",
                (void *)comm, size, comm->my_pe, local_peers, *priority, module ? "enabled" : "disabled");
    return module;

    exit_fatal:
    /* it is possible that other pe(s) succesfully initialized fca.
     * So differnt frameworks will be used for collective ops 
     */
    FCA_ERROR("FCA module query failed - aborting");
    oshmem_shmem_abort(-1);
    return NULL ;
}

OBJ_CLASS_INSTANCE(mca_scoll_fca_module_t,
                   mca_scoll_base_module_t,
                   mca_scoll_fca_module_construct,
                   mca_scoll_fca_module_destruct);

