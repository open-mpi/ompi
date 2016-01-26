/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2015 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "osc_rdma.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_request.h"
#include "osc_rdma_active_target.h"
#include "osc_rdma_passive_target.h"
#include "osc_rdma_comm.h"
#include "osc_rdma_dynamic.h"
#include "osc_rdma_accumulate.h"

#include "opal/threads/mutex.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/align.h"
#if OPAL_CUDA_SUPPORT
#include "opal/datatype/opal_datatype_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */

#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "ompi/mca/bml/base/base.h"

static int ompi_osc_rdma_component_register (void);
static int ompi_osc_rdma_component_init (bool enable_progress_threads, bool enable_mpi_threads);
static int ompi_osc_rdma_component_finalize (void);
static int ompi_osc_rdma_component_query (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                          struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                          int flavor);
static int ompi_osc_rdma_component_select (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                           struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                           int flavor, int *model);

static int ompi_osc_rdma_set_info (struct ompi_win_t *win, struct ompi_info_t *info);
static int ompi_osc_rdma_get_info (struct ompi_win_t *win, struct ompi_info_t **info_used);

static int ompi_osc_rdma_query_btls (ompi_communicator_t *comm, struct mca_btl_base_module_t **btl);

static char *ompi_osc_rdma_btl_names;

ompi_osc_rdma_component_t mca_osc_rdma_component = {
    .super = {
        .osc_version = {
            OMPI_OSC_BASE_VERSION_3_0_0,
            .mca_component_name = "rdma",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_register_component_params = ompi_osc_rdma_component_register
        },
        .osc_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        .osc_init = ompi_osc_rdma_component_init,
        .osc_query = ompi_osc_rdma_component_query,
        .osc_select = ompi_osc_rdma_component_select,
        .osc_finalize = ompi_osc_rdma_component_finalize
    }
};

ompi_osc_base_module_t ompi_osc_rdma_module_rdma_template = {
    .osc_win_attach = ompi_osc_rdma_attach,
    .osc_win_detach  = ompi_osc_rdma_detach,
    .osc_free = ompi_osc_rdma_free,

    .osc_put = ompi_osc_rdma_put,
    .osc_get = ompi_osc_rdma_get,
    .osc_accumulate = ompi_osc_rdma_accumulate,
    .osc_compare_and_swap = ompi_osc_rdma_compare_and_swap,
    .osc_fetch_and_op = ompi_osc_rdma_fetch_and_op,
    .osc_get_accumulate = ompi_osc_rdma_get_accumulate,

    .osc_rput = ompi_osc_rdma_rput,
    .osc_rget = ompi_osc_rdma_rget,
    .osc_raccumulate = ompi_osc_rdma_raccumulate,
    .osc_rget_accumulate = ompi_osc_rdma_rget_accumulate,

    .osc_fence = ompi_osc_rdma_fence_atomic,

    .osc_start = ompi_osc_rdma_start_atomic,
    .osc_complete = ompi_osc_rdma_complete_atomic,
    .osc_post = ompi_osc_rdma_post_atomic,
    .osc_wait = ompi_osc_rdma_wait_atomic,
    .osc_test = ompi_osc_rdma_test_atomic,

    .osc_lock = ompi_osc_rdma_lock_atomic,
    .osc_unlock = ompi_osc_rdma_unlock_atomic,
    .osc_lock_all = ompi_osc_rdma_lock_all_atomic,
    .osc_unlock_all = ompi_osc_rdma_unlock_all_atomic,

    .osc_sync = ompi_osc_rdma_sync,
    .osc_flush = ompi_osc_rdma_flush,
    .osc_flush_all = ompi_osc_rdma_flush_all,
    .osc_flush_local = ompi_osc_rdma_flush_local,
    .osc_flush_local_all = ompi_osc_rdma_flush_local_all,

    .osc_set_info = ompi_osc_rdma_set_info,
    .osc_get_info = ompi_osc_rdma_get_info
};

/* look up parameters for configuring this window.  The code first
   looks in the info structure passed by the user, then it checks
   for a matching MCA variable. */
static bool check_config_value_bool (char *key, ompi_info_t *info)
{
    int ret, flag, param;
    bool result = false;
    const bool *flag_value = &result;

    ret = ompi_info_get_bool (info, key, &result, &flag);
    if (OMPI_SUCCESS == ret && flag) {
        return result;
    }

    param = mca_base_var_find("ompi", "osc", "rdma", key);
    if (0 <= param) {
        (void) mca_base_var_get_value(param, &flag_value, NULL, NULL);
    }

    return flag_value[0];
}

static int ompi_osc_rdma_pvar_read (const struct mca_base_pvar_t *pvar, void *value, void *obj)
{
    ompi_win_t *win = (ompi_win_t *) obj;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int offset = (int) (intptr_t) pvar->ctx;

    memcpy (value, (char *) module + offset, sizeof (unsigned long));

    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_component_register (void)
{
    mca_osc_rdma_component.no_locks = false;
    (void) mca_base_component_var_register(&mca_osc_rdma_component.super.osc_version,
                                           "no_locks", "Enable optimizations available only if MPI_LOCK is "
                                           "not used. Info key of same name overrides this value (default: false)",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.no_locks);

    mca_osc_rdma_component.buffer_size = 32768;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "buffer_size",
                                            "Size of temporary buffers (default: 32k)", MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
                                            &mca_osc_rdma_component.buffer_size);

    mca_osc_rdma_component.max_attach = 32;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "max_attach",
                                            "Maximum number of buffers that can be attached to a dynamic window. "
                                            "Keep in mind that each attached buffer will use a potentially limited "
                                            "resource (default: 32)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.max_attach);

    mca_osc_rdma_component.aggregation_limit = 1024;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "aggregation_limit",
                                            "Maximum size of an aggregated put/get. Messages are aggregated for consecutive"
                                            "put and get operations. In some cases this may lead to higher latency but "
                                            "should also lead to higher bandwidth utilization. Set to 0 to disable (default:"
                                            " 1k)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.aggregation_limit);

    mca_osc_rdma_component.priority = 90;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "priority",
                                            "Priority of the osc/rdma component (default: 90)",
                                            MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.priority);

    ompi_osc_rdma_btl_names = "openib,ugni";
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "btls",
                                            "Comma-delimited list of BTL component names to allow without verifying "
                                            "connectivity. Do not add a BTL to to this list unless it can reach all "
                                            "processes in any communicator used with an MPI window (default: openib,ugni)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &ompi_osc_rdma_btl_names);


    /* register performance variables */

    (void) mca_base_component_pvar_register (&mca_osc_rdma_component.super.osc_version, "put_retry_count",
                                             "Number of times put transaction were retried due to resource limitations",
                                             OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER, MCA_BASE_VAR_TYPE_UNSIGNED_LONG,
                                             NULL, MCA_BASE_VAR_BIND_MPI_WIN, MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                             ompi_osc_rdma_pvar_read, NULL, NULL,
                                             (void *) (intptr_t) offsetof (ompi_osc_rdma_module_t, put_retry_count));

    (void) mca_base_component_pvar_register (&mca_osc_rdma_component.super.osc_version, "get_retry_count",
                                             "Number of times get transaction were retried due to resource limitations",
                                             OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER, MCA_BASE_VAR_TYPE_UNSIGNED_LONG,
                                             NULL, MCA_BASE_VAR_BIND_MPI_WIN, MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                             ompi_osc_rdma_pvar_read, NULL, NULL,
                                             (void *) (intptr_t) offsetof (ompi_osc_rdma_module_t, get_retry_count));

    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_component_init (bool enable_progress_threads,
                                         bool enable_mpi_threads)
{
    int ret;

    OBJ_CONSTRUCT(&mca_osc_rdma_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_osc_rdma_component.request_gc, opal_list_t);
    OBJ_CONSTRUCT(&mca_osc_rdma_component.buffer_gc, opal_list_t);
    OBJ_CONSTRUCT(&mca_osc_rdma_component.modules, opal_hash_table_t);

    opal_hash_table_init(&mca_osc_rdma_component.modules, 2);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.frags, opal_free_list_t);
    ret = opal_free_list_init (&mca_osc_rdma_component.frags,
                               sizeof(ompi_osc_rdma_frag_t), 8,
                               OBJ_CLASS(ompi_osc_rdma_frag_t),
                               mca_osc_rdma_component.buffer_size, 8,
                               4, -1, 4, NULL, 0, NULL, NULL, NULL);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: opal_free_list_init_new failed: %d",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    OBJ_CONSTRUCT(&mca_osc_rdma_component.requests, opal_free_list_t);
    ret = opal_free_list_init (&mca_osc_rdma_component.requests,
                               sizeof(ompi_osc_rdma_request_t), 8,
                               OBJ_CLASS(ompi_osc_rdma_request_t), 0, 0,
                               0, -1, 32, NULL, 0, NULL, NULL, NULL);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                            "%s:%d: opal_free_list_init failed: %d\n",
                            __FILE__, __LINE__, ret);
    }

    OBJ_CONSTRUCT(&mca_osc_rdma_component.aggregate, opal_free_list_t);

    if (!enable_mpi_threads && mca_osc_rdma_component.aggregation_limit) {
        ret = opal_free_list_init (&mca_osc_rdma_component.aggregate,
                                   sizeof(ompi_osc_rdma_aggregation_t), 8,
                                   OBJ_CLASS(ompi_osc_rdma_aggregation_t), 0, 0,
                                   32, 128, 32, NULL, 0, NULL, NULL, NULL);

        if (OPAL_SUCCESS != ret) {
            opal_output_verbose(1, ompi_osc_base_framework.framework_output,
                                "%s:%d: opal_free_list_init failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    } else {
        /* only enable put aggregation when not using threads */
        mca_osc_rdma_component.aggregation_limit = 0;
    }

    return ret;
}


int ompi_osc_rdma_component_finalize (void)
{
    size_t num_modules;

    if (0 != (num_modules = opal_hash_table_get_size(&mca_osc_rdma_component.modules))) {
        opal_output(ompi_osc_base_framework.framework_output, "WARNING: There were %d Windows created but "
                    "not freed.", (int) num_modules);
    }

    OBJ_DESTRUCT(&mca_osc_rdma_component.frags);
    OBJ_DESTRUCT(&mca_osc_rdma_component.modules);
    OBJ_DESTRUCT(&mca_osc_rdma_component.lock);
    OBJ_DESTRUCT(&mca_osc_rdma_component.requests);
    OBJ_DESTRUCT(&mca_osc_rdma_component.request_gc);
    OBJ_DESTRUCT(&mca_osc_rdma_component.buffer_gc);
    OBJ_DESTRUCT(&mca_osc_rdma_component.aggregate);

    return OMPI_SUCCESS;
}


static int ompi_osc_rdma_component_query (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                          struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                          int flavor)
{

    if (MPI_WIN_FLAVOR_SHARED == flavor) {
        return -1;
    }

#if OPAL_CUDA_SUPPORT
    /* GPU buffers are not supported by the rdma component */
    if (MPI_WIN_FLAVOR_CREATE == flavor) {
        if (opal_cuda_check_bufs(*base, NULL)) {
            return -1;
        }
    }
#endif /* OPAL_CUDA_SUPPORT */

    if (OMPI_SUCCESS != ompi_osc_rdma_query_btls (comm, NULL)) {
        return -1;
    }


    return mca_osc_rdma_component.priority;
}

#define RANK_ARRAY_COUNT(module) ((ompi_comm_size ((module)->comm) + (module)->node_count - 1) / (module)->node_count)

static int ompi_osc_rdma_initialize_region (ompi_osc_rdma_module_t *module, void **base, size_t size) {
    ompi_osc_rdma_region_t *region = (ompi_osc_rdma_region_t *) module->state->regions;
    int ret;

    /* store displacement unit */
    module->state->disp_unit = module->disp_unit;

    /* store region info */
    module->state->region_count = 1;
    region->base = (osc_rdma_base_t) (intptr_t) *base;
    region->len = size;

    if (module->selected_btl->btl_register_mem && size) {
        if (MPI_WIN_FLAVOR_ALLOCATE != module->flavor || NULL == module->state_handle) {
            ret = ompi_osc_rdma_register (module, MCA_BTL_ENDPOINT_ANY, *base, size, MCA_BTL_REG_FLAG_ACCESS_ANY,
                                          &module->base_handle);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            memcpy (region->btl_handle_data, module->base_handle, module->selected_btl->btl_registration_handle_size);
        } else {
            memcpy (region->btl_handle_data, module->state_handle, module->selected_btl->btl_registration_handle_size);
        }
    }

    return OMPI_SUCCESS;
}

static int allocate_state_single (ompi_osc_rdma_module_t *module, void **base, size_t size)
{
    size_t total_size, local_rank_array_size, leader_peer_data_size;
    ompi_osc_rdma_peer_t *my_peer;
    int ret, my_rank;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "allocating private internal state");

    my_rank = ompi_comm_rank (module->comm);

    local_rank_array_size = sizeof (ompi_osc_rdma_rank_data_t) * RANK_ARRAY_COUNT(module);
    leader_peer_data_size = module->region_size * module->node_count;

    /* allocate anything that will be accessed remotely in the same region. this cuts down on the number of
     * registration handles needed to access this data. */
    total_size = module->state_size + local_rank_array_size + leader_peer_data_size;

    if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
        total_size += size;
    }

    /* the local data is ordered as follows: rank array (leader, offset mapping), state, leader peer data, and base
     * (if using MPI_Win_allocate). In this case the leader peer data array does not need to be stored in the same
     * segment but placing it there simplifies the peer data fetch and cleanup code. */

    module->rank_array = calloc (total_size, 1);
    if (OPAL_UNLIKELY(NULL == module->rank_array)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    module->state_offset = local_rank_array_size;

    module->state = (ompi_osc_rdma_state_t *) ((intptr_t) module->rank_array + module->state_offset);
    module->node_comm_info = (unsigned char *) ((intptr_t) module->state + module->state_size);

    if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
        *base = (void *) ((intptr_t) module->node_comm_info + leader_peer_data_size);
    }

    /* just go ahead and register the whole segment */
    ret = ompi_osc_rdma_register (module, MCA_BTL_ENDPOINT_ANY, module->rank_array, total_size,
                                  MCA_BTL_REG_FLAG_ACCESS_ANY, &module->state_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
        ret = ompi_osc_rdma_initialize_region (module, base, size);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    ret = ompi_osc_rdma_new_peer (module, my_rank, &my_peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_osc_module_add_peer (module, my_peer);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        OBJ_RELEASE(my_peer);
        return ret;
    }

    module->free_after = module->rank_array;
    my_peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;
    my_peer->state = (uint64_t) (uintptr_t) module->state;

    if (module->selected_btl->btl_flags & MCA_BTL_ATOMIC_SUPPORTS_GLOB) {
        /* all peers are local or it is safe to mix cpu and nic atomics */
        my_peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_STATE;
    } else {
        /* use my endpoint handle to modify the peer's state */
        my_peer->state_handle = module->state_handle;
        my_peer->state_endpoint = ompi_osc_rdma_peer_btl_endpoint (module, my_rank);
    }

    if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
        ompi_osc_rdma_peer_extended_t *ex_peer = (ompi_osc_rdma_peer_extended_t *) my_peer;

        ex_peer->super.base = (intptr_t) *base;

        if (!module->same_size) {
            ex_peer->size = size;
        }

        if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            ex_peer->super.base_handle = module->state_handle;
        }
    }

    return OMPI_SUCCESS;
}

struct _local_data {
    int    rank;
    size_t size;
};

static int allocate_state_shared (ompi_osc_rdma_module_t *module, void **base, size_t size)
{
    ompi_communicator_t *shared_comm;
    unsigned long offset, total_size;
    unsigned long state_base, data_base;
    int local_rank, local_size, ret;
    size_t local_rank_array_size, leader_peer_data_size;
    int my_rank = ompi_comm_rank (module->comm);
    int global_size = ompi_comm_size (module->comm);
    ompi_osc_rdma_region_t *state_region;
    int my_base_offset = 0;
    struct _local_data *temp;
    char *data_file;

    shared_comm = module->shared_comm;

    local_rank = ompi_comm_rank (shared_comm);
    local_size = ompi_comm_size (shared_comm);

    if (1 == local_size) {
        /* no point using a shared segment if there are no other processes on this node */
        return allocate_state_single (module, base, size);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "allocating shared internal state");

    local_rank_array_size = sizeof (ompi_osc_rdma_rank_data_t) * RANK_ARRAY_COUNT (module);
    leader_peer_data_size = module->region_size * module->node_count;

    /* calculate base offsets */
    module->state_offset = state_base = local_rank_array_size + module->region_size;
    data_base = state_base + leader_peer_data_size + module->state_size * local_size;

    do {
        temp = calloc (local_size, sizeof (temp[0]));
        if (NULL == temp) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }

        temp[local_rank].rank = my_rank;
        temp[local_rank].size = size;

        /* gather the local sizes and ranks */
        ret = shared_comm->c_coll.coll_allgather (MPI_IN_PLACE, sizeof (*temp), MPI_BYTE, temp, sizeof (*temp),
                                                  MPI_BYTE, shared_comm, shared_comm->c_coll.coll_allgather_module);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        total_size = data_base;

        if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            for (int i = 0 ; i < local_size ; ++i) {
                total_size += temp[i].size;
                if (local_rank == i) {
                    my_base_offset = total_size;
                }
            }
        }

        /* allocate the shared memory segment */
        ret = asprintf (&data_file, "%s"OPAL_PATH_SEP"window_%d.%s",
                        ompi_process_info.job_session_dir, ompi_comm_get_cid (module->comm),
                        ompi_process_info.nodename);
        if (0 > ret) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }

        if (0 == local_rank) {
            /* allocate enough space for the state + data for all local ranks */
            ret = opal_shmem_segment_create (&module->seg_ds, data_file, total_size);
            free (data_file);
            if (OPAL_SUCCESS != ret) {
                OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to create shared memory segment");
                break;
            }
        }

        ret = module->comm->c_coll.coll_bcast (&module->seg_ds, sizeof (module->seg_ds), MPI_BYTE, 0,
                                               shared_comm, shared_comm->c_coll.coll_bcast_module);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        module->segment_base = opal_shmem_segment_attach (&module->seg_ds);
        if (NULL == module->segment_base) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to attach to the shared memory segment");
            ret = OPAL_ERROR;
            break;
        }

        if (size && MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            *base = (void *)((intptr_t) module->segment_base + my_base_offset);
        }

        module->rank_array = (ompi_osc_rdma_rank_data_t *) module->segment_base;
        /* put local state region data after the rank array */
        state_region = (ompi_osc_rdma_region_t *) ((uintptr_t) module->segment_base + local_rank_array_size);
        module->state = (ompi_osc_rdma_state_t *) ((uintptr_t) module->segment_base + state_base + module->state_size * local_rank);

        /* all local ranks share the array containing the peer data of leader ranks */
        module->node_comm_info = (unsigned char *) ((uintptr_t) module->segment_base + state_base + module->state_size * local_size);

        /* initialize my state */
        memset (module->state, 0, module->state_size);

        if (0 == local_rank) {
            /* just go ahead and register the whole segment */
            ret = ompi_osc_rdma_register (module, MCA_BTL_ENDPOINT_ANY, module->segment_base, total_size, MCA_BTL_REG_FLAG_ACCESS_ANY,
                                          &module->state_handle);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            state_region->base = (intptr_t) module->segment_base;
            if (module->state_handle) {
                memcpy (state_region->btl_handle_data, module->state_handle, module->selected_btl->btl_registration_handle_size);
            }
        }

        if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
            ret = ompi_osc_rdma_initialize_region (module, base, size);
            if (OMPI_SUCCESS != ret) {
                break;
            }
        }

        /* barrier to make sure all ranks have attached */
        shared_comm->c_coll.coll_barrier(shared_comm, shared_comm->c_coll.coll_barrier_module);

        /* unlink the shared memory backing file */
        if (0 == local_rank) {
            opal_shmem_unlink (&module->seg_ds);
        }

        offset = data_base;
        for (int i = 0 ; i < local_size ; ++i) {
            ompi_osc_rdma_peer_extended_t *ex_peer;
            ompi_osc_rdma_state_t *peer_state;
            ompi_osc_rdma_peer_t *peer;
            int peer_rank = temp[i].rank;

            ret = ompi_osc_rdma_new_peer (module, peer_rank, &peer);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            ex_peer = (ompi_osc_rdma_peer_extended_t *) peer;

            /* peer state local pointer */
            peer_state = (ompi_osc_rdma_state_t *) ((uintptr_t) module->segment_base + state_base + module->state_size * i);

            if (local_size == global_size || (module->selected_btl->btl_flags & MCA_BTL_ATOMIC_SUPPORTS_GLOB)) {
                /* all peers are local or it is safe to mix cpu and nic atomics */
                peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_STATE;
                peer->state = (osc_rdma_counter_t) peer_state;
            } else {
                /* use my endpoint handle to modify the peer's state */
                if (module->selected_btl->btl_register_mem) {
                    peer->state_handle = (mca_btl_base_registration_handle_t *) state_region->btl_handle_data;
                }
                peer->state = (osc_rdma_counter_t) ((uintptr_t) state_region->base + state_base + module->state_size * i);
                peer->state_endpoint = ompi_osc_rdma_peer_btl_endpoint (module, temp[0].rank);
            }

            /* finish setting up the local peer structure */
            if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
                if (!module->same_disp_unit) {
                    ex_peer->disp_unit = peer_state->disp_unit;
                }

                if (!module->same_size) {
                    ex_peer->size = temp[i].size;
                }

                if (my_rank == peer_rank) {
                    peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;
                }

                if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
                    if (temp[i].size) {
                        ex_peer->super.base = (uint64_t) (uintptr_t) module->segment_base + offset;
                    } else {
                        ex_peer->super.base = 0;
                    }

                    peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;

                    offset += temp[i].size;
                } else {
                    ompi_osc_rdma_region_t *peer_region = (ompi_osc_rdma_region_t *) peer_state->regions;

                    ex_peer->super.base = peer_region->base;
                    if (module->selected_btl->btl_register_mem) {
                        ex_peer->super.base_handle = (mca_btl_base_registration_handle_t *) peer_region->btl_handle_data;
                    }
                }
            }

            ompi_osc_module_add_peer (module, peer);
        }
    } while (0);

    free (temp);

    return ret;
}

static int ompi_osc_rdma_query_btls (ompi_communicator_t *comm, struct mca_btl_base_module_t **btl)
{
    struct mca_btl_base_module_t **possible_btls = NULL;
    int comm_size = ompi_comm_size (comm);
    int rc = OMPI_SUCCESS, max_btls = 0;
    unsigned int selected_latency = INT_MAX;
    struct mca_btl_base_module_t *selected_btl = NULL;
    mca_btl_base_selected_module_t *item;
    int *btl_counts = NULL;
    char **btls_to_use;
    void *tmp;

    btls_to_use = opal_argv_split (ompi_osc_rdma_btl_names, ',');
    if (btls_to_use) {
        /* rdma and atomics are only supported with BTLs at the moment */
        OPAL_LIST_FOREACH(item, &mca_btl_base_modules_initialized, mca_btl_base_selected_module_t) {
            for (int i = 0 ; btls_to_use[i] ; ++i) {
                if (0 != strcmp (btls_to_use[i], item->btl_module->btl_component->btl_version.mca_component_name)) {
                    continue;
                }

                if ((item->btl_module->btl_flags & (MCA_BTL_FLAGS_RDMA)) == MCA_BTL_FLAGS_RDMA &&
                    (item->btl_module->btl_flags & (MCA_BTL_FLAGS_ATOMIC_FOPS | MCA_BTL_FLAGS_ATOMIC_OPS))) {
                    if (!selected_btl || item->btl_module->btl_latency < selected_btl->btl_latency) {
                        selected_btl = item->btl_module;
                    }
                }
            }
        }

        opal_argv_free (btls_to_use);
    }

    if (btl) {
        *btl = selected_btl;
    }

    if (NULL != selected_btl) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "selected btl: %s",
                         selected_btl->btl_component->btl_version.mca_component_name);
        return OMPI_SUCCESS;
    }

    for (int i = 0 ; i < comm_size ; ++i) {
        ompi_proc_t *proc = ompi_comm_peer_lookup (comm, i);
        mca_bml_base_endpoint_t *endpoint;
        int num_btls, prev_max;

        endpoint = mca_bml_base_get_endpoint (proc);
        if (NULL == endpoint) {
            /* can't continue if some peer is unreachable */
            rc = OMPI_ERR_UNREACH;
            break;
        }

        num_btls = mca_bml_base_btl_array_get_size (&endpoint->btl_rdma);
        if (0 == num_btls) {
            rc = OMPI_ERR_NOT_AVAILABLE;
            /* at least one rank doesn't have an RDMA capable btl */
            break;
        }

        prev_max = max_btls;

        max_btls = (max_btls > num_btls) ? max_btls : num_btls;

        tmp = realloc (possible_btls, sizeof (void *) * max_btls);
        if (NULL == tmp) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }
        possible_btls = tmp;

        for (int j = prev_max ; j < max_btls ; ++j) {
            possible_btls[j] = NULL;
        }

        tmp = realloc (btl_counts, sizeof (int) * max_btls);
        if (NULL == tmp) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }
        btl_counts = tmp;

        for (int i_btl = 0 ; i_btl < num_btls ; ++i_btl) {
            /* for this implementation we need only compare-and-swap and fetch-and-add */
            if ((endpoint->btl_rdma.bml_btls[i_btl].btl->btl_flags & (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) ==
                (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS) && (endpoint->btl_rdma.bml_btls[i_btl].btl->btl_atomic_flags &
                                                                     MCA_BTL_ATOMIC_SUPPORTS_ADD)) {
                for (int j = 0 ; j < max_btls ; ++j) {
                    if (endpoint->btl_rdma.bml_btls[i_btl].btl == possible_btls[j]) {
                        ++btl_counts[j];
                        break;
                    } else if (NULL == possible_btls[j]) {
                        possible_btls[j] = endpoint->btl_rdma.bml_btls[i_btl].btl;
                        btl_counts[j] = 1;
                        break;
                    }
                }
            }
        }
    }

    if (OMPI_SUCCESS != rc) {
        free (possible_btls);
        free (btl_counts);

        /* no btl = no rdma/atomics */
        return OMPI_ERR_NOT_AVAILABLE;
    }

    for (int i = 0 ; i < max_btls ; ++i) {
        if (NULL == possible_btls[i]) {
            break;
        }

        if (btl_counts[i] == comm_size && possible_btls[i]->btl_latency < selected_latency) {
            selected_btl = possible_btls[i];
            selected_latency = possible_btls[i]->btl_latency;
        }
    }

    free (possible_btls);
    free (btl_counts);

    if (btl) {
        *btl = selected_btl;
    }

    if (NULL == selected_btl) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "no suitable btls found");
        /* no btl = no rdma/atomics */
        return OMPI_ERR_NOT_AVAILABLE;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "selected btl: %s",
                     selected_btl->btl_component->btl_version.mca_component_name);

    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_share_data (ompi_osc_rdma_module_t *module)
{
    ompi_osc_rdma_region_t *my_data;
    int ret, global_result;
    int my_rank = ompi_comm_rank (module->comm);
    int comm_size = ompi_comm_size (module->comm);
    ompi_osc_rdma_rank_data_t *temp;

    do {
        temp = malloc (sizeof (*temp) * comm_size);
        if (NULL == temp) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }

        /* fill in rank -> node translation */
        temp[my_rank].node_id = module->node_id;
        temp[my_rank].rank = ompi_comm_rank (module->shared_comm);

        ret = module->comm->c_coll.coll_allgather (MPI_IN_PLACE, 1, MPI_2INT, temp, 1, MPI_2INT,
                                                   module->comm, module->comm->c_coll.coll_allgather_module);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        if (0 == ompi_comm_rank (module->shared_comm)) {
            /* fill in my part of the node array */
            my_data = (ompi_osc_rdma_region_t *) ((intptr_t) module->node_comm_info + ompi_comm_rank (module->local_leaders) *
                                                  module->region_size);

            my_data->base = (uint64_t) (intptr_t) module->rank_array;

            if (module->selected_btl->btl_register_mem) {
                memcpy (my_data->btl_handle_data, module->state_handle, module->selected_btl->btl_registration_handle_size);
            }

            /* gather state data at each node leader */
            if (ompi_comm_size (module->local_leaders) > 1) {
                ret = module->local_leaders->c_coll.coll_allgather (MPI_IN_PLACE, module->region_size, MPI_BYTE, module->node_comm_info,
                                                                    module->region_size, MPI_BYTE, module->local_leaders,
                                                                    module->local_leaders->c_coll.coll_gather_module);
                if (OMPI_SUCCESS != ret) {
                    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "leader allgather failed with ompi error code %d", ret);
                    break;
                }
            }

            /* fill in the local part of the rank -> node map */
            for (int i = 0 ; i < RANK_ARRAY_COUNT(module) ; ++i) {
                int save_rank = my_rank + i;
                if (save_rank >= comm_size) {
                    break;
                }

                module->rank_array[i] = temp[save_rank];
            }
        }

        free (temp);
    } while (0);


    ret = module->comm->c_coll.coll_allreduce (&ret, &global_result, 1, MPI_INT, MPI_MIN, module->comm,
                                               module->comm->c_coll.coll_allreduce_module);

    if (OMPI_SUCCESS != ret) {
        global_result = ret;
    }

    /* none of these communicators are needed anymore so free them now*/
    if (MPI_COMM_NULL != module->local_leaders) {
        ompi_comm_free (&module->local_leaders);
    }

    if (MPI_COMM_NULL != module->shared_comm) {
        ompi_comm_free (&module->shared_comm);
    }

    return global_result;
}

static int ompi_osc_rdma_create_groups (ompi_osc_rdma_module_t *module)
{
    int comm_rank, ret, local_rank;
    int values[2] = {0, 0};

    /* create a shared communicator to handle communication about the local segment */
    ret = ompi_comm_split_type (module->comm, MPI_COMM_TYPE_SHARED, 0, NULL, &module->shared_comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to create a shared memory communicator. error code %d", ret);
        return ret;
    }

    local_rank = ompi_comm_rank (module->shared_comm);

    comm_rank = ompi_comm_rank (module->comm);

    ret = ompi_comm_split (module->comm, (0 == local_rank) ? 0 : MPI_UNDEFINED, comm_rank, &module->local_leaders,
                           false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to create local leaders communicator. error code %d", ret);
        return ret;
    }

    if (0 == local_rank) {
        values[0] = ompi_comm_size (module->local_leaders);
        values[1] = ompi_comm_rank (module->local_leaders);
    }

    if (ompi_comm_size (module->shared_comm) > 1) {
        ret = module->shared_comm->c_coll.coll_bcast (values, 2, MPI_INT, 0, module->shared_comm,
                                                      module->shared_comm->c_coll.coll_bcast_module);
        if (OMPI_SUCCESS != ret) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to broadcast local data. error code %d", ret);
            return ret;
        }
    }

    module->node_count = values[0];
    module->node_id = values[1];

    return OMPI_SUCCESS;
}

/**
 * @brief check the displacement unit and size against peers
 *
 * @param[in] module      osc rdma module
 * @param[in] disp_unit   the displacement unit for this process
 * @param[in] size        the window size for this process
 *
 * This function checks if all ranks have the same displacement unit or size and sets the appropriate
 * flags on the module.
 */
static int ompi_osc_rdma_check_parameters (ompi_osc_rdma_module_t *module, int disp_unit, size_t size)
{
    long values[4];
    int ret;

    if (MPI_WIN_FLAVOR_DYNAMIC == module->flavor || (module->same_size && module->same_disp_unit)) {
        /* done */
        return OMPI_SUCCESS;
    }

    /* check displacements and sizes */
    values[0] = disp_unit;
    values[1] = -disp_unit;
    values[2] = size;
    values[3] = -(ssize_t) size;

    ret = module->comm->c_coll.coll_allreduce (MPI_IN_PLACE, values, 4, MPI_LONG, MPI_MIN, module->comm,
                                               module->comm->c_coll.coll_allreduce_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if (values[0] == -values[1]) {
        /* same displacement */
        module->same_disp_unit = true;
    }

    if (values[2] == -values[3]) {
        /* same size */
        module->same_size = true;
    }

    return OMPI_SUCCESS;
}


static int ompi_osc_rdma_component_select (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                           struct ompi_communicator_t *comm, struct ompi_info_t *info,
                                           int flavor, int *model)
{
    ompi_osc_rdma_module_t *module = NULL;
    int world_size = ompi_comm_size (comm);
    int init_limit = 256;
    int ret;
    char *name;

    /* the osc/sm component is the exclusive provider for support for shared
     * memory windows */
    if (MPI_WIN_FLAVOR_SHARED == flavor) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    /* create module structure with all fields initialized to zero */
    module = (ompi_osc_rdma_module_t *) calloc (1, sizeof (ompi_osc_rdma_module_t));
    if (NULL == module) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize the objects, so that always free in cleanup */
    OBJ_CONSTRUCT(&module->lock, opal_recursive_mutex_t);
    OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);
    OBJ_CONSTRUCT(&module->pending_posts, opal_list_t);
    OBJ_CONSTRUCT(&module->peer_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->all_sync, ompi_osc_rdma_sync_t);

    module->same_disp_unit = check_config_value_bool ("same_disp_unit", info);
    module->same_size      = check_config_value_bool ("same_size", info);
    module->no_locks       = check_config_value_bool ("no_locks", info);

    module->all_sync.module = module;

    module->flavor = flavor;
    module->win = win;
    module->disp_unit = disp_unit;
    module->size = size;

    /* set the module so we properly cleanup */
    win->w_osc_module = (ompi_osc_base_module_t*) module;

    if (!module->no_locks) {
        if (world_size > init_limit) {
            ret = opal_hash_table_init (&module->outstanding_locks, init_limit);
            if (OPAL_SUCCESS != ret) {
                ompi_osc_rdma_free (win);
                return ret;
            }
        } else {
            module->outstanding_lock_array = calloc (world_size, sizeof (module->outstanding_lock_array[0]));
            if (NULL == module->outstanding_lock_array) {
                ompi_osc_rdma_free (win);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    /* options */
    /* FIX ME: should actually check this value... */
#if 1
    module->accumulate_ordering = 1;
#else
    ompi_osc_base_config_value_equal("accumulate_ordering", info, "none");
#endif

    ret = ompi_comm_dup(comm, &module->comm);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "creating osc/rdma window of flavor %d with id %d",
                     flavor, ompi_comm_get_cid(module->comm));

    /* peer data */
    if (world_size > init_limit) {
        OBJ_CONSTRUCT(&module->peer_hash, opal_hash_table_t);
        ret = opal_hash_table_init (&module->peer_hash, init_limit);
    } else {
        module->peer_array = calloc (world_size, sizeof (ompi_osc_rdma_peer_t *));
        if (NULL == module->peer_array) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    if (OPAL_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    /* find rdma capable endpoints */
    ret = ompi_osc_rdma_query_btls (module->comm, &module->selected_btl);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    /* calculate and store various structure sizes */

    module->region_size = module->selected_btl->btl_registration_handle_size + sizeof (ompi_osc_rdma_region_t);

    module->state_size = sizeof (ompi_osc_rdma_state_t);

    if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
        module->state_size += module->region_size;
    } else {
        module->state_size += mca_osc_rdma_component.max_attach * module->region_size;
    }

    /* fill in the function pointer part */
    memcpy(&module->super, &ompi_osc_rdma_module_rdma_template, sizeof(module->super));

    ret = ompi_osc_rdma_check_parameters (module, disp_unit, size);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    ret = ompi_osc_rdma_create_groups (module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    /* fill in our part */
    ret = allocate_state_shared (module, base, size);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to allocate internal state");
        ompi_osc_rdma_free (win);
        return ret;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC == flavor) {
        /* allocate space to store local btl handles for attached regions */
        module->dynamic_handles = (ompi_osc_rdma_handle_t *) calloc (mca_osc_rdma_component.max_attach,
                                                                     sizeof (module->dynamic_handles[0]));
        if (NULL == module->dynamic_handles) {
            ompi_osc_rdma_free (win);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    ret = ompi_osc_rdma_share_data (module);
    if (OMPI_SUCCESS != ret) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "failed to share window data with peers");
        ompi_osc_rdma_free (win);
        return ret;
    }


    /* for now the leader is always rank 0 in the communicator */
    module->leader = ompi_osc_rdma_module_peer (module, 0);

    /* lock data */
    if (module->no_locks) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    if (module->same_size) {
        win->w_flags |= OMPI_WIN_SAME_SIZE;
    }

    if (module->same_disp_unit) {
        win->w_flags |= OMPI_WIN_SAME_DISP;
    }

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.lock);
    ret = opal_hash_table_set_value_uint32(&mca_osc_rdma_component.modules,
                                           ompi_comm_get_cid(module->comm),
                                           module);
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.lock);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    /* fill in window information */
    *model = MPI_WIN_UNIFIED;
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    asprintf(&name, "rdma window %d", ompi_comm_get_cid(module->comm));
    ompi_win_set_name(win, name);
    free(name);

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    /* barrier to prevent arrival of lock requests before we're
       fully created */
    ret = module->comm->c_coll.coll_barrier(module->comm,
                                            module->comm->c_coll.coll_barrier_module);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }


    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "finished creating osc/rdma window with id %d",
                     ompi_comm_get_cid(module->comm));

    return OMPI_SUCCESS;
}


static int ompi_osc_rdma_set_info (struct ompi_win_t *win, struct ompi_info_t *info)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    bool temp;

    temp = check_config_value_bool ("no_locks", info);
    if (temp && !module->no_locks) {
        /* clean up the lock hash. it is up to the user to ensure no lock is
         * outstanding from this process when setting the info key */
        OBJ_DESTRUCT(&module->outstanding_locks);
        OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);

        module->no_locks = true;
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    } else if (!temp && module->no_locks) {
        int world_size = ompi_comm_size (module->comm);
        int init_limit = world_size > 256 ? 256 : world_size;
        int ret;

        ret = opal_hash_table_init (&module->outstanding_locks, init_limit);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }

        module->no_locks = false;
        win->w_flags &= ~OMPI_WIN_NO_LOCKS;
    }

    /* enforce collectiveness... */
    return module->comm->c_coll.coll_barrier(module->comm,
                                             module->comm->c_coll.coll_barrier_module);
}


static int ompi_osc_rdma_get_info (struct ompi_win_t *win, struct ompi_info_t **info_used)
{
    ompi_info_t *info = OBJ_NEW(ompi_info_t);

    if (NULL == info) {
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *info_used = info;

    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(ompi_osc_rdma_aggregation_t, opal_list_item_t, NULL, NULL);
