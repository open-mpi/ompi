/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2020 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2015 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2019-2021 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>

#include "osc_rdma.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_request.h"
#include "osc_rdma_active_target.h"
#include "osc_rdma_passive_target.h"
#include "osc_rdma_comm.h"
#include "osc_rdma_dynamic.h"
#include "osc_rdma_accumulate.h"

#include "opal/mca/threads/mutex.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"
#include "opal/util/sys_limits.h"
#include "opal/util/minmax.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/util/info_subscriber.h"
#include "opal/mca/mpool/base/base.h"

#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/btl/base/base.h"
/* for active-message RMA/atomics support */
#include "opal/mca/btl/base/btl_base_am_rdma.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/mtl/base/base.h"

static int ompi_osc_rdma_component_register (void);
static int ompi_osc_rdma_component_init (bool enable_progress_threads, bool enable_mpi_threads);
static int ompi_osc_rdma_component_finalize (void);
static int ompi_osc_rdma_component_query (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                          struct ompi_communicator_t *comm, struct opal_info_t *info,
                                          int flavor);
static int ompi_osc_rdma_component_select (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                           struct ompi_communicator_t *comm, struct opal_info_t *info,
                                           int flavor, int *model);
static int ompi_osc_rdma_query_accelerated_btls (ompi_communicator_t *comm, ompi_osc_rdma_module_t *module);
static int ompi_osc_rdma_query_alternate_btls (ompi_communicator_t *comm, ompi_osc_rdma_module_t *module);

static const char* ompi_osc_rdma_set_no_lock_info(opal_infosubscriber_t *obj, const char *key, const char *value);

static char *ompi_osc_rdma_full_connectivity_btls;

static const mca_base_var_enum_value_t ompi_osc_rdma_locking_modes[] = {
    {.value = OMPI_OSC_RDMA_LOCKING_TWO_LEVEL, .string = "two_level"},
    {.value = OMPI_OSC_RDMA_LOCKING_ON_DEMAND, .string = "on_demand"},
    {.string = NULL},
};

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
};

/* look up parameters for configuring this window.  The code first
   looks in the info structure passed by the user, then it checks
   for a matching MCA variable. */
static bool check_config_value_bool (char *key, opal_info_t *info)
{
    int ret, flag, param;
    bool result = false;
    const bool *flag_value = &result;

    ret = opal_info_get_bool (info, key, &result, &flag);
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
    char *description_str;
    mca_base_var_enum_t *new_enum;

    mca_osc_rdma_component.no_locks = false;
    opal_asprintf(&description_str, "Enable optimizations available only if MPI_LOCK is "
             "not used. Info key of same name overrides this value (default: %s)",
             mca_osc_rdma_component.no_locks  ? "true" : "false");
    (void) mca_base_component_var_register(&mca_osc_rdma_component.super.osc_version, "no_locks", description_str,
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.no_locks);
    free(description_str);

    mca_osc_rdma_component.acc_single_intrinsic = false;
    opal_asprintf(&description_str, "Enable optimizations for MPI_Fetch_and_op, MPI_Accumulate, etc for codes "
             "that will not use anything more than a single predefined datatype (default: %s)",
             mca_osc_rdma_component.acc_single_intrinsic  ? "true" : "false");
    (void) mca_base_component_var_register(&mca_osc_rdma_component.super.osc_version, "acc_single_intrinsic",
                                           description_str, MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.acc_single_intrinsic);
    free(description_str);

    mca_osc_rdma_component.acc_use_amo = true;
    opal_asprintf(&description_str, "Enable the use of network atomic memory operations when using single "
             "intrinsic optimizations. If not set network compare-and-swap will be "
             "used instead (default: %s)", mca_osc_rdma_component.acc_use_amo ? "true" : "false");
    (void) mca_base_component_var_register(&mca_osc_rdma_component.super.osc_version, "acc_use_amo", description_str,
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_GROUP,
                                           &mca_osc_rdma_component.acc_use_amo);
    free(description_str);

    mca_osc_rdma_component.buffer_size = 32768;
    opal_asprintf(&description_str, "Size of temporary buffers (default: %d)", mca_osc_rdma_component.buffer_size);
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "buffer_size", description_str,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_osc_rdma_component.buffer_size);
    free(description_str);

    mca_osc_rdma_component.max_attach = 64;
    opal_asprintf(&description_str, "Maximum number of buffers that can be attached to a dynamic window. "
             "Keep in mind that each attached buffer will use a potentially limited "
             "resource (default: %d)", mca_osc_rdma_component.max_attach);
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "max_attach", description_str,
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.max_attach);
    free(description_str);

    mca_osc_rdma_component.priority = 20;
    opal_asprintf(&description_str, "Priority of the osc/rdma component (default: %d)",
             mca_osc_rdma_component.priority);
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "priority", description_str,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.priority);
    free(description_str);

    (void) mca_base_var_enum_create ("osc_rdma_locking_mode", ompi_osc_rdma_locking_modes, &new_enum);

    mca_osc_rdma_component.locking_mode = OMPI_OSC_RDMA_LOCKING_TWO_LEVEL;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "locking_mode",
                                            "Locking mode to use for passive-target synchronization (default: two_level)",
                                            MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_rdma_component.locking_mode);
    OBJ_RELEASE(new_enum);

    ompi_osc_rdma_full_connectivity_btls = "ugni,uct,ofi";
    opal_asprintf(&description_str, "Comma-delimited list of BTL component names to allow without verifying "
             "connectivity. Do not add a BTL to to this list unless it can reach all "
             "processes in any communicator used with an MPI window (default: %s)",
             ompi_osc_rdma_full_connectivity_btls);
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "btls", description_str,
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_GROUP, &ompi_osc_rdma_full_connectivity_btls);
    free(description_str);

    if (0 == access ("/dev/shm", W_OK)) {
        mca_osc_rdma_component.backing_directory = "/dev/shm";
    } else {
        mca_osc_rdma_component.backing_directory = ompi_process_info.proc_session_dir;
    }

    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "backing_directory",
                                            "Directory to place backing files for memory windows. "
                                            "This directory should be on a local filesystem such as /tmp or "
                                            "/dev/shm (default: (linux) /dev/shm, (others) session directory)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_osc_rdma_component.backing_directory);

    mca_osc_rdma_component.network_amo_max_count = 32;
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "network_max_amo",
                                            "Maximum predefined datatype count for which network atomic operations "
                                            "will be used. Accumulate operations larger than this count will use "
                                            "a get/op/put protocol. The optimal value is dictated by the network "
                                            "injection rate for the interconnect. Generally a smaller number will "
                                            "yield better larger accumulate performance. (default: 32)",
                                            MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_osc_rdma_component.network_amo_max_count);

    mca_osc_rdma_component.memory_alignment = opal_getpagesize();
    opal_asprintf(&description_str, "The minimum memory alignment used to allocate local window memory (default: %zu). "
                  "This is a best effort approach. Alignments larger than the page size may not be supported.",
                  mca_osc_rdma_component.memory_alignment);
    (void) mca_base_component_var_register (&mca_osc_rdma_component.super.osc_version, "minimum_memory_alignment",
                                            description_str,
                                            MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0, OPAL_INFO_LVL_3,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_osc_rdma_component.memory_alignment);
    free(description_str);

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

    return OMPI_SUCCESS;
}


static int ompi_osc_rdma_component_query (struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                                          struct ompi_communicator_t *comm, struct opal_info_t *info,
                                          int flavor)
{

    if (MPI_WIN_FLAVOR_SHARED == flavor) {
        return -1;
    }

    if (MPI_WIN_FLAVOR_CREATE == flavor) {
        uint64_t flags;
        int dev_id;
        if (opal_accelerator.check_addr(*base, &dev_id, &flags)) {
            if (!osc_rdma_btl_accel_support(&mca_btl_base_modules_initialized)) {
                return -1;
            }
        }
    }

    /* verify if we have any btls available.  Since we do not verify
     * connectivity across all btls in the alternate case, this is as
     * good a test as we are going to have for success. */
    if (opal_list_is_empty(&mca_btl_base_modules_initialized)) {
        return -1;
    }

    return mca_osc_rdma_component.priority;
}

static int ompi_osc_rdma_initialize_region (ompi_osc_rdma_module_t *module, void **base, size_t size) {
    ompi_osc_rdma_region_t *region = (ompi_osc_rdma_region_t *) module->state->regions;
    int ret;

    /* store displacement unit */
    module->state->disp_unit = module->disp_unit;

    /* store region info */
    module->state->region_count = 1;
    region->base = (osc_rdma_base_t) (intptr_t) *base;
    region->len = size;

    if (module->use_memory_registration && size) {
        assert(module->use_accelerated_btl);
        if (MPI_WIN_FLAVOR_ALLOCATE != module->flavor || NULL == module->state_handle) {
            ret = ompi_osc_rdma_register (module, MCA_BTL_ENDPOINT_ANY, *base, size, MCA_BTL_REG_FLAG_ACCESS_ANY,
                                          &module->base_handle);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            memcpy (region->btl_handle_data, module->base_handle, module->accelerated_btl->btl_registration_handle_size);
        } else {
            memcpy (region->btl_handle_data, module->state_handle, module->accelerated_btl->btl_registration_handle_size);
        }
    }

    return OMPI_SUCCESS;
}

static int allocate_state_single (ompi_osc_rdma_module_t *module, void **base, size_t size, bool use_cpu_atomics)
{
    size_t total_size, local_rank_array_size, leader_peer_data_size, base_data_size;
    ompi_osc_rdma_peer_t *my_peer;
    int ret, my_rank;
    size_t memory_alignment = module->memory_alignment;

    opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                        "allocating private internal state");

    my_rank = ompi_comm_rank (module->comm);

    local_rank_array_size = sizeof (ompi_osc_rdma_rank_data_t) * RANK_ARRAY_COUNT(module);
    leader_peer_data_size = module->region_size * module->node_count;

    /* allocate anything that will be accessed remotely in the same region. this cuts down on the number of
     * registration handles needed to access this data. */
    total_size = local_rank_array_size + module->region_size +
        module->state_size + leader_peer_data_size;
    base_data_size = total_size;

    if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
        base_data_size += OPAL_ALIGN_PAD_AMOUNT(base_data_size, memory_alignment);
        total_size = base_data_size + size;
    }

    /* the local data is ordered as follows: rank array (leader, offset mapping), state, leader peer data, and base
     * (if using MPI_Win_allocate). In this case the leader peer data array does not need to be stored in the same
     * segment but placing it there simplifies the peer data fetch and cleanup code. */

    module->rank_array = mca_mpool_base_default_module->mpool_alloc(mca_mpool_base_default_module, total_size,
                                                                    memory_alignment, 0);
    if (OPAL_UNLIKELY(NULL == module->rank_array)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset(module->rank_array, 0, total_size);

    /* Note, the extra module->region_size space added after local_rank_array_size
     * is unused but is there to match what happens in allocte_state_shared()
     * This allows module->state_offset to be uniform across the ranks which
     * is part of how they pull peer info from each other. */
    module->state_offset = local_rank_array_size + module->region_size;

    module->state = (ompi_osc_rdma_state_t *) ((intptr_t) module->rank_array + module->state_offset);
    module->node_comm_info = (unsigned char *) ((intptr_t) module->state + module->state_size);

    if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
        *base = (void *) ((intptr_t) module->rank_array + base_data_size);
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

    module->my_peer = my_peer;
    module->free_after = module->rank_array;
    my_peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;
    my_peer->state = (uint64_t) (uintptr_t) module->state;

    if (use_cpu_atomics) {
        /* all peers are local or it is safe to mix cpu and nic atomics */
        my_peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_STATE;
    } else {
        /* use my endpoint handle to modify the peer's state */
        my_peer->state_handle = module->state_handle;
        my_peer->state_btl_index = my_peer->data_btl_index;
        my_peer->state_endpoint = my_peer->data_endpoint;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
        ompi_osc_rdma_peer_extended_t *ex_peer = (ompi_osc_rdma_peer_extended_t *) my_peer;

        ex_peer->super.base = (intptr_t) *base;

        if (!module->same_size) {
            ex_peer->size = size;
        }

        if (!use_cpu_atomics) {
            if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
                /* base is local and cpu atomics are available */
                ex_peer->super.base_handle = module->state_handle;
            } else {
                ex_peer->super.base_handle = module->base_handle;
            }
        }
    }

    return OMPI_SUCCESS;
}

struct _local_data {
    int    rank;
    size_t size;
};

static int synchronize_errorcode(int errorcode, ompi_communicator_t *comm)
{
    int ret;
    int err = errorcode;
    /* This assumes that error codes are negative integers */
    ret = comm->c_coll->coll_allreduce (MPI_IN_PLACE, &err, 1, MPI_INT, MPI_MIN,
                                        comm, comm->c_coll->coll_allreduce_module);
    if (OPAL_UNLIKELY (OMPI_SUCCESS != ret)) {
        err = ret;
    }
    return err;
}

static int allocate_state_shared (ompi_osc_rdma_module_t *module, void **base, size_t size)
{
    ompi_communicator_t *shared_comm;
    unsigned long offset, total_size;
    unsigned long state_base, data_base;
    int local_rank, local_size, ret;
    size_t local_rank_array_size, leader_peer_data_size, my_base_offset = 0;
    int my_rank = ompi_comm_rank (module->comm);
    int global_size = ompi_comm_size (module->comm);
    ompi_osc_rdma_region_t *state_region;
    struct _local_data *temp;
    char *data_file;
    size_t memory_alignment = module->memory_alignment;
    bool use_cpu_atomics;

    shared_comm = module->shared_comm;

    local_rank = ompi_comm_rank (shared_comm);
    local_size = ompi_comm_size (shared_comm);

    /* CPU atomics can be used if every process is on the same node or the NIC allows mixing CPU and NIC atomics */
    module->single_node     = local_size == global_size;

    if (module->single_node) {
        use_cpu_atomics = true;
    } else if (module->use_accelerated_btl) {
        use_cpu_atomics = !!(module->accelerated_btl->btl_flags & MCA_BTL_ATOMIC_SUPPORTS_GLOB);
    } else {
        /* using the shared state optimization that is enabled by
         * being able to use cpu atomics was never enabled for
         * alternate btls, due to a previous bug in the enablement
         * logic when alternate btls were first supported.  It is
         * likely that this optimization could work with sufficient
         * testing, but for now, always disable to not introduce new
         * correctness risks.
         */
        use_cpu_atomics = false;
    }

    if (1 == local_size) {
        /* no point using a shared segment if there are no other processes on this node */
        return allocate_state_single (module, base, size, use_cpu_atomics);
    }

    opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                        "allocating shared internal state");

    local_rank_array_size = sizeof (ompi_osc_rdma_rank_data_t) * RANK_ARRAY_COUNT (module);
    leader_peer_data_size = module->region_size * module->node_count;

    /* calculate base offsets */
    module->state_offset = state_base = local_rank_array_size + module->region_size;
    data_base = state_base + leader_peer_data_size + module->state_size * local_size;

    /* ensure proper alignment */
    if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
        data_base += OPAL_ALIGN_PAD_AMOUNT(data_base, memory_alignment);
        size += OPAL_ALIGN_PAD_AMOUNT(size, memory_alignment);
    }

    do {
        temp = calloc (local_size, sizeof (temp[0]));
        if (NULL == temp) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }

        temp[local_rank].rank = my_rank;
        temp[local_rank].size = size;

        /* gather the local sizes and ranks */
        ret = shared_comm->c_coll->coll_allgather (MPI_IN_PLACE, sizeof (*temp), MPI_BYTE, temp, sizeof (*temp),
                                                  MPI_BYTE, shared_comm, shared_comm->c_coll->coll_allgather_module);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        total_size = data_base;

        if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            for (int i = 0 ; i < local_size ; ++i) {
                if (local_rank == i) {
                    my_base_offset = total_size;
                }
                total_size += temp[i].size;
                total_size += OPAL_ALIGN_PAD_AMOUNT(total_size, memory_alignment);
            }
        }

        if (0 == local_rank) {
            /* allocate the shared memory segment */
            ret = opal_asprintf (&data_file, "%s" OPAL_PATH_SEP "osc_rdma.%s.%x.%s.%d",
                            mca_osc_rdma_component.backing_directory, ompi_process_info.nodename,
                            OMPI_PROC_MY_NAME->jobid, ompi_comm_print_cid(module->comm), getpid());
            if (0 > ret) {
                ret = OMPI_ERR_OUT_OF_RESOURCE;
            } else {
                /* allocate enough space for the state + data for all local ranks */
                ret = opal_shmem_segment_create (&module->seg_ds, data_file, total_size);
                free (data_file);
                if (OPAL_SUCCESS != ret) {
                    opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                                        "failed to create shared memory segment");
                }
            }
        }

        ret = synchronize_errorcode(ret, shared_comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        ret = shared_comm->c_coll->coll_bcast (&module->seg_ds, sizeof (module->seg_ds), MPI_BYTE, 0,
                                               shared_comm, shared_comm->c_coll->coll_bcast_module);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        module->segment_base = opal_shmem_segment_attach (&module->seg_ds);
        if (NULL == module->segment_base) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                                "failed to attach to the shared memory segment");
            ret = OPAL_ERROR;
        }

        ret = synchronize_errorcode(ret, shared_comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (size && MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            size_t page_size = opal_getpagesize();
            char *baseptr = (char *)((intptr_t) module->segment_base + my_base_offset);
            *base = (void *)baseptr;
            // touch each page to force allocation on local NUMA node
            for (size_t i = 0; i < size; i += page_size) {
                baseptr[i] = 0;
            }
        }

        module->rank_array = (ompi_osc_rdma_rank_data_t *) module->segment_base;
        /* put local state region data after the rank array */
        state_region = (ompi_osc_rdma_region_t *) ((uintptr_t) module->segment_base + local_rank_array_size);
        module->state = (ompi_osc_rdma_state_t *) ((uintptr_t) module->segment_base + state_base + module->state_size * local_rank);

        /* all local ranks share the array containing the peer data of leader ranks */
        module->node_comm_info = (unsigned char *) ((uintptr_t) module->segment_base + state_base + module->state_size * local_size);

        /* initialize my state */
        memset (module->state, 0, module->state_size);

        /* barrier to make sure all ranks have attached and initialized */
        shared_comm->c_coll->coll_barrier(shared_comm, shared_comm->c_coll->coll_barrier_module);

        if (0 == local_rank) {
            /* unlink the shared memory backing file */
            opal_shmem_unlink (&module->seg_ds);

            state_region->base = (intptr_t) module->segment_base;
            if (module->use_accelerated_btl) {
                /* just go ahead and register the whole segment */
                ret = ompi_osc_rdma_register(module, MCA_BTL_ENDPOINT_ANY, module->segment_base, total_size,
                                             MCA_BTL_REG_FLAG_ACCESS_ANY, &module->state_handle);
                if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
                    if (module->state_handle) {
                        memcpy(state_region->btl_handle_data, module->state_handle,
                               module->accelerated_btl->btl_registration_handle_size);
                    }
                }
            }
        }

        /* synchronization to make sure memory is registered */
        ret = synchronize_errorcode(ret, shared_comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (MPI_WIN_FLAVOR_CREATE == module->flavor) {
            ret = ompi_osc_rdma_initialize_region (module, base, size);
        } else if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            ompi_osc_rdma_region_t *region = (ompi_osc_rdma_region_t *) module->state->regions;
            module->state->disp_unit = module->disp_unit;
            module->state->region_count = 1;
            region->base = state_region->base + my_base_offset;
            region->len = size;
            if (module->use_memory_registration) {
                assert(module->use_accelerated_btl);
                memcpy(region->btl_handle_data, state_region->btl_handle_data,
                       module->accelerated_btl->btl_registration_handle_size);
            }
        }

        /* synchronization to make sure all ranks have set up their region data */
        ret = synchronize_errorcode(ret, shared_comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        offset = data_base;
        ompi_osc_rdma_peer_t *local_leader;
        for (int i = 0 ; i < local_size ; ++i) {
            /* local pointer to peer's state */
            ompi_osc_rdma_state_t *peer_state = (ompi_osc_rdma_state_t *) ((uintptr_t) module->segment_base + state_base + module->state_size * i);
            ompi_osc_rdma_region_t *peer_region = (ompi_osc_rdma_region_t *) peer_state->regions;
            ompi_osc_rdma_peer_extended_t *ex_peer;
            ompi_osc_rdma_peer_t *peer;
            int peer_rank = temp[i].rank;

            ret = ompi_osc_rdma_new_peer (module, peer_rank, &peer);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            if (0 == i) {
                local_leader = peer;
            }

            ex_peer = (ompi_osc_rdma_peer_extended_t *) peer;

            /* set up peer state */
            if (use_cpu_atomics) {
                /* all peers are local or it is safe to mix cpu and nic atomics */
                peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_STATE;
                peer->state = (osc_rdma_counter_t) peer_state;
                peer->state_endpoint = NULL;
            } else {
                /* use my endpoint handle to modify the peer's state */
                if (module->use_memory_registration) {
                    peer->state_handle = (mca_btl_base_registration_handle_t *) state_region->btl_handle_data;
                }
                peer->state = (osc_rdma_counter_t) ((uintptr_t) state_region->base + state_base + module->state_size * i);
                if (i==0) {
                    peer->state_endpoint = peer->data_endpoint;
                    peer->state_btl_index = peer->data_btl_index;
                } else {
                    peer->state_endpoint = local_leader->state_endpoint;
                    peer->state_btl_index = local_leader->state_btl_index;
                }
            }

            if (my_rank == peer_rank) {
                module->my_peer = peer;
            }

            if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor && MPI_WIN_FLAVOR_CREATE != module->flavor &&
                !use_cpu_atomics && temp[i].size && i > 0) {
                /* use the local leader's endpoint */
                peer->data_endpoint = local_leader->data_endpoint;
                peer->data_btl_index = local_leader->data_btl_index;
            }

            ompi_osc_module_add_peer (module, peer);

            if (MPI_WIN_FLAVOR_DYNAMIC == module->flavor) {
                if (use_cpu_atomics && peer_rank == my_rank) {
                    peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;
                }
                /* nothing more to do */
                continue;
            } else if (0 == temp[i].size) {
                /* nothing more to do */
                continue;
            }

            /* finish setting up the local peer structure for win allocate/create */
            if (!(module->same_disp_unit && module->same_size)) {
                ex_peer->disp_unit = peer_state->disp_unit;
                ex_peer->size = temp[i].size;
            }

            if (use_cpu_atomics && (MPI_WIN_FLAVOR_ALLOCATE == module->flavor || peer_rank == my_rank)) {
                /* base is local and cpu atomics are available */
                if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
                    ex_peer->super.base = (uintptr_t) module->segment_base + offset;
                } else {
                    ex_peer->super.base = (uintptr_t) *base;
                }

                peer->flags |= OMPI_OSC_RDMA_PEER_LOCAL_BASE;
                offset += temp[i].size;
            } else {
                ex_peer->super.base = peer_region->base;

                if (module->use_memory_registration) {
                    ex_peer->super.base_handle = (mca_btl_base_registration_handle_t *) peer_region->btl_handle_data;
                }
            }
        }
    } while (0);

    free (temp);

    return ret;
}

/**
 * @brief ensure that all local procs are added to the bml
 *
 * The sm btl requires that all local procs be added to work correctly. If pml/ob1
 * was not selected then we can't rely on this property. Since osc/rdma may use
 * btl/sm we need to ensure that btl/sm is set up correctly. This function will
 * only (potentially) call add_procs on local procs.
 */
static void ompi_osc_rdma_ensure_local_add_procs (void)
{
    size_t nprocs;
    ompi_proc_t** procs = ompi_proc_get_allocated (&nprocs);
    if (NULL == procs) {
        /* weird, this should have caused MPI_Init to fail */
        return;
    }

    for (size_t proc_index = 0 ; proc_index < nprocs ; ++proc_index) {
        ompi_proc_t *proc = procs[proc_index];
        if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
            /* this will cause add_proc to get called if it has not already been called */
            (void) mca_bml_base_get_endpoint (proc);
        }
    } 

    free(procs);
}


/*
 * qsort() sorting function for ompi_osc_rdma_query_alternate_btls(),
 * using latency as the sorting metric.
 */
static int btl_latency_sort_fn(const void *a, const void *b)
{
    const mca_btl_base_am_rdma_module_t * const *am_rdma_a_p = a;
    const mca_btl_base_am_rdma_module_t * const *am_rdma_b_p = b;
    const mca_btl_base_am_rdma_module_t *am_rdma_a = *am_rdma_a_p;
    const mca_btl_base_am_rdma_module_t *am_rdma_b = *am_rdma_b_p;

    if (am_rdma_a->btl->btl_latency < am_rdma_b->btl->btl_latency) {
        return -1;
    } else if (am_rdma_a->btl->btl_latency == am_rdma_b->btl->btl_latency) {
        return 0;
    } else {
        return 1;
    }
}


/**
 * @brief query for alternate BTLs
 *
 * @in  comm   Communicator to query
 * @inout module OSC module to store BTLs/count to (optional)
 *
 * @return OMPI_SUCCESS if BTLs can be found
 * @return OMPI_ERR_UNREACH if no BTLs can be found that match
 *
 * We directly use the active message rdma wrappers for alternate
 * BTLs, in all cases.  This greatly simplifies the alternate BTL
 * implementation, at the expense of some performance.  With the
 * AM wrappers, we can always enforce remote completion and the lack
 * of memory registration, at some performance cost.  But we can use
 * as many BTLs as we like.  The module's btl list is sorted by
 * latency, so that ompi_osc_rdma_peer_btl_endpoint() picks the lowest
 * available latency btl to communicate with the peer.  Unlike the OB1
 * PML, we only use one BTL per peer.
 *
 * Like the OB1 PML, there is no verification that there is at least
 * one BTL that can communicate with every other peer in the window.
 */
static int ompi_osc_rdma_query_alternate_btls (ompi_communicator_t *comm, ompi_osc_rdma_module_t *module)
{
    size_t btl_count;
    size_t index = 0;
    mca_btl_base_selected_module_t *item;
    int ret;

    assert(NULL != module);

    module->put_alignment = 1;
    module->get_alignment = 1;
    module->put_limit = SIZE_MAX;
    module->get_limit = SIZE_MAX;

    btl_count = opal_list_get_size(&mca_btl_base_modules_initialized);
    if (btl_count > UINT8_MAX) {
        return OMPI_ERROR;
    }

    module->alternate_btl_count = btl_count;
    module->alternate_am_rdmas = malloc(sizeof(struct mca_btl_base_am_rdma_module_t *) * module->alternate_btl_count);
    if (NULL == module->alternate_am_rdmas) {
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* add all alternate btls to the selected_btls list, not worrying
       about ordering yet.  We have to add all btls unless we want to
       iterate over all endpoints to build the minimum set of btls
       needed to communicate with all peers.  An MCA parameter just
       for osc rdma also wouldn't work, as the BML can decide not to
       add an endpoint for a btl given the priority of another btl.
       For example, it is not uncommon that the only endpoint created
       to a peer on the same host is the sm btl's endpoint.  If we
       had an osc rdma specific parameter list, and the user
       specified a combination not including sm, that would result in
       an eventual failure, as no btl would be found to talk to ranks
       on the same host.*/
    OPAL_LIST_FOREACH(item, &mca_btl_base_modules_initialized, mca_btl_base_selected_module_t) {
        opal_output_verbose(MCA_BASE_VERBOSE_INFO, ompi_osc_base_framework.framework_output,
                            "found alternate btl %s",
                            item->btl_module->btl_component->btl_version.mca_component_name);

        ret = opal_btl_base_am_rdma_create(item->btl_module,
                                           MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION,
                                           true /* no_memory_registration */,
                                           &(module->alternate_am_rdmas[index]));
        if (OMPI_SUCCESS != ret) {
            return ret;
        }

        module->put_alignment = opal_max(module->put_alignment,
                                         module->alternate_am_rdmas[index]->am_btl_put_alignment);
        module->get_alignment = opal_max(module->get_alignment,
                                         module->alternate_am_rdmas[index]->am_btl_get_alignment);
        module->put_limit = opal_min(module->put_limit,
                                     module->alternate_am_rdmas[index]->am_btl_put_limit);
        module->get_limit = opal_min(module->get_limit,
                                     module->alternate_am_rdmas[index]->am_btl_get_limit);

        index++;
    }
    assert(index == module->alternate_btl_count);

    /* sort based on latency, lowest first */
    qsort(module->alternate_am_rdmas, module->alternate_btl_count,
          sizeof(module->alternate_am_rdmas[0]), btl_latency_sort_fn);

    module->use_memory_registration = false;
    module->atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_ADD |
            MCA_BTL_ATOMIC_SUPPORTS_AND |
            MCA_BTL_ATOMIC_SUPPORTS_OR |
            MCA_BTL_ATOMIC_SUPPORTS_XOR |
            MCA_BTL_ATOMIC_SUPPORTS_SWAP |
            MCA_BTL_ATOMIC_SUPPORTS_MIN |
            MCA_BTL_ATOMIC_SUPPORTS_MAX |
            MCA_BTL_ATOMIC_SUPPORTS_32BIT |
            MCA_BTL_ATOMIC_SUPPORTS_CSWAP |
            MCA_BTL_ATOMIC_SUPPORTS_GLOB;

    return OMPI_SUCCESS;
}


/* Check for BTL requirements:
 *  1) RDMA (put/get) and ATOMIC operations.  We only require cswap
 *     and fetch and add and will emulate other opterations with those
 *     two as necessary.
 *  2) Remote Completion
 */
static bool ompi_osc_rdma_check_accelerated_btl(struct mca_btl_base_module_t *btl)
{
    return ((btl->btl_flags & MCA_BTL_FLAGS_RDMA) &&
            (btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_FOPS) &&
            (btl->btl_flags & MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION) &&
            (btl->btl_atomic_flags & MCA_BTL_ATOMIC_SUPPORTS_ADD));
}

/*
 * Attempt to find a BTL that can be used for native RDMA
 *
 * Attempt to find an "accelerated" BTL that can be used directly, as
 * opposed to emulated rdma semantics with the alternate BTLs.  To be
 * an accelerated BTL, four conditions must be true:
 *
 *  1) The BTL must be able to communicate with all peers in the
 *     Window
 *  2) The BTL must provide remote completion
 *  3) The BTL must be able to register the entire target window
 *  4) The BTL must natively support put/get/atomic operations
 *
 * Testing (1) is expensive, so as an optimization, the
 * ompi_osc_rdma_full_connectivity_btls list contains the list of BTL
 * components we know can achieve (1) in almost all usage scenarios.
 */
static int ompi_osc_rdma_query_accelerated_btls (ompi_communicator_t *comm, ompi_osc_rdma_module_t *module)
{
    int comm_size = ompi_comm_size (comm);
    struct mca_btl_base_module_t *selected_btl;
    mca_bml_base_endpoint_t *base_endpoint;
    char **btls_to_use;

    assert(NULL != module);

    module->use_accelerated_btl = false;
    module->use_memory_registration = false;

    /* Check for BTLs in the list of BTLs we know can reach all peers
       in general usage. */
    btls_to_use = opal_argv_split (ompi_osc_rdma_full_connectivity_btls, ',');
    if (btls_to_use) {
        mca_btl_base_selected_module_t *item;

        selected_btl = NULL;

        /* rdma and atomics are only supported with BTLs at the moment */
        OPAL_LIST_FOREACH(item, &mca_btl_base_modules_initialized, mca_btl_base_selected_module_t) {
            for (int i = 0 ; btls_to_use[i] ; ++i) {
                if (0 != strcmp (btls_to_use[i], item->btl_module->btl_component->btl_version.mca_component_name)) {
                    continue;
                }

                if (ompi_osc_rdma_check_accelerated_btl(item->btl_module)) {
                    if (NULL == selected_btl || item->btl_module->btl_latency < selected_btl->btl_latency) {
                        selected_btl = item->btl_module;
                    }
                }
            }
        }

        opal_argv_free (btls_to_use);

        if (NULL != selected_btl) {
            goto btl_selection_complete;
        }
    }

    /* if osc/rdma gets selected we need to ensure that all local procs have been added */
    ompi_osc_rdma_ensure_local_add_procs ();

    /*
     * A BTL in the list of known can reach all peers that met our
     * other requirements was not found.  Look for BTLs that may be
     * able to talk to all peers.  This is obviously more expensive
     * than the check above.
     *
     * This algorithm skips a potential use case: it requires
     * reachability to self, which is not strictly needed if BTL and
     * CPU atomics are atomic with each other.  However, the set of
     * BTLs which can not send to self, which have RDMA semantics, an
     * which have the required atomicity is currently the null set and
     * almost certain to remain the null set, so we keep it simple.
     *
     * We only want BTLs that can reach all peers, so use rank 0's BTL
     * list as the list of all available BTLs.  Any BTL that cannot
     * be used to communicate with rank 0 necessarily is not in the
     * list of all available BTLs for this algorithm.
     */
    base_endpoint = mca_bml_base_get_endpoint(ompi_comm_peer_lookup(comm, 0));
    if (NULL == base_endpoint) {
        return OMPI_ERR_UNREACH;
    }

    selected_btl = NULL;
    for (size_t i_btl = 0 ;
         i_btl < mca_bml_base_btl_array_get_size(&base_endpoint->btl_rdma);
         ++i_btl) {
        bool have_connectivity = true;
        struct mca_bml_base_btl_t *examine_bml_btl;
        struct mca_btl_base_module_t *examine_btl;

        examine_bml_btl = mca_bml_base_btl_array_get_index(&base_endpoint->btl_rdma, i_btl);
        if (NULL == examine_bml_btl) {
            return OMPI_ERR_NOT_FOUND;
        }
        examine_btl = examine_bml_btl->btl;

        /* skip any BTL which doesn't meet our requirements */
        if (!ompi_osc_rdma_check_accelerated_btl(examine_btl)) {
            continue;
        }

        /* check connectivity across all ranks */
        for (int rank = 0 ; rank < comm_size ; ++rank) {
            ompi_proc_t *proc = ompi_comm_peer_lookup(comm, rank);
            mca_bml_base_endpoint_t *endpoint;

            endpoint = mca_bml_base_get_endpoint(proc);
            if (NULL == endpoint) {
                have_connectivity = false;
                break;
            }

            if (NULL == mca_bml_base_btl_array_find(&endpoint->btl_rdma,
                                                    examine_btl)) {
                have_connectivity = false;
                break;
            }
        }

        /* if we have connectivity, displace currently selected btl if
         * this one has lower latency; we prioritize latency over all
         * other parameters
         */
        if (have_connectivity) {
            if (NULL == selected_btl || examine_btl->btl_latency < selected_btl->btl_latency) {
                selected_btl = examine_btl;
            }
        }
    }

    if (NULL == selected_btl) {
        opal_output_verbose(MCA_BASE_VERBOSE_INFO, ompi_osc_base_framework.framework_output,
                            "accelerated_query: no suitable btls found");
        return OMPI_ERR_NOT_AVAILABLE;
    }

btl_selection_complete:
    module->use_accelerated_btl = true;
    module->accelerated_btl = selected_btl;
    module->use_memory_registration = (selected_btl->btl_register_mem != NULL);
    module->put_alignment = selected_btl->btl_put_alignment;
    module->get_alignment = selected_btl->btl_get_alignment;
    module->put_limit = selected_btl->btl_put_limit;
    module->get_limit = selected_btl->btl_get_limit;
    module->atomic_flags = selected_btl->btl_atomic_flags;

    opal_output_verbose(MCA_BASE_VERBOSE_INFO, ompi_osc_base_framework.framework_output,
                        "accelerated_query: selected btl: %s",
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

        ret = module->comm->c_coll->coll_allgather (MPI_IN_PLACE, 1, MPI_2INT, temp, 1, MPI_2INT,
                                                   module->comm, module->comm->c_coll->coll_allgather_module);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        if (0 == ompi_comm_rank (module->shared_comm)) {
            /* fill in my part of the node array */
            my_data = (ompi_osc_rdma_region_t *) ((intptr_t) module->node_comm_info + ompi_comm_rank (module->local_leaders) *
                                                  module->region_size);

            my_data->base = (uint64_t) (intptr_t) module->rank_array;
            /* store my rank in the length field */
            my_data->len = (osc_rdma_size_t) my_rank;

            if (module->use_memory_registration && module->state_handle) {
                assert(module->use_accelerated_btl);
                memcpy (my_data->btl_handle_data, module->state_handle, module->accelerated_btl->btl_registration_handle_size);
            }

            /* gather state data at each node leader */
            if (ompi_comm_size (module->local_leaders) > 1) {
                ret = module->local_leaders->c_coll->coll_allgather (MPI_IN_PLACE, module->region_size, MPI_BYTE, module->node_comm_info,
                                                                    module->region_size, MPI_BYTE, module->local_leaders,
                                                                    module->local_leaders->c_coll->coll_allgather_module);
                if (OMPI_SUCCESS != ret) {
                    opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                                        "leader allgather failed with ompi error code %d", ret);
                    break;
                }
            }

            int base_rank = ompi_comm_rank (module->local_leaders) * ((comm_size + module->node_count - 1) / module->node_count);

            /* fill in the local part of the rank -> node map */
            for (int i = 0 ; i < RANK_ARRAY_COUNT(module) ; ++i) {
                int save_rank = base_rank + i;
                if (save_rank >= comm_size) {
                    break;
                }

                module->rank_array[i] = temp[save_rank];
            }
        }

        free (temp);
    } while (0);

    global_result = synchronize_errorcode(ret, module->comm);

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
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                            "failed to create a shared memory communicator. error code %d", ret);
        return ret;
    }

    local_rank = ompi_comm_rank (module->shared_comm);

    comm_rank = ompi_comm_rank (module->comm);

    ret = ompi_comm_split (module->comm, (0 == local_rank) ? 0 : MPI_UNDEFINED, comm_rank, &module->local_leaders,
                           false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                            "failed to create local leaders communicator. error code %d", ret);
        return ret;
    }

    if (0 == local_rank) {
        values[0] = ompi_comm_size (module->local_leaders);
        values[1] = ompi_comm_rank (module->local_leaders);
    }

    if (ompi_comm_size (module->shared_comm) > 1) {
        ret = module->shared_comm->c_coll->coll_bcast (values, 2, MPI_INT, 0, module->shared_comm,
                                                      module->shared_comm->c_coll->coll_bcast_module);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                                "failed to broadcast local data. error code %d", ret);
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

    ret = module->comm->c_coll->coll_allreduce (MPI_IN_PLACE, values, 4, MPI_LONG, MPI_MIN, module->comm,
                                               module->comm->c_coll->coll_allreduce_module);
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
                                           struct ompi_communicator_t *comm, struct opal_info_t *info,
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
    module->locking_mode   = mca_osc_rdma_component.locking_mode;
    module->acc_single_intrinsic = check_config_value_bool ("acc_single_intrinsic", info);
    module->acc_use_amo = mca_osc_rdma_component.acc_use_amo;
    module->network_amo_max_count = mca_osc_rdma_component.network_amo_max_count;

    module->all_sync.module = module;

    module->flavor = flavor;
    module->win = win;
    module->disp_unit = disp_unit;
    module->size = size;
    module->memory_alignment = mca_osc_rdma_component.memory_alignment;
    if (NULL != info) {
        ompi_osc_base_set_memory_alignment(info, &module->memory_alignment);
    }

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

    ret = ompi_comm_dup(comm, &module->comm);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    opal_output_verbose(MCA_BASE_VERBOSE_INFO, ompi_osc_base_framework.framework_output,
                        "creating osc/rdma window of flavor %d with id %s",
                        flavor, ompi_comm_print_cid(module->comm));

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
    module->use_accelerated_btl = false;
    ret = ompi_osc_rdma_query_accelerated_btls (module->comm, module);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN, ompi_osc_base_framework.framework_output,
                            "could not find an accelerated btl. falling back on "
                            "active-message BTLs");
        ret = ompi_osc_rdma_query_alternate_btls (module->comm, module);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN, ompi_osc_base_framework.framework_output,
                                "no BTL available for RMA window");
            ompi_osc_rdma_free (win);
            return ret;
        }
    }

    /* calculate and store various structure sizes */

    module->region_size = sizeof (ompi_osc_rdma_region_t);
    if (module->use_memory_registration) {
        assert(module->use_accelerated_btl);
        module->region_size += module->accelerated_btl->btl_registration_handle_size;
    }

    module->state_size = sizeof (ompi_osc_rdma_state_t);

    if (MPI_WIN_FLAVOR_DYNAMIC != module->flavor) {
        module->state_size += module->region_size;
    } else {
        module->state_size += mca_osc_rdma_component.max_attach * module->region_size;
    }

    /*
     * These are the info's that this module is interested in
     */
    opal_infosubscribe_subscribe(&win->super, "no_locks", "false", ompi_osc_rdma_set_no_lock_info);

    /*
     * TODO: same_size, same_disp_unit have w_flag entries, but do not appear
     * to be used anywhere.  If that changes, they should be subscribed
     */

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

    /* notify all others if something went wrong */
    ret = synchronize_errorcode(ret, module->comm);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                            "failed to allocate internal state");
        ompi_osc_rdma_free (win);
        return ret;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC == flavor) {
        /* allocate space to store local btl handles for attached regions */
        module->dynamic_handles = (ompi_osc_rdma_handle_t **) calloc (mca_osc_rdma_component.max_attach,
                                                                      sizeof (module->dynamic_handles[0]));
        if (NULL == module->dynamic_handles) {
            ompi_osc_rdma_free (win);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

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
                                           ompi_comm_get_local_cid(module->comm),
                                           module);
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.lock);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_free (win);
        return ret;
    }

    /* fill in window information */
    *model = MPI_WIN_UNIFIED;
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    opal_asprintf(&name, "rdma window %s", ompi_comm_print_cid(module->comm));
    ompi_win_set_name(win, name);
    free(name);

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    ret = ompi_osc_rdma_share_data (module);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                            "failed to share window data with peers");
        ompi_osc_rdma_free (win);
    } else {
        /* for now the leader is always rank 0 in the communicator */
        module->leader = ompi_osc_rdma_module_peer (module, 0);

        opal_output_verbose(MCA_BASE_VERBOSE_INFO, ompi_osc_base_framework.framework_output,
                            "finished creating osc/rdma window with id %s",
                            ompi_comm_print_cid(module->comm));
    }

    return ret;
}


static const char*
ompi_osc_rdma_set_no_lock_info(opal_infosubscriber_t *obj, const char *key, const char *value)
{

    struct ompi_win_t *win = (struct ompi_win_t*) obj;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    bool temp;

    temp = opal_str_to_bool(value);
    if (temp && !module->no_locks) {
        /* clean up the lock hash. it is up to the user to ensure no lock is
         * outstanding from this process when setting the info key */
        OBJ_DESTRUCT(&module->outstanding_locks);
        OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);

        module->no_locks = true;
    } else if (!temp && module->no_locks) {
        int world_size = ompi_comm_size (module->comm);
        int init_limit = world_size > 256 ? 256 : world_size;
        int ret;

        ret = opal_hash_table_init (&module->outstanding_locks, init_limit);
        if (OPAL_SUCCESS != ret) {
            module->no_locks = true;
        }

        module->no_locks = false;
    }
    /* enforce collectiveness... */
    module->comm->c_coll->coll_barrier(module->comm, module->comm->c_coll->coll_barrier_module);
    /*
     * Accept any value
     */
    return module->no_locks ? "true" : "false";
}
