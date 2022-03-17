/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/printf.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "opal/mca/common/ucx/common_ucx.h"

#include "osc_ucx.h"
#include "osc_ucx_request.h"

#define memcpy_off(_dst, _src, _len, _off)        \
    memcpy(((char*)(_dst)) + (_off), _src, _len); \
    (_off) += (_len);

opal_mutex_t mca_osc_service_mutex = OPAL_MUTEX_STATIC_INIT;
static void _osc_ucx_init_lock(void)
{
    if(mca_osc_ucx_component.enable_mpi_threads) {
        opal_mutex_lock(&mca_osc_service_mutex);
    }
}
static void _osc_ucx_init_unlock(void)
{
    if(mca_osc_ucx_component.enable_mpi_threads) {
        opal_mutex_unlock(&mca_osc_service_mutex);
    }
}


static int component_open(void);
static int component_register(void);
static int component_init(bool enable_progress_threads, bool enable_mpi_threads);
static int component_finalize(void);
static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor);
static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct opal_info_t *info,
                            int flavor, int *model);
static void ompi_osc_ucx_unregister_progress(void);

ompi_osc_ucx_component_t mca_osc_ucx_component = {
    { /* ompi_osc_base_component_t */
        .osc_version = {
            OMPI_OSC_BASE_VERSION_3_0_0,
            .mca_component_name = "ucx",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = component_open,
            .mca_register_component_params = component_register,
        },
        .osc_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        .osc_init = component_init,
        .osc_query = component_query,
        .osc_select = component_select,
        .osc_finalize = component_finalize,
    },
    .wpool                  = NULL,
    .env_initialized        = false,
    .num_incomplete_req_ops = 0,
    .num_modules            = 0,
    .acc_single_intrinsic   = false
};

ompi_osc_ucx_module_t ompi_osc_ucx_module_template = {
    {
        .osc_win_attach = ompi_osc_ucx_win_attach,
        .osc_win_detach = ompi_osc_ucx_win_detach,
        .osc_free = ompi_osc_ucx_free,

        .osc_put = ompi_osc_ucx_put,
        .osc_get = ompi_osc_ucx_get,
        .osc_accumulate = ompi_osc_ucx_accumulate,
        .osc_compare_and_swap = ompi_osc_ucx_compare_and_swap,
        .osc_fetch_and_op = ompi_osc_ucx_fetch_and_op,
        .osc_get_accumulate = ompi_osc_ucx_get_accumulate,

        .osc_rput = ompi_osc_ucx_rput,
        .osc_rget = ompi_osc_ucx_rget,
        .osc_raccumulate = ompi_osc_ucx_raccumulate,
        .osc_rget_accumulate = ompi_osc_ucx_rget_accumulate,

        .osc_fence = ompi_osc_ucx_fence,

        .osc_start = ompi_osc_ucx_start,
        .osc_complete = ompi_osc_ucx_complete,
        .osc_post = ompi_osc_ucx_post,
        .osc_wait = ompi_osc_ucx_wait,
        .osc_test = ompi_osc_ucx_test,

        .osc_lock = ompi_osc_ucx_lock,
        .osc_unlock = ompi_osc_ucx_unlock,
        .osc_lock_all = ompi_osc_ucx_lock_all,
        .osc_unlock_all = ompi_osc_ucx_unlock_all,

        .osc_sync = ompi_osc_ucx_sync,
        .osc_flush = ompi_osc_ucx_flush,
        .osc_flush_all = ompi_osc_ucx_flush_all,
        .osc_flush_local = ompi_osc_ucx_flush_local,
        .osc_flush_local_all = ompi_osc_ucx_flush_local_all,
    }
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

    param = mca_base_var_find("ompi", "osc", "ucx", key);
    if (0 <= param) {
        (void) mca_base_var_get_value(param, &flag_value, NULL, NULL);
    }

    return flag_value[0];
}

static int component_open(void) {
    return OMPI_SUCCESS;
}

static int component_register(void) {
    unsigned major          = 0;
    unsigned minor          = 0;
    unsigned release_number = 0;
    char *description_str;

    ucp_get_version(&major, &minor, &release_number);

    mca_osc_ucx_component.priority = UCX_VERSION(major, minor, release_number) >= UCX_VERSION(1, 5, 0) ? 60 : 0;

    opal_asprintf(&description_str, "Priority of the osc/ucx component (default: %d)",
             mca_osc_ucx_component.priority);
    (void) mca_base_component_var_register(&mca_osc_ucx_component.super.osc_version, "priority", description_str,
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_ucx_component.priority);
    free(description_str);

    mca_osc_ucx_component.no_locks = false;

    opal_asprintf(&description_str, "Enable optimizations available only if MPI_LOCK is "
             "not used. Info key of same name overrides this value (default: %s)",
             mca_osc_ucx_component.no_locks  ? "true" : "false");
    (void) mca_base_component_var_register(&mca_osc_ucx_component.super.osc_version, "no_locks", description_str,
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_ucx_component.no_locks);
    free(description_str);

    mca_osc_ucx_component.acc_single_intrinsic = false;
    opal_asprintf(&description_str, "Enable optimizations for MPI_Fetch_and_op, MPI_Accumulate, etc for codes "
             "that will not use anything more than a single predefined datatype (default: %s)",
             mca_osc_ucx_component.acc_single_intrinsic  ? "true" : "false");
    (void) mca_base_component_var_register(&mca_osc_ucx_component.super.osc_version, "acc_single_intrinsic",
                                           description_str, MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_ucx_component.acc_single_intrinsic);
    free(description_str);

    opal_common_ucx_mca_var_register(&mca_osc_ucx_component.super.osc_version);

    return OMPI_SUCCESS;
}

static int progress_callback(void) {
    if (mca_osc_ucx_component.wpool != NULL) {
        opal_common_ucx_wpool_progress(mca_osc_ucx_component.wpool);
    }
    return 0;
}

static int component_init(bool enable_progress_threads, bool enable_mpi_threads) {
    mca_osc_ucx_component.enable_mpi_threads = enable_mpi_threads;
    mca_osc_ucx_component.wpool = opal_common_ucx_wpool_allocate();
    opal_common_ucx_mca_register();
    return OMPI_SUCCESS;
}

static int component_finalize(void) {
    opal_common_ucx_mca_deregister();
    if (mca_osc_ucx_component.env_initialized) {
        opal_common_ucx_wpool_finalize(mca_osc_ucx_component.wpool);
    }
    opal_common_ucx_wpool_free(mca_osc_ucx_component.wpool);
    return OMPI_SUCCESS;
}

static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor) {
    if (MPI_WIN_FLAVOR_SHARED == flavor) return -1;
    return mca_osc_ucx_component.priority;
}

static int exchange_len_info(void *my_info, size_t my_info_len, char **recv_info_ptr,
                             int **disps_ptr, void *metadata)
{
    int ret = OMPI_SUCCESS;
    struct ompi_communicator_t *comm = (struct ompi_communicator_t *)metadata;
    int comm_size = ompi_comm_size(comm);
    int lens[comm_size];
    int total_len, i;

    ret = comm->c_coll->coll_allgather(&my_info_len, 1, MPI_INT,
                                       lens, 1, MPI_INT, comm,
                                       comm->c_coll->coll_allgather_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    total_len = 0;
    (*disps_ptr) = (int *)calloc(comm_size, sizeof(int));
    for (i = 0; i < comm_size; i++) {
        (*disps_ptr)[i] = total_len;
        total_len += lens[i];
    }

    (*recv_info_ptr) = (char *)calloc(total_len, sizeof(char));
    ret = comm->c_coll->coll_allgatherv(my_info, my_info_len, MPI_BYTE,
                                        (void *)(*recv_info_ptr), lens, (*disps_ptr), MPI_BYTE,
                                        comm, comm->c_coll->coll_allgatherv_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return ret;
}

static void ompi_osc_ucx_unregister_progress()
{
    int ret;

    /* May be called concurrently - protect */
    _osc_ucx_init_lock();

    mca_osc_ucx_component.num_modules--;
    OSC_UCX_ASSERT(mca_osc_ucx_component.num_modules >= 0);
    if (0 == mca_osc_ucx_component.num_modules) {
        ret = opal_progress_unregister(progress_callback);
        if (OMPI_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_progress_unregister failed: %d", ret);
        }
    }

    _osc_ucx_init_unlock();
}

static const char* ompi_osc_ucx_set_no_lock_info(opal_infosubscriber_t *obj, const char *key, const char *value)
{

    struct ompi_win_t *win = (struct ompi_win_t*) obj;
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t *)win->w_osc_module;
    bool temp;

    temp = opal_str_to_bool(value);

    if (temp && !module->no_locks) {
        /* clean up the lock hash. it is up to the user to ensure no lock is
         * outstanding from this process when setting the info key */
        OBJ_DESTRUCT(&module->outstanding_locks);
        module->no_locks = true;
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    } else if (!temp && module->no_locks) {
        int comm_size = ompi_comm_size (module->comm);
        int ret;

        OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);
        ret = opal_hash_table_init (&module->outstanding_locks, comm_size);
        if (OPAL_SUCCESS != ret) {
            module->no_locks = true;
        } else {
            module->no_locks = false;
        }
        win->w_flags &= ~OMPI_WIN_NO_LOCKS;
    }
    module->comm->c_coll->coll_barrier(module->comm, module->comm->c_coll->coll_barrier_module);
    return module->no_locks ? "true" : "false";
}

static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct opal_info_t *info,
                            int flavor, int *model) {
    ompi_osc_ucx_module_t *module = NULL;
    char *name = NULL;
    long values[2];
    int ret = OMPI_SUCCESS;
    //ucs_status_t status;
    int i, comm_size = ompi_comm_size(comm);
    bool env_initialized = false;
    void *state_base = NULL;
    opal_common_ucx_mem_type_t mem_type;
    uint64_t zero = 0;
    char *my_mem_addr;
    int my_mem_addr_size;
    void * my_info = NULL;
    char *recv_buf = NULL;

    /* the osc/sm component is the exclusive provider for support for
     * shared memory windows */
    if (flavor == MPI_WIN_FLAVOR_SHARED) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    /* May be called concurrently - protect */
    _osc_ucx_init_lock();

    if (mca_osc_ucx_component.env_initialized == false) {
        /* Lazy initialization of the global state.
         * As not all of the MPI applications are using One-Sided functionality
         * we don't want to initialize in the component_init()
         */

        OBJ_CONSTRUCT(&mca_osc_ucx_component.requests, opal_free_list_t);
        ret = opal_free_list_init (&mca_osc_ucx_component.requests,
                                   sizeof(ompi_osc_ucx_request_t),
                                   opal_cache_line_size,
                                   OBJ_CLASS(ompi_osc_ucx_request_t),
                                   0, 0, 8, 0, 8, NULL, 0, NULL, NULL, NULL);
        if (OMPI_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_free_list_init failed: %d", ret);
            goto select_unlock;
        }

        ret = opal_common_ucx_wpool_init(mca_osc_ucx_component.wpool,
                                         ompi_proc_world_size(),
                                         mca_osc_ucx_component.enable_mpi_threads);
        if (OMPI_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_common_ucx_wpool_init failed: %d", ret);
            goto select_unlock;
        }

        /* Make sure that all memory updates performed above are globally
         * observable before (mca_osc_ucx_component.env_initialized = true)
         */
        mca_osc_ucx_component.env_initialized = true;
        env_initialized = true;
    }

    /* Account for the number of active "modules" = MPI windows */
    mca_osc_ucx_component.num_modules++;

    /* If this is the first window to be registered - register the progress
     * callback
     */
    OSC_UCX_ASSERT(mca_osc_ucx_component.num_modules > 0);
    if (1 == mca_osc_ucx_component.num_modules) {
        ret = opal_progress_register(progress_callback);
        if (OMPI_SUCCESS != ret) {
            OSC_UCX_VERBOSE(1, "opal_progress_register failed: %d", ret);
            goto select_unlock;
        }
    }

select_unlock:
    _osc_ucx_init_unlock();
    if (ret) {
        goto error;
    }

    /* create module structure */
    module = (ompi_osc_ucx_module_t *)calloc(1, sizeof(ompi_osc_ucx_module_t));
    if (module == NULL) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error_nomem;
    }

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_ucx_module_template, sizeof(ompi_osc_base_module_t));

    ret = ompi_comm_dup(comm, &module->comm);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    *model = MPI_WIN_UNIFIED;
    opal_asprintf(&name, "ucx window %s", ompi_comm_print_cid(module->comm));
    ompi_win_set_name(win, name);
    free(name);

    module->flavor = flavor;
    module->size = size;
    module->no_locks = check_config_value_bool ("no_locks", info);
    module->acc_single_intrinsic = check_config_value_bool ("acc_single_intrinsic", info);

    /* share everyone's displacement units. Only do an allgather if
       strictly necessary, since it requires O(p) state. */
    values[0] = disp_unit;
    values[1] = -disp_unit;

    ret = module->comm->c_coll->coll_allreduce(MPI_IN_PLACE, values, 2, MPI_LONG,
                                               MPI_MIN, module->comm,
                                               module->comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    if (values[0] == -values[1]) { /* everyone has the same disp_unit, we do not need O(p) space */
        module->disp_unit = disp_unit;
    } else { /* different disp_unit sizes, allocate O(p) space to store them */
        module->disp_unit = -1;
        module->disp_units = calloc(comm_size, sizeof(int));
        if (module->disp_units == NULL) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto error;
        }

        ret = module->comm->c_coll->coll_allgather(&disp_unit, 1, MPI_INT,
                                                   module->disp_units, 1, MPI_INT,
                                                   module->comm,
                                                   module->comm->c_coll->coll_allgather_module);
        if (OMPI_SUCCESS != ret) {
            goto error;
        }
    }

    ret = opal_common_ucx_wpctx_create(mca_osc_ucx_component.wpool, comm_size,
                                     &exchange_len_info, (void *)module->comm,
                                     &module->ctx);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    if (flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE) {
        switch (flavor) {
        case MPI_WIN_FLAVOR_ALLOCATE:
            mem_type = OPAL_COMMON_UCX_MEM_ALLOCATE_MAP;
            break;
        case MPI_WIN_FLAVOR_CREATE:
            mem_type = OPAL_COMMON_UCX_MEM_MAP;
            break;
        }

        ret = opal_common_ucx_wpmem_create(module->ctx, base, size,
                                         mem_type, &exchange_len_info,
                                         OPAL_COMMON_UCX_WPMEM_ADDR_EXCHANGE_FULL,
                                         (void *)module->comm,
                                           &my_mem_addr, &my_mem_addr_size,
                                           &module->mem);
        if (ret != OMPI_SUCCESS) {
            goto error;
        }
    }

    state_base = (void *)&(module->state);
    ret = opal_common_ucx_wpmem_create(module->ctx, &state_base,
                                     sizeof(ompi_osc_ucx_state_t),
                                     OPAL_COMMON_UCX_MEM_MAP,
                                     &exchange_len_info,
                                     OPAL_COMMON_UCX_WPMEM_ADDR_EXCHANGE_FULL,
                                     (void *)module->comm,
                                     &my_mem_addr, &my_mem_addr_size,
                                     &module->state_mem);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    /* exchange window addrs */
    my_info = malloc(2 * sizeof(uint64_t));
    if (my_info == NULL) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    if (flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE) {
        memcpy(my_info, base, sizeof(uint64_t));
    } else {
        memcpy(my_info, &zero, sizeof(uint64_t));
    }
    memcpy((char*)my_info + sizeof(uint64_t), &state_base, sizeof(uint64_t));

    recv_buf = (char *)calloc(comm_size, 2 * sizeof(uint64_t));
    ret = comm->c_coll->coll_allgather((void *)my_info, 2 * sizeof(uint64_t),
                                       MPI_BYTE, recv_buf, 2 * sizeof(uint64_t),
                                       MPI_BYTE, comm, comm->c_coll->coll_allgather_module);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    module->addrs = calloc(comm_size, sizeof(uint64_t));
    module->state_addrs = calloc(comm_size, sizeof(uint64_t));
    for (i = 0; i < comm_size; i++) {
        memcpy(&(module->addrs[i]), recv_buf + i * 2 * sizeof(uint64_t), sizeof(uint64_t));
        memcpy(&(module->state_addrs[i]), recv_buf + i * 2 * sizeof(uint64_t) + sizeof(uint64_t), sizeof(uint64_t));
    }
    free(recv_buf);

    /* init window state */
    module->state.lock = TARGET_LOCK_UNLOCKED;
    module->state.post_index = 0;
    memset((void *)module->state.post_state, 0, sizeof(uint64_t) * OMPI_OSC_UCX_POST_PEER_MAX);
    module->state.complete_count = 0;
    module->state.req_flag = 0;
    module->state.acc_lock = TARGET_LOCK_UNLOCKED;
    module->state.dynamic_win_count = 0;
    for (i = 0; i < OMPI_OSC_UCX_ATTACH_MAX; i++) {
        module->local_dynamic_win_info[i].refcnt = 0;
    }
    module->epoch_type.access = NONE_EPOCH;
    module->epoch_type.exposure = NONE_EPOCH;
    module->lock_count = 0;
    module->post_count = 0;
    module->start_group = NULL;
    module->post_group = NULL;
    OBJ_CONSTRUCT(&module->pending_posts, opal_list_t);
    module->start_grp_ranks = NULL;
    module->lock_all_is_nocheck = false;

    if (!module->no_locks) {
        OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);
        ret = opal_hash_table_init(&module->outstanding_locks, comm_size);
        if (ret != OPAL_SUCCESS) {
            goto error;
        }
    } else {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    win->w_osc_module = &module->super;

    opal_infosubscribe_subscribe(&win->super, "no_locks", "false", ompi_osc_ucx_set_no_lock_info);

    /* sync with everyone */

    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    return ret;

error:
    if (module->disp_units) free(module->disp_units);
    if (module->comm) ompi_comm_free(&module->comm);
    free(module);

error_nomem:
    if (env_initialized == true) {
        opal_common_ucx_wpool_finalize(mca_osc_ucx_component.wpool);
        OBJ_DESTRUCT(&mca_osc_ucx_component.requests);
        mca_osc_ucx_component.env_initialized = false;
    }

    ompi_osc_ucx_unregister_progress();
    return ret;
}

int ompi_osc_find_attached_region_position(ompi_osc_dynamic_win_info_t *dynamic_wins,
                                           int min_index, int max_index,
                                           uint64_t base, size_t len, int *insert) {
    int mid_index = (max_index + min_index) >> 1;

    if (dynamic_wins[mid_index].size == 1) {
        len = 0;
    }

    if (min_index > max_index) {
        (*insert) = min_index;
        return -1;
    }

    if (dynamic_wins[mid_index].base > base) {
        return ompi_osc_find_attached_region_position(dynamic_wins, min_index, mid_index-1,
                                                      base, len, insert);
    } else if (base + len <= dynamic_wins[mid_index].base + dynamic_wins[mid_index].size) {
        return mid_index;
    } else {
        return ompi_osc_find_attached_region_position(dynamic_wins, mid_index+1, max_index,
                                                      base, len, insert);
    }
}

int ompi_osc_ucx_win_attach(struct ompi_win_t *win, void *base, size_t len) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int insert_index = -1, contain_index;
    int ret = OMPI_SUCCESS;

    if (module->state.dynamic_win_count >= OMPI_OSC_UCX_ATTACH_MAX) {
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    if (module->state.dynamic_win_count > 0) {
        contain_index = ompi_osc_find_attached_region_position((ompi_osc_dynamic_win_info_t *)module->state.dynamic_wins,
                                                               0, (int)module->state.dynamic_win_count,
                                                               (uint64_t)base, len, &insert_index);
        if (contain_index >= 0) {
            module->local_dynamic_win_info[contain_index].refcnt++;
            return ret;
        }

        assert(insert_index >= 0 && (uint64_t)insert_index < module->state.dynamic_win_count);

        memmove((void *)&module->local_dynamic_win_info[insert_index+1],
                (void *)&module->local_dynamic_win_info[insert_index],
                (OMPI_OSC_UCX_ATTACH_MAX - (insert_index + 1)) * sizeof(ompi_osc_local_dynamic_win_info_t));
        memmove((void *)&module->state.dynamic_wins[insert_index+1],
                (void *)&module->state.dynamic_wins[insert_index],
                (OMPI_OSC_UCX_ATTACH_MAX - (insert_index + 1)) * sizeof(ompi_osc_dynamic_win_info_t));
    } else {
        insert_index = 0;
    }

    ret = opal_common_ucx_wpmem_create(module->ctx, &base, len,
                                       OPAL_COMMON_UCX_MEM_MAP, &exchange_len_info,
                                       OPAL_COMMON_UCX_WPMEM_ADDR_EXCHANGE_DIRECT,
                                       (void *)module->comm,
                                       &(module->local_dynamic_win_info[insert_index].my_mem_addr),
                                       &(module->local_dynamic_win_info[insert_index].my_mem_addr_size),
                                       &(module->local_dynamic_win_info[insert_index].mem));
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    module->state.dynamic_wins[insert_index].base = (uint64_t)base;
    module->state.dynamic_wins[insert_index].size = len;

    memcpy((char *)(module->state.dynamic_wins[insert_index].mem_addr),
           (char *)module->local_dynamic_win_info[insert_index].my_mem_addr,
           module->local_dynamic_win_info[insert_index].my_mem_addr_size);

    module->local_dynamic_win_info[insert_index].refcnt++;
    module->state.dynamic_win_count++;

    return ret;
}

int ompi_osc_ucx_win_detach(struct ompi_win_t *win, const void *base) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int insert, contain;

    assert(module->state.dynamic_win_count > 0);

    contain = ompi_osc_find_attached_region_position((ompi_osc_dynamic_win_info_t *)module->state.dynamic_wins,
                                                     0, (int)module->state.dynamic_win_count,
                                                     (uint64_t)base, 1, &insert);
    assert(contain >= 0 && (uint64_t)contain < module->state.dynamic_win_count);

    /* if we can't find region - just exit */
    if (contain < 0) {
        return OMPI_SUCCESS;
    }

    module->local_dynamic_win_info[contain].refcnt--;
    if (module->local_dynamic_win_info[contain].refcnt == 0) {
        opal_common_ucx_wpmem_free(module->local_dynamic_win_info[contain].mem);
        memmove((void *)&(module->local_dynamic_win_info[contain]),
                (void *)&(module->local_dynamic_win_info[contain+1]),
                (OMPI_OSC_UCX_ATTACH_MAX - (contain + 1)) * sizeof(ompi_osc_local_dynamic_win_info_t));
        memmove((void *)&module->state.dynamic_wins[contain],
                (void *)&module->state.dynamic_wins[contain+1],
                (OMPI_OSC_UCX_ATTACH_MAX - (contain + 1)) * sizeof(ompi_osc_dynamic_win_info_t));

        module->state.dynamic_win_count--;
    }

    return OMPI_SUCCESS;
}

int ompi_osc_ucx_free(struct ompi_win_t *win) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int ret;
    uint64_t i;

    assert(module->lock_count == 0);
    assert(opal_list_is_empty(&module->pending_posts) == true);
    if(!module->no_locks) {
        OBJ_DESTRUCT(&module->outstanding_locks);
    }
    OBJ_DESTRUCT(&module->pending_posts);

    if (NULL != module->mem) {
        opal_common_ucx_wpmem_flush(module->mem, OPAL_COMMON_UCX_SCOPE_WORKER, 0);
    }

    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

   /* MPI_Win_free should detach any memory attached to dynamic windows */
    for (i = 0; i < module->state.dynamic_win_count; i++) {
        assert(module->local_dynamic_win_info[i].refcnt == 1);
        opal_common_ucx_wpmem_free(module->local_dynamic_win_info[i].mem);
    }
    module->state.dynamic_win_count = 0;

    free(module->addrs);
    free(module->state_addrs);

    opal_common_ucx_wpmem_free(module->state_mem);
    if (NULL != module->mem) {
        opal_common_ucx_wpmem_free(module->mem);
    }

    opal_common_ucx_wpctx_release(module->ctx);

    if (module->disp_units) {
        free(module->disp_units);
    }
    ompi_comm_free(&module->comm);

    free(module);
    ompi_osc_ucx_unregister_progress();

    return ret;
}
