/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

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
    .ucp_context            = NULL,
    .ucp_worker             = NULL,
    .env_initialized        = false,
    .num_incomplete_req_ops = 0,
    .num_modules            = 0
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

    asprintf(&description_str, "Priority of the osc/ucx component (default: %d)",
             mca_osc_ucx_component.priority);
    (void) mca_base_component_var_register(&mca_osc_ucx_component.super.osc_version, "priority", description_str,
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_GROUP, &mca_osc_ucx_component.priority);
    free(description_str);

    opal_common_ucx_mca_var_register(&mca_osc_ucx_component.super.osc_version);

    return OMPI_SUCCESS;
}

static int progress_callback(void) {
    ucp_worker_progress(mca_osc_ucx_component.ucp_worker);
    return 0;
}

static int component_init(bool enable_progress_threads, bool enable_mpi_threads) {
    mca_osc_ucx_component.enable_mpi_threads = enable_mpi_threads;

    opal_common_ucx_mca_register();
    return OMPI_SUCCESS;
}

static int component_finalize(void) {
    int i;
    for (i = 0; i < ompi_proc_world_size(); i++) {
        ucp_ep_h ep = OSC_UCX_GET_EP(&(ompi_mpi_comm_world.comm), i);
        if (ep != NULL) {
            ucp_ep_destroy(ep);
        }
    }

    if (mca_osc_ucx_component.ucp_worker != NULL) {
        ucp_worker_destroy(mca_osc_ucx_component.ucp_worker);
    }

    assert(mca_osc_ucx_component.num_incomplete_req_ops == 0);
    if (mca_osc_ucx_component.env_initialized == true) {
        OBJ_DESTRUCT(&mca_osc_ucx_component.requests);
        ucp_cleanup(mca_osc_ucx_component.ucp_context);
        mca_osc_ucx_component.env_initialized = false;
    }
    opal_common_ucx_mca_deregister();
    return OMPI_SUCCESS;
}

static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct opal_info_t *info, int flavor) {
    if (MPI_WIN_FLAVOR_SHARED == flavor) return -1;
    return mca_osc_ucx_component.priority;
}

static inline int allgather_len_and_info(void *my_info, int my_info_len, char **recv_info,
                                         int *disps, struct ompi_communicator_t *comm) {
    int ret = OMPI_SUCCESS;
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
    for (i = 0; i < comm_size; i++) {
        disps[i] = total_len;
        total_len += lens[i];
    }

    (*recv_info) = (char *)malloc(total_len);

    ret = comm->c_coll->coll_allgatherv(my_info, my_info_len, MPI_BYTE,
                                        (void *)(*recv_info), lens, disps, MPI_BYTE,
                                        comm, comm->c_coll->coll_allgatherv_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    return ret;
}

static inline int mem_map(void **base, size_t size, ucp_mem_h *memh_ptr,
                          ompi_osc_ucx_module_t *module, int flavor) {
    ucp_mem_map_params_t mem_params;
    ucp_mem_attr_t mem_attrs;
    ucs_status_t status;
    int ret = OMPI_SUCCESS;

    if (!(flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE)
        || size == 0) {
        return ret;
    }

    memset(&mem_params, 0, sizeof(ucp_mem_map_params_t));
    mem_params.field_mask = UCP_MEM_MAP_PARAM_FIELD_ADDRESS |
                            UCP_MEM_MAP_PARAM_FIELD_LENGTH |
                            UCP_MEM_MAP_PARAM_FIELD_FLAGS;
    mem_params.length = size;
    if (flavor == MPI_WIN_FLAVOR_ALLOCATE) {
        mem_params.address = NULL;
        mem_params.flags = UCP_MEM_MAP_ALLOCATE;
    } else {
        mem_params.address = (*base);
    }

    /* memory map */

    status = ucp_mem_map(mca_osc_ucx_component.ucp_context, &mem_params, memh_ptr);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_mem_map failed: %d", status);
        ret = OMPI_ERROR;
        goto error;
    }

    mem_attrs.field_mask = UCP_MEM_ATTR_FIELD_ADDRESS | UCP_MEM_ATTR_FIELD_LENGTH;
    status = ucp_mem_query((*memh_ptr), &mem_attrs);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_mem_query failed: %d", status);
        ret = OMPI_ERROR;
        goto error;
    }

    assert(mem_attrs.length >= size);
    if (flavor == MPI_WIN_FLAVOR_CREATE) {
        assert(mem_attrs.address == (*base));
    } else {
        (*base) = mem_attrs.address;
    }

    return ret;
 error:
    ucp_mem_unmap(mca_osc_ucx_component.ucp_context, (*memh_ptr));
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

static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct opal_info_t *info,
                            int flavor, int *model) {
    ompi_osc_ucx_module_t *module = NULL;
    char *name = NULL;
    long values[2];
    int ret = OMPI_SUCCESS;
    ucs_status_t status;
    int i, comm_size = ompi_comm_size(comm);
    int is_eps_ready;
    bool eps_created = false, env_initialized = false;
    ucp_address_t *my_addr = NULL;
    size_t my_addr_len;
    char *recv_buf = NULL;
    void *rkey_buffer = NULL, *state_rkey_buffer = NULL;
    size_t rkey_buffer_size, state_rkey_buffer_size;
    void *state_base = NULL;
    void * my_info = NULL;
    size_t my_info_len;
    int disps[comm_size];
    int rkey_sizes[comm_size];
    uint64_t zero = 0;
    size_t info_offset;
    uint64_t size_u64;

    /* the osc/sm component is the exclusive provider for support for
     * shared memory windows */
    if (flavor == MPI_WIN_FLAVOR_SHARED) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    _osc_ucx_init_lock();

    if (mca_osc_ucx_component.env_initialized == false) {
        ucp_config_t *config = NULL;
        ucp_params_t context_params;
        ucp_worker_params_t worker_params;
        ucp_worker_attr_t worker_attr;

        status = ucp_config_read("MPI", NULL, &config);
        if (UCS_OK != status) {
            OSC_UCX_VERBOSE(1, "ucp_config_read failed: %d", status);
            ret = OMPI_ERROR;
            goto select_unlock;
        }

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

        /* initialize UCP context */

        memset(&context_params, 0, sizeof(context_params));
        context_params.field_mask = UCP_PARAM_FIELD_FEATURES |
                                    UCP_PARAM_FIELD_MT_WORKERS_SHARED |
                                    UCP_PARAM_FIELD_ESTIMATED_NUM_EPS |
                                    UCP_PARAM_FIELD_REQUEST_INIT |
                                    UCP_PARAM_FIELD_REQUEST_SIZE;
        context_params.features = UCP_FEATURE_RMA | UCP_FEATURE_AMO32 | UCP_FEATURE_AMO64;
        context_params.mt_workers_shared = 0;
        context_params.estimated_num_eps = ompi_proc_world_size();
        context_params.request_init = internal_req_init;
        context_params.request_size = sizeof(ompi_osc_ucx_internal_request_t);

        status = ucp_init(&context_params, config, &mca_osc_ucx_component.ucp_context);
        ucp_config_release(config);
        if (UCS_OK != status) {
            OSC_UCX_VERBOSE(1, "ucp_init failed: %d", status);
            ret = OMPI_ERROR;
            goto select_unlock;
        }

        assert(mca_osc_ucx_component.ucp_worker == NULL);
        memset(&worker_params, 0, sizeof(worker_params));
        worker_params.field_mask = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
        worker_params.thread_mode = (mca_osc_ucx_component.enable_mpi_threads == true)
                                    ? UCS_THREAD_MODE_MULTI : UCS_THREAD_MODE_SINGLE;
        status = ucp_worker_create(mca_osc_ucx_component.ucp_context, &worker_params,
                                   &(mca_osc_ucx_component.ucp_worker));
        if (UCS_OK != status) {
            OSC_UCX_VERBOSE(1, "ucp_worker_create failed: %d", status);
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto select_unlock;
        }

        /* query UCP worker attributes */
        worker_attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
        status = ucp_worker_query(mca_osc_ucx_component.ucp_worker, &worker_attr);
        if (UCS_OK != status) {
            OSC_UCX_VERBOSE(1, "ucp_worker_query failed: %d", status);
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto select_unlock;
        }

        if (mca_osc_ucx_component.enable_mpi_threads == true &&
            worker_attr.thread_mode != UCS_THREAD_MODE_MULTI) {
            OSC_UCX_VERBOSE(1, "ucx does not support multithreading");
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto select_unlock;
        }

        mca_osc_ucx_component.env_initialized = true;
        env_initialized = true;
    }
    
    mca_osc_ucx_component.num_modules++;

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
    switch(ret) {
    case OMPI_SUCCESS:
        break;
    case OMPI_ERROR:
        goto error;
    case OMPI_ERR_TEMP_OUT_OF_RESOURCE:
        goto error_nomem;
    default:
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
    asprintf(&name, "ucx window %d", ompi_comm_get_cid(module->comm));
    ompi_win_set_name(win, name);
    free(name);

    module->flavor = flavor;
    module->size = size;

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

    /* exchange endpoints if necessary */
    is_eps_ready = 1;
    for (i = 0; i < comm_size; i++) {
        if (OSC_UCX_GET_EP(module->comm, i) == NULL) {
            is_eps_ready = 0;
            break;
        }
    }

    ret = module->comm->c_coll->coll_allreduce(MPI_IN_PLACE, &is_eps_ready, 1, MPI_INT,
                                               MPI_LAND,
                                               module->comm,
                                               module->comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    if (!is_eps_ready) {
        status = ucp_worker_get_address(mca_osc_ucx_component.ucp_worker,
                                        &my_addr, &my_addr_len);
        if (status != UCS_OK) {
            OSC_UCX_VERBOSE(1, "ucp_worker_get_address failed: %d", status);
            ret = OMPI_ERROR;
            goto error;
        }

        ret = allgather_len_and_info(my_addr, (int)my_addr_len,
                                     &recv_buf, disps, module->comm);
        if (ret != OMPI_SUCCESS) {
            goto error;
        }

        for (i = 0; i < comm_size; i++) {
            if (OSC_UCX_GET_EP(module->comm, i) == NULL) {
                ucp_ep_params_t ep_params;
                ucp_ep_h ep;
                memset(&ep_params, 0, sizeof(ucp_ep_params_t));
                ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
                ep_params.address = (ucp_address_t *)&(recv_buf[disps[i]]);
                status = ucp_ep_create(mca_osc_ucx_component.ucp_worker, &ep_params, &ep);
                if (status != UCS_OK) {
                    OSC_UCX_VERBOSE(1, "ucp_ep_create failed: %d", status);
                    ret = OMPI_ERROR;
                    goto error;
                }

                ompi_comm_peer_lookup(module->comm, i)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_UCX] = ep;
            }
        }

        ucp_worker_release_address(mca_osc_ucx_component.ucp_worker, my_addr);
        my_addr = NULL;
        free(recv_buf);
        recv_buf = NULL;

        eps_created = true;
    }

    ret = mem_map(base, size, &(module->memh), module, flavor);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    state_base = (void *)&(module->state);
    ret = mem_map(&state_base, sizeof(ompi_osc_ucx_state_t), &(module->state_memh),
                  module, MPI_WIN_FLAVOR_CREATE);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    module->win_info_array = calloc(comm_size, sizeof(ompi_osc_ucx_win_info_t));
    if (module->win_info_array == NULL) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    module->state_info_array = calloc(comm_size, sizeof(ompi_osc_ucx_win_info_t));
    if (module->state_info_array == NULL) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    if (size > 0 && (flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE)) {
        status = ucp_rkey_pack(mca_osc_ucx_component.ucp_context, module->memh,
                               &rkey_buffer, &rkey_buffer_size);
        if (status != UCS_OK) {
            OSC_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", status);
            ret = OMPI_ERROR;
            goto error;
        }
    } else {
        rkey_buffer_size = 0;
    }

    status = ucp_rkey_pack(mca_osc_ucx_component.ucp_context, module->state_memh,
                           &state_rkey_buffer, &state_rkey_buffer_size);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", status);
        ret = OMPI_ERROR;
        goto error;
    }

    size_u64 = (uint64_t)size;
    my_info_len = 3 * sizeof(uint64_t) + rkey_buffer_size + state_rkey_buffer_size;
    my_info = malloc(my_info_len);
    if (my_info == NULL) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    info_offset = 0;

    if (flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE) {
        memcpy_off(my_info, base, sizeof(uint64_t), info_offset);
    } else {
        memcpy_off(my_info, &zero, sizeof(uint64_t), info_offset);
    }
    memcpy_off(my_info, &state_base, sizeof(uint64_t), info_offset);
    memcpy_off(my_info, &size_u64, sizeof(uint64_t), info_offset);
    memcpy_off(my_info, rkey_buffer, rkey_buffer_size, info_offset);
    memcpy_off(my_info, state_rkey_buffer, state_rkey_buffer_size, info_offset);

    assert(my_info_len == info_offset);

    ret = allgather_len_and_info(my_info, (int)my_info_len, &recv_buf, disps, module->comm);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    ret = comm->c_coll->coll_allgather((void *)&rkey_buffer_size, 1, MPI_INT,
                                       rkey_sizes, 1, MPI_INT, comm,
                                       comm->c_coll->coll_allgather_module);
    if (OMPI_SUCCESS != ret) {
        goto error;
    }

    for (i = 0; i < comm_size; i++) {
        ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, i);
        uint64_t dest_size;
        assert(ep != NULL);

        info_offset = disps[i];

        memcpy(&(module->win_info_array[i]).addr, &recv_buf[info_offset], sizeof(uint64_t));
        info_offset += sizeof(uint64_t);
        memcpy(&(module->state_info_array[i]).addr, &recv_buf[info_offset], sizeof(uint64_t));
        info_offset += sizeof(uint64_t);
        memcpy(&dest_size, &recv_buf[info_offset], sizeof(uint64_t));
        info_offset += sizeof(uint64_t);

        (module->win_info_array[i]).rkey_init = false;
        if (dest_size > 0 && (flavor == MPI_WIN_FLAVOR_ALLOCATE || flavor == MPI_WIN_FLAVOR_CREATE)) {
            status = ucp_ep_rkey_unpack(ep, &recv_buf[info_offset],
                                        &((module->win_info_array[i]).rkey));
            if (status != UCS_OK) {
                OSC_UCX_VERBOSE(1, "ucp_ep_rkey_unpack failed: %d", status);
                ret = OMPI_ERROR;
                goto error;
            }
            info_offset += rkey_sizes[i];
            (module->win_info_array[i]).rkey_init = true;
        }

        status = ucp_ep_rkey_unpack(ep, &recv_buf[info_offset],
                                    &((module->state_info_array[i]).rkey));
        if (status != UCS_OK) {
            OSC_UCX_VERBOSE(1, "ucp_ep_rkey_unpack failed: %d", status);
            ret = OMPI_ERROR;
            goto error;
        }
        (module->state_info_array[i]).rkey_init = true;
    }

    free(my_info);
    free(recv_buf);

    if (rkey_buffer_size != 0) {
        ucp_rkey_buffer_release(rkey_buffer);
    }
    ucp_rkey_buffer_release(state_rkey_buffer);

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
    OBJ_CONSTRUCT(&module->outstanding_locks, opal_hash_table_t);
    OBJ_CONSTRUCT(&module->pending_posts, opal_list_t);
    module->global_ops_num = 0;
    module->per_target_ops_nums = calloc(comm_size, sizeof(int));
    module->start_grp_ranks = NULL;
    module->lock_all_is_nocheck = false;

    ret = opal_hash_table_init(&module->outstanding_locks, comm_size);
    if (ret != OPAL_SUCCESS) {
        goto error;
    }

    win->w_osc_module = &module->super;

    /* sync with everyone */

    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);
    if (ret != OMPI_SUCCESS) {
        goto error;
    }

    return ret;

 error:
    if (my_addr) ucp_worker_release_address(mca_osc_ucx_component.ucp_worker, my_addr);
    if (recv_buf) free(recv_buf);
    if (my_info) free(my_info);
    for (i = 0; i < comm_size; i++) {
        if ((module->win_info_array[i]).rkey != NULL) {
            ucp_rkey_destroy((module->win_info_array[i]).rkey);
        }
        if ((module->state_info_array[i]).rkey != NULL) {
            ucp_rkey_destroy((module->state_info_array[i]).rkey);
        }
    }
    if (rkey_buffer) ucp_rkey_buffer_release(rkey_buffer);
    if (state_rkey_buffer) ucp_rkey_buffer_release(state_rkey_buffer);
    if (module->win_info_array) free(module->win_info_array);
    if (module->state_info_array) free(module->state_info_array);
    if (module->disp_units) free(module->disp_units);
    if (module->comm) ompi_comm_free(&module->comm);
    if (module->per_target_ops_nums) free(module->per_target_ops_nums);
    if (eps_created) {
        for (i = 0; i < comm_size; i++) {
            ucp_ep_h ep = OSC_UCX_GET_EP(module->comm, i);
            ucp_ep_destroy(ep);
        }
    }
    if (module) {
        free(module);
        ompi_osc_ucx_unregister_progress();
    }

error_nomem:
    if (env_initialized == true) {
        OBJ_DESTRUCT(&mca_osc_ucx_component.requests);
        ucp_worker_destroy(mca_osc_ucx_component.ucp_worker);
        ucp_cleanup(mca_osc_ucx_component.ucp_context);
        mca_osc_ucx_component.env_initialized = false;
    }
    return ret;
}

int ompi_osc_find_attached_region_position(ompi_osc_dynamic_win_info_t *dynamic_wins,
                                           int min_index, int max_index,
                                           uint64_t base, size_t len, int *insert) {
    int mid_index = (max_index + min_index) >> 1;

    if (min_index > max_index) {
        (*insert) = min_index;
        return -1;
    }

    if (dynamic_wins[mid_index].base > base) {
        return ompi_osc_find_attached_region_position(dynamic_wins, min_index, mid_index-1,
                                                      base, len, insert);
    } else if (base + len < dynamic_wins[mid_index].base + dynamic_wins[mid_index].size) {
        return mid_index;
    } else {
        return ompi_osc_find_attached_region_position(dynamic_wins, mid_index+1, max_index,
                                                      base, len, insert);
    }
}

int ompi_osc_ucx_win_attach(struct ompi_win_t *win, void *base, size_t len) {
    ompi_osc_ucx_module_t *module = (ompi_osc_ucx_module_t*) win->w_osc_module;
    int insert_index = -1, contain_index;
    void *rkey_buffer;
    size_t rkey_buffer_size;
    int ret = OMPI_SUCCESS;
    ucs_status_t status;

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

    ret = mem_map(&base, len, &(module->local_dynamic_win_info[insert_index].memh),
                  module, MPI_WIN_FLAVOR_CREATE);
    if (ret != OMPI_SUCCESS) {
        return ret;
    }

    module->state.dynamic_wins[insert_index].base = (uint64_t)base;
    module->state.dynamic_wins[insert_index].size = len;

    status = ucp_rkey_pack(mca_osc_ucx_component.ucp_context,
                           module->local_dynamic_win_info[insert_index].memh,
                           &rkey_buffer, (size_t *)&rkey_buffer_size);
    if (status != UCS_OK) {
        OSC_UCX_VERBOSE(1, "ucp_rkey_pack failed: %d", status);
        return OMPI_ERROR;
    }

    assert(rkey_buffer_size <= OMPI_OSC_UCX_RKEY_BUF_MAX);
    memcpy((char *)(module->state.dynamic_wins[insert_index].rkey_buffer),
           (char *)rkey_buffer, rkey_buffer_size);

    module->local_dynamic_win_info[insert_index].refcnt++;
    module->state.dynamic_win_count++;

    ucp_rkey_buffer_release(rkey_buffer);

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
        ucp_mem_unmap(mca_osc_ucx_component.ucp_context,
                      module->local_dynamic_win_info[contain].memh);
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
    int i, ret;

    assert(module->global_ops_num == 0);
    assert(module->lock_count == 0);
    assert(opal_list_is_empty(&module->pending_posts) == true);
    OBJ_DESTRUCT(&module->outstanding_locks);
    OBJ_DESTRUCT(&module->pending_posts);

    while (module->state.lock != TARGET_LOCK_UNLOCKED) {
        /* not sure if this is required */
        ucp_worker_progress(mca_osc_ucx_component.ucp_worker);
    }

    ret = opal_common_ucx_worker_flush(mca_osc_ucx_component.ucp_worker);
    if (OMPI_SUCCESS != ret) {
        OSC_UCX_VERBOSE(1, "opal_common_ucx_worker_flush failed: %d", ret);
    }

    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);

    for (i = 0; i < ompi_comm_size(module->comm); i++) {
        if ((module->win_info_array[i]).rkey_init == true) {
            ucp_rkey_destroy((module->win_info_array[i]).rkey);
            (module->win_info_array[i]).rkey_init = false;
        }
        ucp_rkey_destroy((module->state_info_array[i]).rkey);
    }
    free(module->win_info_array);
    free(module->state_info_array);

    free(module->per_target_ops_nums);

    if ((module->flavor == MPI_WIN_FLAVOR_ALLOCATE || module->flavor == MPI_WIN_FLAVOR_CREATE)
        && module->size > 0) {
        ucp_mem_unmap(mca_osc_ucx_component.ucp_context, module->memh);
    }
    ucp_mem_unmap(mca_osc_ucx_component.ucp_context, module->state_memh);

    if (module->disp_units) free(module->disp_units);
    ompi_comm_free(&module->comm);

    free(module);
    ompi_osc_ucx_unregister_progress();

    return ret;
}
