/**
 * Copyright (c) 2021      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2025 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_ucc.h"
#include "coll_ucc_common.h"
#include "coll_ucc_dtypes.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

static int ucc_comm_attr_keyval;
/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_ucc_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

static void mca_coll_ucc_module_clear(mca_coll_ucc_module_t *ucc_module)
{
    ucc_module->ucc_team                              = NULL;
    ucc_module->previous_allreduce                    = NULL;
    ucc_module->previous_allreduce_module             = NULL;
    ucc_module->previous_iallreduce                   = NULL;
    ucc_module->previous_iallreduce_module            = NULL;
    ucc_module->previous_barrier                      = NULL;
    ucc_module->previous_barrier_module               = NULL;
    ucc_module->previous_ibarrier                     = NULL;
    ucc_module->previous_ibarrier_module              = NULL;
    ucc_module->previous_bcast                        = NULL;
    ucc_module->previous_bcast_module                 = NULL;
    ucc_module->previous_ibcast                       = NULL;
    ucc_module->previous_ibcast_module                = NULL;
    ucc_module->previous_alltoall                     = NULL;
    ucc_module->previous_alltoall_module              = NULL;
    ucc_module->previous_ialltoall                    = NULL;
    ucc_module->previous_ialltoall_module             = NULL;
    ucc_module->previous_alltoallv                    = NULL;
    ucc_module->previous_alltoallv_module             = NULL;
    ucc_module->previous_ialltoallv                   = NULL;
    ucc_module->previous_ialltoallv_module            = NULL;
    ucc_module->previous_allgather                    = NULL;
    ucc_module->previous_allgather_module             = NULL;
    ucc_module->previous_iallgather                   = NULL;
    ucc_module->previous_iallgather_module            = NULL;
    ucc_module->previous_allgatherv                   = NULL;
    ucc_module->previous_allgatherv_module            = NULL;
    ucc_module->previous_iallgatherv                  = NULL;
    ucc_module->previous_iallgatherv_module           = NULL;
    ucc_module->previous_reduce                       = NULL;
    ucc_module->previous_reduce_module                = NULL;
    ucc_module->previous_ireduce                      = NULL;
    ucc_module->previous_ireduce_module               = NULL;
    ucc_module->previous_gather                       = NULL;
    ucc_module->previous_gather_module                = NULL;
    ucc_module->previous_igather                      = NULL;
    ucc_module->previous_igather_module               = NULL;
    ucc_module->previous_gatherv                      = NULL;
    ucc_module->previous_gatherv_module               = NULL;
    ucc_module->previous_igatherv                     = NULL;
    ucc_module->previous_igatherv_module              = NULL;
    ucc_module->previous_reduce_scatter_block         = NULL;
    ucc_module->previous_reduce_scatter_block_module  = NULL;
    ucc_module->previous_ireduce_scatter_block        = NULL;
    ucc_module->previous_ireduce_scatter_block_module = NULL;
    ucc_module->previous_reduce_scatter               = NULL;
    ucc_module->previous_reduce_scatter_module        = NULL;
    ucc_module->previous_ireduce_scatter              = NULL;
    ucc_module->previous_ireduce_scatter_module       = NULL;
    ucc_module->previous_scatterv                     = NULL;
    ucc_module->previous_scatterv_module              = NULL;
    ucc_module->previous_iscatterv                    = NULL;
    ucc_module->previous_iscatterv_module             = NULL;
    ucc_module->previous_scatter                      = NULL;
    ucc_module->previous_scatter_module               = NULL;
    ucc_module->previous_iscatter                     = NULL;
    ucc_module->previous_iscatter_module              = NULL;
    ucc_module->previous_allreduce_init               = NULL;
    ucc_module->previous_allreduce_init_module        = NULL;
    ucc_module->previous_barrier_init                 = NULL;
    ucc_module->previous_barrier_init_module          = NULL;
    ucc_module->previous_bcast_init                   = NULL;
    ucc_module->previous_bcast_init_module            = NULL;
    ucc_module->previous_alltoall_init                = NULL;
    ucc_module->previous_alltoall_init_module         = NULL;
    ucc_module->previous_alltoallv_init               = NULL;
    ucc_module->previous_alltoallv_init_module        = NULL;
    ucc_module->previous_allgather_init               = NULL;
    ucc_module->previous_allgather_init_module        = NULL;
    ucc_module->previous_allgatherv_init              = NULL;
    ucc_module->previous_allgatherv_init_module       = NULL;
    ucc_module->previous_reduce_init                  = NULL;
    ucc_module->previous_reduce_init_module           = NULL;
    ucc_module->previous_gather_init                  = NULL;
    ucc_module->previous_gather_init_module           = NULL;
    ucc_module->previous_gatherv_init                 = NULL;
    ucc_module->previous_gatherv_init_module          = NULL;
    ucc_module->previous_reduce_scatter_block_init    = NULL;
    ucc_module->previous_reduce_scatter_block_init_module = NULL;
    ucc_module->previous_reduce_scatter_init          = NULL;
    ucc_module->previous_reduce_scatter_init_module   = NULL;
    ucc_module->previous_scatterv_init                = NULL;
    ucc_module->previous_scatterv_init_module         = NULL;
    ucc_module->previous_scatter_init                 = NULL;
    ucc_module->previous_scatter_init_module          = NULL;
}

static void mca_coll_ucc_module_construct(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_module_clear(ucc_module);
}

static int mca_coll_ucc_progress(void)
{
    ucc_context_progress(mca_coll_ucc_component.ucc_context);
    return OPAL_SUCCESS;
}

static void mca_coll_ucc_module_destruct(mca_coll_ucc_module_t *ucc_module)
{
    if (ucc_module->comm == &ompi_mpi_comm_world.comm){
        if (OMPI_SUCCESS != ompi_attr_free_keyval(COMM_ATTR, &ucc_comm_attr_keyval, 0)) {
            UCC_ERROR("ucc ompi_attr_free_keyval failed");
        }
    }
    mca_coll_ucc_module_clear(ucc_module);
}

/*
** Communicator free callback
*/
static int ucc_comm_attr_del_fn(MPI_Comm comm, int keyval, void *attr_val, void *extra)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*) attr_val;
    ucc_status_t status;
    while(UCC_INPROGRESS == (status = ucc_team_destroy(ucc_module->ucc_team))) {}
    if (ucc_module->comm == &ompi_mpi_comm_world.comm) {
        if (mca_coll_ucc_component.libucc_initialized) {
            UCC_VERBOSE(1,"finalizing ucc library");
            opal_progress_unregister(mca_coll_ucc_progress);
            ucc_context_destroy(mca_coll_ucc_component.ucc_context);
            ucc_finalize(mca_coll_ucc_component.ucc_lib);
        }
    }
    if (UCC_OK != status) {
        UCC_ERROR("UCC team destroy failed");
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

typedef struct oob_allgather_req{
    void           *sbuf;
    void           *rbuf;
    void           *oob_coll_ctx;
    size_t          msglen;
    int             iter;
    ompi_request_t *reqs[2];
} oob_allgather_req_t;

static ucc_status_t oob_allgather_test(void *req)
{
    oob_allgather_req_t *oob_req = (oob_allgather_req_t*)req;
    ompi_communicator_t *comm    = (ompi_communicator_t *)oob_req->oob_coll_ctx;
    char                *tmpsend = NULL;
    char                *tmprecv = NULL;
    size_t               msglen  = oob_req->msglen;
    int                  probe_count = 5;
    int rank, size, sendto, recvfrom, recvdatafrom,
        senddatafrom, completed, probe, rc;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    if (oob_req->iter == 0) {
        tmprecv = (char*) oob_req->rbuf + (ptrdiff_t)rank * (ptrdiff_t)msglen;
        memcpy(tmprecv, oob_req->sbuf, msglen);
    }
    sendto   = (rank + 1) % size;
    recvfrom = (rank - 1 + size) % size;
    for (; oob_req->iter < size - 1; oob_req->iter++) {
        if (oob_req->iter > 0) {
            probe = 0;
            do {
                ompi_request_test_all(2, oob_req->reqs, &completed, MPI_STATUS_IGNORE);
                probe++;
            } while (!completed && probe < probe_count);
            if (!completed) {
                return UCC_INPROGRESS;
            }
        }
        recvdatafrom = (rank - oob_req->iter - 1 + size) % size;
        senddatafrom = (rank - oob_req->iter + size) % size;
        tmprecv = (char*)oob_req->rbuf + (ptrdiff_t)recvdatafrom * (ptrdiff_t)msglen;
        tmpsend = (char*)oob_req->rbuf + (ptrdiff_t)senddatafrom * (ptrdiff_t)msglen;
        rc = MCA_PML_CALL(isend(tmpsend, msglen, MPI_BYTE, sendto, MCA_COLL_BASE_TAG_UCC,
                           MCA_PML_BASE_SEND_STANDARD, comm, &oob_req->reqs[0]));
        if (OMPI_SUCCESS != rc) {
            return UCC_ERR_NO_MESSAGE;
        }
        rc = MCA_PML_CALL(irecv(tmprecv, msglen, MPI_BYTE, recvfrom,
                           MCA_COLL_BASE_TAG_UCC, comm, &oob_req->reqs[1]));
	if (OMPI_SUCCESS != rc) {
            return UCC_ERR_NO_MESSAGE;
	}
    }
    probe = 0;
    do {
        ompi_request_test_all(2, oob_req->reqs, &completed, MPI_STATUS_IGNORE);
        probe++;
    } while (!completed && probe < probe_count);
    if (!completed) {
        return UCC_INPROGRESS;
    }
    return UCC_OK;
}

static ucc_status_t oob_allgather_free(void *req)
{
    free(req);
    return UCC_OK;
}

static ucc_status_t oob_allgather(void *sbuf, void *rbuf, size_t msglen,
                                  void *oob_coll_ctx, void **req)
{
    oob_allgather_req_t *oob_req = malloc(sizeof(*oob_req));
    oob_req->sbuf                = sbuf;
    oob_req->rbuf                = rbuf;
    oob_req->msglen              = msglen;
    oob_req->oob_coll_ctx        = oob_coll_ctx;
    oob_req->iter                = 0;
    oob_req->reqs[0]             = MPI_REQUEST_NULL;
    oob_req->reqs[1]             = MPI_REQUEST_NULL;
    *req                         = oob_req;
    return UCC_OK;
}


static int mca_coll_ucc_init_ctx(ompi_communicator_t* comm)
{
    mca_coll_ucc_component_t     *cm = &mca_coll_ucc_component;
    char                          str_buf[256];
    ompi_attribute_fn_ptr_union_t del_fn;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ucc_lib_config_h              lib_config;
    ucc_context_config_h          ctx_config;
    ucc_thread_mode_t             tm_requested;
    ucc_lib_params_t              lib_params;
    ucc_context_params_t          ctx_params;
    unsigned                      ucc_api_major, ucc_api_minor, ucc_api_patch;

    ucc_get_version(&ucc_api_major, &ucc_api_minor, &ucc_api_patch);

    tm_requested           = ompi_mpi_thread_multiple ? UCC_THREAD_MULTIPLE :
                                                        UCC_THREAD_SINGLE;
    lib_params.mask        = UCC_LIB_PARAM_FIELD_THREAD_MODE;
    lib_params.thread_mode = tm_requested;

    if (UCC_OK != ucc_lib_config_read("OMPI", NULL, &lib_config)) {
        UCC_ERROR("UCC lib config read failed");
        return OMPI_ERROR;
    }
    if (strlen(cm->cls) > 0) {
        if (UCC_OK != ucc_lib_config_modify(lib_config, "CLS", cm->cls)) {
            ucc_lib_config_release(lib_config);
            UCC_ERROR("failed to modify UCC lib config to set CLS");
            return OMPI_ERROR;
        }
    }

    if (UCC_OK != ucc_init(&lib_params, lib_config, &cm->ucc_lib)) {
        UCC_ERROR("UCC lib init failed");
        ucc_lib_config_release(lib_config);
        cm->ucc_enable = 0;
        return OMPI_ERROR;
    }
    ucc_lib_config_release(lib_config);

    cm->ucc_lib_attr.mask = UCC_LIB_ATTR_FIELD_THREAD_MODE |
                            UCC_LIB_ATTR_FIELD_COLL_TYPES;
    if (UCC_OK != ucc_lib_get_attr(cm->ucc_lib, &cm->ucc_lib_attr)) {
        UCC_ERROR("UCC get lib attr failed");
        goto cleanup_lib;
    }

    if (cm->ucc_lib_attr.thread_mode < tm_requested) {
        UCC_ERROR("UCC library doesn't support MPI_THREAD_MULTIPLE");
        goto cleanup_lib;
    }
    ctx_params.mask             = UCC_CONTEXT_PARAM_FIELD_OOB;
    ctx_params.oob.allgather    = oob_allgather;
    ctx_params.oob.req_test     = oob_allgather_test;
    ctx_params.oob.req_free     = oob_allgather_free;
    ctx_params.oob.coll_info    = (void*)comm;
    ctx_params.oob.n_oob_eps    = ompi_comm_size(comm);
    ctx_params.oob.oob_ep       = ompi_comm_rank(comm);
    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        UCC_ERROR("UCC context config read failed");
        goto cleanup_lib;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", ompi_proc_world_size());
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_EPS",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.num_local_peers + 1);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_PPN",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
    }

    if (ucc_api_major > 1 || (ucc_api_major == 1 && ucc_api_minor >= 6)) {
        snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.my_local_rank);
        if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "NODE_LOCAL_ID",
                                                str_buf)) {
            UCC_ERROR("UCC context config modify failed for node_local_id");
            goto cleanup_lib;
        }
    }

    if (UCC_OK != ucc_context_create(cm->ucc_lib, &ctx_params,
                                     ctx_config, &cm->ucc_context)) {
        UCC_ERROR("UCC context create failed");
        ucc_context_config_release(ctx_config);
        goto cleanup_lib;
    }
    ucc_context_config_release(ctx_config);

    copy_fn.attr_communicator_copy_fn  = MPI_COMM_NULL_COPY_FN;
    del_fn.attr_communicator_delete_fn = ucc_comm_attr_del_fn;
    if (OMPI_SUCCESS != ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn,
                                                &ucc_comm_attr_keyval, NULL ,0, NULL)) {
        UCC_ERROR("UCC comm keyval create failed");
        goto cleanup_ctx;
    }

    OBJ_CONSTRUCT(&cm->requests, opal_free_list_t);
    opal_free_list_init(&cm->requests, sizeof(mca_coll_ucc_req_t),
                        opal_cache_line_size, OBJ_CLASS(mca_coll_ucc_req_t),
                        0, 0,                     /* no payload data */
                        8, -1, 8,                 /* num_to_alloc, max, per alloc */
                        NULL, 0, NULL, NULL, NULL /* no Mpool or init function */);

    opal_progress_register(mca_coll_ucc_progress);
    UCC_VERBOSE(1, "initialized ucc context");
    cm->libucc_initialized = true;
    return OMPI_SUCCESS;
cleanup_ctx:
    ucc_context_destroy(cm->ucc_context);

cleanup_lib:
    ucc_finalize(cm->ucc_lib);
    cm->ucc_enable         = 0;
    cm->libucc_initialized = false;
    return OMPI_ERROR;
}

static uint64_t rank_map_cb(uint64_t ep, void *cb_ctx)
{
    struct ompi_communicator_t *comm = cb_ctx;

    return ((ompi_process_name_t*)&ompi_comm_peer_lookup(comm, ep)->super.
            proc_name)->vpid;
}

static inline ucc_ep_map_t get_rank_map(struct ompi_communicator_t *comm)
{
    ucc_ep_map_t map;
    int64_t      r1, r2, stride;
    uint64_t     i;
    int          is_strided;

    map.ep_num = ompi_comm_size(comm);
    if (comm == &ompi_mpi_comm_world.comm) {
        map.type = UCC_EP_MAP_FULL;
        return map;
    }

    /* try to detect strided pattern */
    is_strided = 1;
    r1         = rank_map_cb(0, comm);
    r2         = rank_map_cb(1, comm);
    stride     = r2 - r1;
    for (i = 2; i < map.ep_num; i++) {
        r1 = r2;
        r2 = rank_map_cb(i, comm);
        if (r2 - r1 != stride) {
            is_strided = 0;
            break;
        }
    }

    if (is_strided) {
        map.type           = UCC_EP_MAP_STRIDED;
        map.strided.start  = r1;
        map.strided.stride = stride;
    } else {
        map.type      = UCC_EP_MAP_CB;
        map.cb.cb     = rank_map_cb;
        map.cb.cb_ctx = (void*)comm;
    }

    return map;
}

#define UCC_INSTALL_COLL_API(__comm, __ucc_module, __COLL, __api)                                                                          \
    do                                                                                                                                     \
    {                                                                                                                                      \
        if ((mca_coll_ucc_component.ucc_lib_attr.coll_types & UCC_COLL_TYPE_##__COLL))                                                     \
        {                                                                                                                                  \
            if (mca_coll_ucc_component.cts_requested & UCC_COLL_TYPE_##__COLL)                                                             \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, __api, (__ucc_module)->previous_##__api, (__ucc_module)->previous_##__api##_module, "ucc");      \
                MCA_COLL_INSTALL_API(__comm, __api, mca_coll_ucc_##__api, &__ucc_module->super, "ucc");                                    \
                (__ucc_module)->super.coll_##__api = mca_coll_ucc_##__api;                                                                 \
            }                                                                                                                              \
            if (mca_coll_ucc_component.nb_cts_requested & UCC_COLL_TYPE_##__COLL)                                                          \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, i##__api, (__ucc_module)->previous_i##__api, (__ucc_module)->previous_i##__api##_module, "ucc"); \
                MCA_COLL_INSTALL_API(__comm, i##__api, mca_coll_ucc_i##__api, &__ucc_module->super, "ucc");                                \
                (__ucc_module)->super.coll_i##__api = mca_coll_ucc_i##__api;                                                               \
            }                                                                                                                              \
            if (mca_coll_ucc_component.ps_cts_requested & UCC_COLL_TYPE_##__COLL)                                                          \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, __api##_init, (__ucc_module)->previous_##__api##_init, (__ucc_module)->previous_##__api##_init_module, "ucc"); \
                MCA_COLL_INSTALL_API(__comm, __api##_init, mca_coll_ucc_##__api##_init, &__ucc_module->super, "ucc");                      \
                (__ucc_module)->super.coll_##__api##_init = mca_coll_ucc_##__api##_init;                                                   \
            }                                                                                                                              \
        }                                                                                                                                  \
    } while (0)

static int mca_coll_ucc_replace_coll_handlers(mca_coll_ucc_module_t *ucc_module)
{
    ompi_communicator_t *comm = ucc_module->comm;

    UCC_INSTALL_COLL_API(comm, ucc_module, ALLREDUCE, allreduce);
    UCC_INSTALL_COLL_API(comm, ucc_module, BARRIER, barrier);
    UCC_INSTALL_COLL_API(comm, ucc_module, BCAST, bcast);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLTOALL, alltoall);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLTOALLV, alltoallv);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLGATHER, allgather);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLGATHERV, allgatherv);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE, reduce);

    UCC_INSTALL_COLL_API(comm, ucc_module, GATHER, gather);
    UCC_INSTALL_COLL_API(comm, ucc_module, GATHERV, gatherv);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE_SCATTER, reduce_scatter_block);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE_SCATTERV, reduce_scatter);
    UCC_INSTALL_COLL_API(comm, ucc_module, SCATTER, scatter);
    UCC_INSTALL_COLL_API(comm, ucc_module, SCATTERV, scatterv);

    return OMPI_SUCCESS;
}

/*
 * Initialize module on the communicator
 */
static int mca_coll_ucc_module_enable(mca_coll_base_module_t *module,
                                      struct ompi_communicator_t *comm)
{
    mca_coll_ucc_component_t *cm         = &mca_coll_ucc_component;
    mca_coll_ucc_module_t    *ucc_module = (mca_coll_ucc_module_t *)module;
    ucc_status_t              status;
    int rc;
    ucc_team_params_t team_params = {
        .mask   = UCC_TEAM_PARAM_FIELD_EP_MAP   |
                  UCC_TEAM_PARAM_FIELD_EP       |
                  UCC_TEAM_PARAM_FIELD_EP_RANGE,
        .ep_map = {
            .type      = (comm == &ompi_mpi_comm_world.comm) ?
                          UCC_EP_MAP_FULL : UCC_EP_MAP_CB,
            .ep_num    = ompi_comm_size(comm),
            .cb.cb     = rank_map_cb,
            .cb.cb_ctx = (void*)comm
        },
        .ep       = ompi_comm_rank(comm),
        .ep_range = UCC_COLLECTIVE_EP_RANGE_CONTIG
    };
    if (OMPI_COMM_IS_GLOBAL_INDEX(comm)) {
	team_params.mask |= UCC_TEAM_PARAM_FIELD_ID;
	team_params.id    = ompi_comm_get_local_cid(comm);
        UCC_VERBOSE(2, "creating ucc_team for comm %p, comm_id %llu, comm_size %d",
                    (void*)comm, (long long unsigned)team_params.id,
                    ompi_comm_size(comm));
    } else {
        UCC_VERBOSE(2, "creating ucc_team for comm %p, comm_id not provided, comm_size %d",
                    (void*)comm, ompi_comm_size(comm));
    }

    if (UCC_OK != ucc_team_create_post(&cm->ucc_context, 1,
                                       &team_params, &ucc_module->ucc_team)) {
        UCC_ERROR("ucc_team_create_post failed");
        goto err;
    }
    while (UCC_INPROGRESS == (status = ucc_team_create_test(
                                  ucc_module->ucc_team))) {
        opal_progress();
    }
    if (UCC_OK != status) {
        UCC_ERROR("ucc_team_create_test failed");
        goto err;
    }

    if (OMPI_SUCCESS != mca_coll_ucc_replace_coll_handlers(ucc_module)) {
        UCC_ERROR("mca_coll_ucc_replace_coll_handlers failed");
        goto err;
    }

    rc = ompi_attr_set_c(COMM_ATTR, comm, &comm->c_keyhash,
                         ucc_comm_attr_keyval, (void *)module, false);
    if (OMPI_SUCCESS != rc) {
        UCC_ERROR("ucc ompi_attr_set_c failed");
        goto err;
    }

    return OMPI_SUCCESS;

err:
    ucc_module->ucc_team = NULL;
    cm->ucc_enable       = 0;
    opal_progress_unregister(mca_coll_ucc_progress);
    return OMPI_ERROR;
}

#define UCC_UNINSTALL_COLL_API(__comm, __ucc_module, __api)                                                                          \
    do                                                                                                                               \
    {                                                                                                                                \
        if (&(__ucc_module)->super == (__comm)->c_coll->coll_##__api##_module)                                                       \
        {                                                                                                                            \
            MCA_COLL_INSTALL_API(__comm, __api, (__ucc_module)->previous_##__api, (__ucc_module)->previous_##__api##_module, "ucc"); \
            (__ucc_module)->previous_##__api = NULL;                                                                                 \
            (__ucc_module)->previous_##__api##_module = NULL;                                                                        \
        }                                                                                                                            \
    } while (0)

/**
 * The disable will be called once per collective module, in the reverse order
 * in which enable has been called. This reverse order allows the module to properly
 * unregister the collective function pointers they provide for the communicator.
 */
static int
mca_coll_ucc_module_disable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allreduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallreduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, barrier);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ibarrier);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, bcast);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ibcast);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoall);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ialltoall);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoallv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ialltoallv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallgather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallgatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, igather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, igatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_block);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce_scatter_block);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce_scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iscatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatterv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iscatterv);

    UCC_UNINSTALL_COLL_API(comm, ucc_module, allreduce_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, barrier_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, bcast_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoall_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoallv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgather_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgatherv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gather_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gatherv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_block_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatter_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatterv_init);

    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_ucc_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    mca_coll_ucc_module_t    *ucc_module;
    *priority = 0;

    if (!cm->ucc_enable){
        return NULL;
    }

    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < cm->ucc_np
        || ompi_comm_size(comm) < 2){
        return NULL;
    }

    if (!cm->libucc_initialized) {
        if (OMPI_SUCCESS != mca_coll_ucc_init_ctx(comm)) {
            cm->ucc_enable = 0;
            return NULL;
        }
    }

    ucc_module = OBJ_NEW(mca_coll_ucc_module_t);
    if (!ucc_module) {
        cm->ucc_enable = 0;
        return NULL;
    }
    ucc_module->comm                      = comm;
    ucc_module->super.coll_module_enable  = mca_coll_ucc_module_enable;
    ucc_module->super.coll_module_disable = mca_coll_ucc_module_disable;
    *priority                             = cm->ucc_priority;

    return &ucc_module->super;
}


OBJ_CLASS_INSTANCE(mca_coll_ucc_module_t,
                   mca_coll_base_module_t,
                   mca_coll_ucc_module_construct,
                   mca_coll_ucc_module_destruct);

OBJ_CLASS_INSTANCE(mca_coll_ucc_req_t, ompi_request_t,
                   NULL, NULL);

int mca_coll_ucc_req_free(struct ompi_request_t **ompi_req)
{
    {
        mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t *) ompi_req[0];
        if (true == coll_req->super.req_persistent) {
            UCC_VERBOSE(5, "%s free %p", "<coll>_init", coll_req);
            if (NULL != coll_req->ucc_req) {
                ucc_status_t rc_ucc;
                rc_ucc = ucc_collective_finalize(coll_req->ucc_req);
                if (UCC_OK != rc_ucc) {
                    UCC_ERROR("ucc_collective_finalize failed: %s", ucc_status_string(rc_ucc));
                }
            }
        }
    }
    opal_free_list_return (&mca_coll_ucc_component.requests,
                           (opal_free_list_item_t *)(*ompi_req));
    *ompi_req = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}


void mca_coll_ucc_completion(void *data, ucc_status_t status)
{
    mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t*)data;
    if (false == coll_req->super.req_persistent) {
        ucc_collective_finalize(coll_req->ucc_req);
    } else {
        UCC_VERBOSE(5, "%s done %p", "<coll>_init", coll_req);
        assert(!REQUEST_COMPLETE(&coll_req->super));
    }
    ompi_request_complete(&coll_req->super, true);
}

/* req_start() : ompi_request_start_fn_t */
int mca_coll_ucc_req_start(size_t count, struct ompi_request_t **requests)
{
    size_t ii;
    int rc = OMPI_SUCCESS;

    for (ii = 0; ii < count; ++ii) {
        mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t *) requests[ii];
        ucc_status_t rc_ucc;

        if ((NULL == coll_req) || (OMPI_REQUEST_COLL != coll_req->super.req_type)) {
            continue;
        }
        if (true != coll_req->super.req_persistent) {
            coll_req->super.req_status.MPI_ERROR = MPI_ERR_REQUEST;
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERROR;
            }
            continue;
        }
        UCC_VERBOSE(5, "%s post %p", "<coll>_init", coll_req);
        assert(REQUEST_COMPLETE(&coll_req->super));
        assert(OMPI_REQUEST_INACTIVE == coll_req->super.req_state);

        coll_req->super.req_status.MPI_TAG = MPI_ANY_TAG;
        coll_req->super.req_status.MPI_ERROR = OMPI_SUCCESS;
        coll_req->super.req_status._cancelled = 0;
        coll_req->super.req_complete = REQUEST_PENDING;
        coll_req->super.req_state = OMPI_REQUEST_ACTIVE;

        rc_ucc = ucc_collective_post(coll_req->ucc_req);
        if (UCC_OK != rc_ucc) {
            UCC_ERROR("ucc_collective_post failed: %s", ucc_status_string(rc_ucc));
            coll_req->super.req_complete = REQUEST_COMPLETED;
            coll_req->super.req_status.MPI_ERROR = MPI_ERR_INTERN;
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERROR;
            }
            continue;
        }
    }

    return rc;
}
