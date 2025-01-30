/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022 NVIDIA Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_ucc.h"
#include "coll_ucc_dtypes.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

#define OBJ_RELEASE_IF_NOT_NULL( obj ) if( NULL != (obj) ) OBJ_RELEASE( obj );

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
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_allreduce_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_iallreduce_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_barrier_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ibarrier_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_bcast_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ibcast_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_alltoall_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ialltoall_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_alltoallv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ialltoallv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_allgather_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_iallgather_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_allgatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_iallgatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_reduce_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ireduce_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_gather_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_igather_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_gatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_igatherv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_reduce_scatter_block_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ireduce_scatter_block_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_reduce_scatter_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_ireduce_scatter_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_scatterv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_iscatterv_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_scatter_module);
    OBJ_RELEASE_IF_NOT_NULL(ucc_module->previous_iscatter_module);
    mca_coll_ucc_module_clear(ucc_module);
}

#define SAVE_PREV_COLL_API(__api) do {                                                       \
        ucc_module->previous_ ## __api            = comm->c_coll->coll_ ## __api;            \
        ucc_module->previous_ ## __api ## _module = comm->c_coll->coll_ ## __api ## _module; \
        if (comm->c_coll->coll_ ## __api && comm->c_coll->coll_ ## __api ## _module) {       \
            OBJ_RETAIN(ucc_module->previous_ ## __api ## _module);                           \
        }                                                                                    \
    } while(0)

static void mca_coll_ucc_save_coll_handlers(mca_coll_ucc_module_t *ucc_module)
{
    ompi_communicator_t *comm = ucc_module->comm;
    SAVE_PREV_COLL_API(allreduce);
    SAVE_PREV_COLL_API(iallreduce);
    SAVE_PREV_COLL_API(barrier);
    SAVE_PREV_COLL_API(ibarrier);
    SAVE_PREV_COLL_API(bcast);
    SAVE_PREV_COLL_API(ibcast);
    SAVE_PREV_COLL_API(alltoall);
    SAVE_PREV_COLL_API(ialltoall);
    SAVE_PREV_COLL_API(alltoallv);
    SAVE_PREV_COLL_API(ialltoallv);
    SAVE_PREV_COLL_API(allgather);
    SAVE_PREV_COLL_API(iallgather);
    SAVE_PREV_COLL_API(allgatherv);
    SAVE_PREV_COLL_API(iallgatherv);
    SAVE_PREV_COLL_API(reduce);
    SAVE_PREV_COLL_API(ireduce);
    SAVE_PREV_COLL_API(gather);
    SAVE_PREV_COLL_API(igather);
    SAVE_PREV_COLL_API(gatherv);
    SAVE_PREV_COLL_API(igatherv);
    SAVE_PREV_COLL_API(reduce_scatter_block);
    SAVE_PREV_COLL_API(ireduce_scatter_block);
    SAVE_PREV_COLL_API(reduce_scatter);
    SAVE_PREV_COLL_API(ireduce_scatter);
    SAVE_PREV_COLL_API(scatterv);
    SAVE_PREV_COLL_API(iscatterv);
    SAVE_PREV_COLL_API(scatter);
    SAVE_PREV_COLL_API(iscatter);
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
        senddatafrom, completed, probe;

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
        MCA_PML_CALL(isend(tmpsend, msglen, MPI_BYTE, sendto, MCA_COLL_BASE_TAG_UCC,
                           MCA_PML_BASE_SEND_STANDARD, comm, &oob_req->reqs[0]));
        MCA_PML_CALL(irecv(tmprecv, msglen, MPI_BYTE, recvfrom,
                           MCA_COLL_BASE_TAG_UCC, comm, &oob_req->reqs[1]));
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
    *req                         = oob_req;
    return UCC_OK;
}


static int mca_coll_ucc_init_ctx() {
    mca_coll_ucc_component_t     *cm = &mca_coll_ucc_component;
    char                          str_buf[256];
    ompi_attribute_fn_ptr_union_t del_fn;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ucc_lib_config_h              lib_config;
    ucc_context_config_h          ctx_config;
    ucc_thread_mode_t             tm_requested;
    ucc_lib_params_t              lib_params;
    ucc_context_params_t          ctx_params;

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
    ctx_params.oob.coll_info    = (void*)MPI_COMM_WORLD;
    ctx_params.oob.n_oob_eps    = ompi_comm_size(&ompi_mpi_comm_world.comm);
    ctx_params.oob.oob_ep       = ompi_comm_rank(&ompi_mpi_comm_world.comm);
    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        UCC_ERROR("UCC context config read failed");
        goto cleanup_lib;
    }

    sprintf(str_buf, "%u", ompi_proc_world_size());
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_EPS",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
    }

    sprintf(str_buf, "%u", opal_process_info.num_local_peers + 1);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_PPN",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_lib;
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

uint64_t rank_map_cb(uint64_t ep, void *cb_ctx)
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
                  UCC_TEAM_PARAM_FIELD_EP_RANGE |
                  UCC_TEAM_PARAM_FIELD_ID,
        .ep_map = {
            .type      = (comm == &ompi_mpi_comm_world.comm) ?
                          UCC_EP_MAP_FULL : UCC_EP_MAP_CB,
            .ep_num    = ompi_comm_size(comm),
            .cb.cb     = rank_map_cb,
            .cb.cb_ctx = (void*)comm
        },
        .ep       = ompi_comm_rank(comm),
        .ep_range = UCC_COLLECTIVE_EP_RANGE_CONTIG,
        .id       = ompi_comm_get_local_cid(comm)
    };
    UCC_VERBOSE(2, "creating ucc_team for comm %p, comm_id %llu, comm_size %d",
                (void*)comm, (long long unsigned)team_params.id,
                ompi_comm_size(comm));

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

    rc = ompi_attr_set_c(COMM_ATTR, comm, &comm->c_keyhash,
                         ucc_comm_attr_keyval, (void *)module, false);
    if (OMPI_SUCCESS != rc) {
        UCC_ERROR("ucc ompi_attr_set_c failed");
        goto err;
    }

    mca_coll_ucc_save_coll_handlers(ucc_module);

    return OMPI_SUCCESS;

err:
    ucc_module->ucc_team = NULL;
    cm->ucc_enable       = 0;
    opal_progress_unregister(mca_coll_ucc_progress);
    return OMPI_ERROR;
}


#define SET_COLL_PTR(_module, _COLL, _coll) do {                          \
        _module->super.coll_  ## _coll = NULL;                            \
        _module->super.coll_i ## _coll = NULL;                            \
        if ((mca_coll_ucc_component.ucc_lib_attr.coll_types &             \
             UCC_COLL_TYPE_ ## _COLL)) {                                  \
            if (mca_coll_ucc_component.cts_requested &                    \
                UCC_COLL_TYPE_ ## _COLL) {                                \
                _module->super.coll_ ## _coll  = mca_coll_ucc_  ## _coll; \
            }                                                             \
            if (mca_coll_ucc_component.nb_cts_requested &                 \
                UCC_COLL_TYPE_ ## _COLL) {                                \
                _module->super.coll_i ## _coll = mca_coll_ucc_i ## _coll; \
            }                                                             \
        }                                                                 \
    } while(0)

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
        if (OMPI_SUCCESS != mca_coll_ucc_init_ctx()) {
            cm->ucc_enable = 0;
            return NULL;
        }
    }

    ucc_module = OBJ_NEW(mca_coll_ucc_module_t);
    if (!ucc_module) {
        cm->ucc_enable = 0;
        return NULL;
    }
    ucc_module->comm                     = comm;
    ucc_module->super.coll_module_enable = mca_coll_ucc_module_enable;
    *priority                            = cm->ucc_priority;
    SET_COLL_PTR(ucc_module, BARRIER,         barrier);
    SET_COLL_PTR(ucc_module, BCAST,           bcast);
    SET_COLL_PTR(ucc_module, ALLREDUCE,       allreduce);
    SET_COLL_PTR(ucc_module, ALLTOALL,        alltoall);
    SET_COLL_PTR(ucc_module, ALLTOALLV,       alltoallv);
    SET_COLL_PTR(ucc_module, REDUCE,          reduce);
    SET_COLL_PTR(ucc_module, ALLGATHER,       allgather);
    SET_COLL_PTR(ucc_module, ALLGATHERV,      allgatherv);
    SET_COLL_PTR(ucc_module, GATHER,          gather);
    SET_COLL_PTR(ucc_module, GATHERV,         gatherv);
    SET_COLL_PTR(ucc_module, REDUCE_SCATTER,  reduce_scatter_block);
    SET_COLL_PTR(ucc_module, REDUCE_SCATTERV, reduce_scatter);
    SET_COLL_PTR(ucc_module, SCATTERV,        scatterv);
    SET_COLL_PTR(ucc_module, SCATTER,         scatter);
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
    opal_free_list_return (&mca_coll_ucc_component.requests,
                           (opal_free_list_item_t *)(*ompi_req));
    *ompi_req = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}


void mca_coll_ucc_completion(void *data, ucc_status_t status)
{
    mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t*)data;
    ucc_collective_finalize(coll_req->ucc_req);
    ompi_request_complete(&coll_req->super, true);
}
