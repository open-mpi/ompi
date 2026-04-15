/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2025 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
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

static int ucc_self_attr_keyval = MPI_KEYVAL_INVALID;

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
    ucc_module->array_idx                             = -1;
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

/*
 * MPI_COMM_SELF attribute delete callback.
 * Fires at the very start of MPI_Finalize, before any communicator is touched
 * and before the PMIx barrier.  We only *initiate* the COMM_WORLD team destroy
 * here — we must not spin waiting for completion because collective transports
 * (SHARP, NCCL) require all ranks to reach their own destroy call first, and
 * there is no global ordering guarantee at this point.
 *
 * Completion of the destroy and full context teardown is deferred to
 * mca_coll_ucc_module_destruct (COMM_WORLD case), which fires after the PMIx
 * barrier when all ranks are in finalize.  That destructor also sweeps any
 * remaining sub-communicator teams before calling ucc_context_destroy, while
 * COMM_WORLD's group is still valid for the UCP OOB barrier.
 */
static int ucc_self_attr_del_fn(MPI_Comm comm, int keyval,
                                void *attr_val, void *extra)
{
    mca_coll_ucc_component_t *cm = (mca_coll_ucc_component_t *) attr_val;
    ucc_status_t              status;

    if (!cm->libucc_initialized || cm->ucc_comm_world_team == NULL) {
        return OMPI_SUCCESS;
    }
    /* Initiate the COMM_WORLD team destroy.  If it completes immediately
     * (e.g. no SHARP/NCCL), record that.  If UCC_INPROGRESS, leave
     * ucc_comm_world_team non-NULL so mca_coll_ucc_module_destruct can
     * finish it after the PMIx barrier. */
    status = ucc_team_destroy(cm->ucc_comm_world_team);
    if (UCC_OK == status) {
        cm->ucc_comm_world_team = NULL;
    } else if (UCC_INPROGRESS != status) {
        UCC_ERROR("UCC team destroy initiation failed for MPI_COMM_WORLD: %s",
                  ucc_status_string(status));
        cm->ucc_comm_world_team = NULL;
    }
    /* Do NOT call mca_coll_ucc_finalize_ctx() here: the UCC context must
     * remain alive so that sub-communicator team destroys (cid>=3 loop in
     * ompi_comm_finalize) can complete cleanly. */
    return OMPI_SUCCESS;
}

void mca_coll_ucc_finalize_ctx(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    if (!cm->libucc_initialized) {
        return;
    }
    UCC_VERBOSE(1, "finalizing ucc library");
    opal_progress_unregister(mca_coll_ucc_progress);
    ucc_context_destroy(cm->ucc_context);
    ucc_finalize(cm->ucc_lib);
    OBJ_DESTRUCT(&cm->requests);
    cm->libucc_initialized = false;
}

static void mca_coll_ucc_module_destruct(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    if (ucc_module->comm == &ompi_mpi_comm_world.comm) {
        /* Complete the COMM_WORLD team destroy that was initiated in
         * ucc_self_attr_del_fn.  By the time this destructor fires, the PMIx
         * barrier in ompi_mpi_finalize() has already passed, so all ranks are
         * in finalize and collective transports (SHARP, NCCL, ...) can safely
         * complete their group destroy protocols. */
        if (cm->libucc_initialized && cm->ucc_comm_world_team != NULL) {
            ucc_status_t status;
            while (UCC_INPROGRESS ==
                   (status = ucc_team_destroy(cm->ucc_comm_world_team))) {
                opal_progress();
            }
            if (UCC_OK != status) {
                UCC_ERROR("UCC team destroy failed for MPI_COMM_WORLD: %s",
                          ucc_status_string(status));
            }
            cm->ucc_comm_world_team = NULL;
        }
        ucc_module->ucc_team = NULL;

        /* Free the COMM_SELF keyval; the callback has already fired. */
        if (ucc_self_attr_keyval != MPI_KEYVAL_INVALID) {
            if (OMPI_SUCCESS !=
                ompi_attr_free_keyval(COMM_ATTR, &ucc_self_attr_keyval, 0)) {
                UCC_ERROR("ucc ompi_attr_free_keyval for self keyval failed");
            }
            ucc_self_attr_keyval = MPI_KEYVAL_INVALID;
        }

        /* Destroy any remaining sub-communicator UCC teams.  These belong to
         * communicators that the user never freed (leaked comms), which will be
         * cleaned up by ompi_comm_finalize's cid>=3 loop AFTER this destructor
         * returns.  We must destroy their teams NOW because ucc_context_destroy
         * (called below) uses the UCP OOB allgather which requires COMM_WORLD's
         * group to still be valid — and COMM_WORLD's group is freed after this
         * function returns in ompi_comm_destruct. */
        if (cm->libucc_initialized) {
            int n = opal_pointer_array_get_size(&cm->active_modules);
            for (int i = 0; i < n; i++) {
                mca_coll_ucc_module_t *m =
                    (mca_coll_ucc_module_t *)opal_pointer_array_get_item(
                        &cm->active_modules, i);
                if (NULL == m || NULL == m->ucc_team) {
                    continue;
                }
                ucc_status_t status;
                while (UCC_INPROGRESS ==
                       (status = ucc_team_destroy(m->ucc_team))) {
                    opal_progress();
                }
                if (UCC_OK != status) {
                    UCC_ERROR("UCC team destroy failed for sub-communicator");
                }
                m->ucc_team    = NULL;
                m->array_idx   = -1;
                opal_pointer_array_set_item(&cm->active_modules, i, NULL);
            }
        }

        /* Now safe to destroy the UCC context and library.  COMM_WORLD's
         * group is still valid at this point (ompi_comm_destruct releases it
         * after mca_coll_base_comm_unselect returns), so the UCP OOB barrier
         * inside ucc_context_destroy can complete successfully. */
        mca_coll_ucc_finalize_ctx();
    } else if (ucc_module->ucc_team != NULL) {
        /* Normal path: user-created communicator freed before MPI_Finalize or
         * during the cid>=3 loop.  Remove from the tracking array first so
         * the COMM_WORLD destructor sweep does not double-free. */
        if (ucc_module->array_idx >= 0) {
            opal_pointer_array_set_item(&cm->active_modules,
                                        ucc_module->array_idx, NULL);
            ucc_module->array_idx = -1;
        }
        if (cm->libucc_initialized) {
            ucc_status_t status;
            while (UCC_INPROGRESS ==
                   (status = ucc_team_destroy(ucc_module->ucc_team))) {
                opal_progress();
            }
            if (UCC_OK != status) {
                UCC_ERROR("UCC team destroy failed");
            }
        }
        ucc_module->ucc_team = NULL;
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


static int mca_coll_ucc_init_ctx() {
    mca_coll_ucc_component_t     *cm = &mca_coll_ucc_component;
    char                          str_buf[256];
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

    if (ucc_api_major > 1 || (ucc_api_major == 1 && ucc_api_minor >= 6)) {
        sprintf(str_buf, "%u", opal_process_info.my_local_rank);
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

    OBJ_CONSTRUCT(&cm->requests, opal_free_list_t);
    opal_free_list_init(&cm->requests, sizeof(mca_coll_ucc_req_t),
                        opal_cache_line_size, OBJ_CLASS(mca_coll_ucc_req_t),
                        0, 0,                     /* no payload data */
                        8, -1, 8,                 /* num_to_alloc, max, per alloc */
                        NULL, 0, NULL, NULL, NULL /* no Mpool or init function */);

    opal_progress_register(mca_coll_ucc_progress);
    UCC_VERBOSE(1, "initialized ucc context");
    cm->libucc_initialized   = true;
    cm->ucc_comm_world_team  = NULL;

    /* Register a MPI_COMM_SELF attribute whose del_fn fires at the very start
     * of MPI_Finalize (ompi_mpi_finalize.c), before any communicator is
     * touched.  This guarantees ucc_context_destroy is called while
     * MPI_COMM_WORLD is still alive, even on OMPI 5.x where COMM_WORLD's
     * own user-attribute del callbacks are intentionally skipped. */
    {
        ompi_attribute_fn_ptr_union_t copy_fn, del_fn;
        copy_fn.attr_communicator_copy_fn  = MPI_COMM_NULL_COPY_FN;
        del_fn.attr_communicator_delete_fn = ucc_self_attr_del_fn;
        if (OMPI_SUCCESS != ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn,
                                                    &ucc_self_attr_keyval,
                                                    NULL, 0, NULL)) {
            UCC_ERROR("UCC ompi_attr_create_keyval for COMM_SELF failed");
            goto cleanup_ctx;
        }
        if (OMPI_SUCCESS != ompi_attr_set_c(COMM_ATTR,
                                            &ompi_mpi_comm_self.comm,
                                            &ompi_mpi_comm_self.comm.c_keyhash,
                                            ucc_self_attr_keyval,
                                            (void *)cm, false)) {
            UCC_ERROR("UCC ompi_attr_set_c on MPI_COMM_SELF failed");
            ompi_attr_free_keyval(COMM_ATTR, &ucc_self_attr_keyval, 0);
            ucc_self_attr_keyval = MPI_KEYVAL_INVALID;
            goto cleanup_ctx;
        }
    }
    return OMPI_SUCCESS;
cleanup_ctx:
    opal_progress_unregister(mca_coll_ucc_progress);
    OBJ_DESTRUCT(&cm->requests);
    ucc_context_destroy(cm->ucc_context);
    cm->libucc_initialized = false;
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

    if (comm == &ompi_mpi_comm_world.comm) {
        /* Track the COMM_WORLD team so ucc_self_attr_del_fn can destroy it
         * at MPI_Finalize start. */
        cm->ucc_comm_world_team = ucc_module->ucc_team;
    } else {
        /* Track non-WORLD modules so the COMM_WORLD destructor can sweep
         * any remaining teams before ucc_context_destroy. */
        ucc_module->array_idx = opal_pointer_array_add(&cm->active_modules,
                                                        ucc_module);
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
