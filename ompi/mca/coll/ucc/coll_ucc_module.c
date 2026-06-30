/**
 * Copyright (c) 2021      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2026 NVIDIA Corporation. All rights reserved.
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
#include "ompi/runtime/ompi_rte.h"

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
    ucc_module->ep_map_ranks                          = NULL;
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
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *domain;

    /* Progress every live UCC context (one per OOB domain). */
    OPAL_LIST_FOREACH(domain, &cm->domains, mca_coll_ucc_oob_domain_t) {
        ucc_context_progress(domain->ucc_context);
    }
    return OPAL_SUCCESS;
}

/*
 * Release a reference to an OOB domain.
 *
 * A domain is shared by the communicator that bootstrapped it and every
 * communicator derived from it; each holds one reference.  When the last
 * reference is dropped the UCC context is destroyed, the reference the domain
 * took on its bootstrap communicator is dropped, and -- when the last domain
 * goes away -- the shared UCC library is finalized and the progress callback
 * is unregistered.
 *
 * Context teardown may itself drive the OOB; that is safe because the domain
 * kept its own reference to the bootstrap communicator (see
 * mca_coll_ucc_domain_create), so the OOB communicator is still valid here
 * even if the user has already freed its handle to it.
 */
static void mca_coll_ucc_domain_release(mca_coll_ucc_oob_domain_t *domain)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    if (NULL == domain) {
        return;
    }

    if (0 != --domain->refcount) {
        return;
    }

    opal_list_remove_item(&cm->domains, &domain->super);
    ucc_context_destroy(domain->ucc_context);
    /* Drop the reference the domain held on its bootstrap communicator (only
       taken for non-intrinsic communicators; see mca_coll_ucc_domain_create). */
    if (!OMPI_COMM_IS_INTRINSIC(domain->comm)) {
        OBJ_RELEASE(domain->comm);
    }
    OBJ_RELEASE(domain);

    if (0 == --cm->domain_count) {
        opal_progress_unregister(mca_coll_ucc_progress);
        ucc_finalize(cm->ucc_lib);
        cm->ucc_lib = NULL;
    }
}

static void mca_coll_ucc_module_destruct(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    /* The per-communicator keyval is a process-global resource shared by all
       UCC communicators.  Free it once there are no live OOB domains left.
       This runs in the module destructor (not in the attribute delete
       callback below) on purpose: freeing a keyval while we are inside one of
       its own delete callbacks -- i.e. while ompi_attr_delete_all() is still
       iterating that comm's attributes -- is unsafe.  The destructor runs
       after the comm's attributes have already been deleted, with the
       attribute subsystem still alive, which matches the proven-safe timing
       of the original code.  If a new UCC communicator appears later the
       keyval is simply recreated lazily by mca_coll_ucc_lib_init(). */
    if (cm->keyval_created && 0 == cm->domain_count) {
        if (OMPI_SUCCESS != ompi_attr_free_keyval(COMM_ATTR, &ucc_comm_attr_keyval, 0)) {
            UCC_ERROR("ucc ompi_attr_free_keyval failed");
        }
        cm->keyval_created = false;
    }
    /* The team (the only user of the ep_map backing array) has already been
       destroyed by the attribute delete callback at this point, so it is safe
       to release the array now. */
    free(ucc_module->ep_map_ranks);
    mca_coll_ucc_module_clear(ucc_module);
}

/*
** Communicator free callback.
**
** Registered through an MPI attribute keyval and invoked by
** ompi_attr_delete_all() while the communicator is being freed -- i.e.
** synchronously inside the *collective* MPI_Comm_free / MPI_Comm_disconnect
** path, before the communicator object is destructed.  Because the
** communicator is freed collectively, every rank reaches this callback for
** the same communicator, so team teardown (a collective over exactly this
** communicator's ranks) and the domain refcount drop happen consistently on
** all ranks.  Doing the teardown here, while the attribute subsystem is still
** alive, also matches the proven-safe timing of the original code.
*/
static int ucc_comm_attr_del_fn(MPI_Comm comm, int keyval, void *attr_val, void *extra)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*) attr_val;
    ucc_status_t           status     = UCC_OK;

    /* Tear down this communicator's UCC team.  Team destroy is collective
       over the team's own ranks (this communicator), not over the domain's
       OOB, so it is safe even when this communicator is a subset of the
       domain's bootstrap communicator. */
    if (NULL != ucc_module->ucc_team) {
        while (UCC_INPROGRESS == (status = ucc_team_destroy(ucc_module->ucc_team))) {
            opal_progress();
        }
        if (UCC_OK != status) {
            UCC_ERROR("UCC team destroy failed");
        }
        ucc_module->ucc_team = NULL;
    }

    /* Drop this communicator's reference to the shared OOB domain; the last
       reference destroys the context and releases the bootstrap comm. */
    mca_coll_ucc_domain_release(ucc_module->domain);
    ucc_module->domain = NULL;

    return (UCC_OK == status) ? OMPI_SUCCESS : OMPI_ERROR;
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
    oob_allgather_req_t       *oob_req = (oob_allgather_req_t*)req;
    /* The OOB context is the domain (see mca_coll_ucc_domain_create); its
       bootstrap communicator backs the point-to-point ring. */
    mca_coll_ucc_oob_domain_t *domain  = (mca_coll_ucc_oob_domain_t *)oob_req->oob_coll_ctx;
    ompi_communicator_t       *comm    = domain->comm;
    char                *tmpsend = NULL;
    char                *tmprecv = NULL;
    size_t               msglen  = oob_req->msglen;
    int                  probe_count = 5;
    int rank, size, sendto, recvfrom, recvdatafrom,
        senddatafrom, completed, probe, rc;

    /* The OOB only runs while bootstrapping or tearing down the context, and
       always over the full bootstrap communicator.  The domain holds its own
       reference to that communicator for its whole lifetime, so it is valid
       here even if the user already freed its handle to it.  A NULL comm
       would mean that invariant was violated -- fail loudly rather than
       dereference a freed communicator. */
    if (NULL == comm) {
        UCC_ERROR("UCC OOB invoked after its bootstrap communicator was freed");
        ompi_rte_abort(1, "coll/ucc: OOB used after bootstrap communicator was freed");
    }

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


/*
 * One-time initialization of the shared UCC library, the request free list
 * and the per-communicator attribute keyval.  Called when the first OOB
 * domain is created; the resources persist across domain churn and are torn
 * down when the last domain is released (library) / at module destruct
 * (keyval) / at component close (request free list).
 */
static int mca_coll_ucc_lib_init(void)
{
    mca_coll_ucc_component_t     *cm = &mca_coll_ucc_component;
    ompi_attribute_fn_ptr_union_t del_fn;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ucc_lib_config_h              lib_config;
    ucc_thread_mode_t             tm_requested;
    ucc_lib_params_t              lib_params;

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

    if (!cm->requests_initialized) {
        OBJ_CONSTRUCT(&cm->requests, opal_free_list_t);
        opal_free_list_init(&cm->requests, sizeof(mca_coll_ucc_req_t),
                            opal_cache_line_size, OBJ_CLASS(mca_coll_ucc_req_t),
                            0, 0,                     /* no payload data */
                            8, -1, 8,                 /* num_to_alloc, max, per alloc */
                            NULL, 0, NULL, NULL, NULL /* no Mpool or init function */);
        cm->requests_initialized = true;
    }

    if (!cm->keyval_created) {
        copy_fn.attr_communicator_copy_fn  = MPI_COMM_NULL_COPY_FN;
        del_fn.attr_communicator_delete_fn = ucc_comm_attr_del_fn;
        if (OMPI_SUCCESS != ompi_attr_create_keyval(COMM_ATTR, copy_fn, del_fn,
                                                    &ucc_comm_attr_keyval, NULL, 0, NULL)) {
            UCC_ERROR("UCC comm keyval create failed");
            goto cleanup_lib;
        }
        cm->keyval_created = true;
    }

    UCC_VERBOSE(1, "initialized ucc library");
    return OMPI_SUCCESS;

cleanup_lib:
    ucc_finalize(cm->ucc_lib);
    cm->ucc_lib = NULL;
    return OMPI_ERROR;
}

/*
 * Create a new OOB domain (and its UCC context) bootstrapped over @comm.
 * The OOB is bound to the domain (coll_info = domain), and the domain takes
 * its own reference to @comm so the OOB communicator stays valid for the
 * whole life of the context even if the user frees its handle to @comm.
 */
static int mca_coll_ucc_domain_create(ompi_communicator_t *comm,
                                      mca_coll_ucc_oob_domain_t **domain_out)
{
    mca_coll_ucc_component_t   *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t  *domain = NULL;
    ucc_context_config_h        ctx_config;
    ucc_context_params_t        ctx_params;
    char                        str_buf[256];
    unsigned                    ucc_api_major, ucc_api_minor, ucc_api_patch;
    bool                        first = (0 == cm->domain_count);

    ucc_get_version(&ucc_api_major, &ucc_api_minor, &ucc_api_patch);

    /* The shared UCC library is created together with the first domain. */
    if (first) {
        if (OMPI_SUCCESS != mca_coll_ucc_lib_init()) {
            return OMPI_ERROR;
        }
    }

    domain = OBJ_NEW(mca_coll_ucc_oob_domain_t);
    if (NULL == domain) {
        goto cleanup_lib;
    }
    /* The context can outlive the user's handle to the bootstrap comm, so the
       domain keeps its own reference to it for the OOB (released in
       mca_coll_ucc_domain_release once the context is destroyed).  Intrinsic
       communicators (MPI_COMM_WORLD) are an exception: they live until
       MPI_Finalize and are torn down with OBJ_DESTRUCT, during which this
       domain is released -- taking/dropping a reference on a communicator
       while it is being destructed is both unnecessary (it is never freed
       early) and unsafe, so we simply borrow it. */
    if (!OMPI_COMM_IS_INTRINSIC(comm)) {
        OBJ_RETAIN(comm);
    }
    domain->comm     = comm;
    domain->refcount = 1;

    ctx_params.mask          = UCC_CONTEXT_PARAM_FIELD_OOB;
    ctx_params.oob.allgather = oob_allgather;
    ctx_params.oob.req_test  = oob_allgather_test;
    ctx_params.oob.req_free  = oob_allgather_free;
    /* coll_info is the domain, not the comm: the OOB callbacks reach the
       retained bootstrap communicator through domain->comm. */
    ctx_params.oob.coll_info = (void*)domain;
    ctx_params.oob.n_oob_eps = ompi_comm_size(comm);
    ctx_params.oob.oob_ep    = ompi_comm_rank(comm);

    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        UCC_ERROR("UCC context config read failed");
        goto cleanup_domain;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", (unsigned)ompi_comm_size(comm));
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_EPS",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_config;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.num_local_peers + 1);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_PPN",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_ppn");
        goto cleanup_config;
    }

    if (ucc_api_major > 1 || (ucc_api_major == 1 && ucc_api_minor >= 6)) {
        snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.my_local_rank);
        if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "NODE_LOCAL_ID",
                                                str_buf)) {
            UCC_ERROR("UCC context config modify failed for node_local_id");
            goto cleanup_config;
        }
    }

    if (UCC_OK != ucc_context_create(cm->ucc_lib, &ctx_params, ctx_config,
                                     &domain->ucc_context)) {
        UCC_ERROR("UCC context create failed");
        goto cleanup_config;
    }
    ucc_context_config_release(ctx_config);

    opal_list_append(&cm->domains, &domain->super);
    if (first) {
        opal_progress_register(mca_coll_ucc_progress);
    }
    cm->domain_count++;

    UCC_VERBOSE(1, "created ucc oob domain %p for comm %p (size %d)",
                (void*)domain, (void*)comm, ompi_comm_size(comm));
    *domain_out = domain;
    return OMPI_SUCCESS;

cleanup_config:
    ucc_context_config_release(ctx_config);
cleanup_domain:
    /* Undo the bootstrap-comm reference taken above (non-intrinsic only). */
    if (!OMPI_COMM_IS_INTRINSIC(domain->comm)) {
        OBJ_RELEASE(domain->comm);
    }
    OBJ_RELEASE(domain);
cleanup_lib:
    /* Only undo the library init if this call created it. */
    if (first) {
        ucc_finalize(cm->ucc_lib);
        cm->ucc_lib = NULL;
    }
    return OMPI_ERROR;
}

/* UCC team ep_map backing-array callback: translate a team endpoint (a rank in
   the team's communicator) into a UCC context endpoint by indexing the
   precomputed array of bootstrap-communicator ranks. */
static uint64_t ep_map_array_cb(uint64_t ep, void *cb_ctx)
{
    return (uint64_t)((const int *)cb_ctx)[ep];
}

/*
 * Build the UCC team ep_map for @comm relative to the domain's bootstrap
 * communicator @boot_comm.
 *
 * A UCC context numbers its endpoints by the OOB ep index used when the
 * context was created, which is the process's rank in the bootstrap
 * communicator (see mca_coll_ucc_domain_create: oob_ep = rank in boot_comm).
 * A team must therefore address its members by their bootstrap-communicator
 * rank, not by any global identifier.  Mapping team rank r -> rank of that
 * same process in boot_comm is exactly what ompi_group_translate_ranks gives
 * us.
 *
 * Using the global vpid (as earlier revisions did) happens to work only when
 * the context was bootstrapped over MPI_COMM_WORLD, where vpid == boot_comm
 * rank == context endpoint.  Over a subset bootstrap communicator -- e.g. an
 * MPI Sessions "half" created with MPI_Comm_create_from_group -- the vpid is
 * out of range for the context's endpoint table and the transport crashes
 * (segfault in ucp_tag_send deep under ucc_team_create_test).
 *
 * If the resulting map needs a backing array (an arbitrary, non-strided
 * permutation) it is returned in *array_out, which the caller must keep alive
 * for the team's lifetime and free afterwards; otherwise *array_out is NULL.
 */
static ucc_ep_map_t get_rank_map(struct ompi_communicator_t *comm,
                                 struct ompi_communicator_t *boot_comm,
                                 int **array_out)
{
    ucc_ep_map_t map;
    int          size = ompi_comm_size(comm);
    int         *ranks, *boot_ranks;
    int          i, stride, is_strided;

    *array_out = NULL;
    map.ep_num = size;

    /* The communicator that bootstrapped the context maps onto it identically. */
    if (comm == boot_comm) {
        map.type = UCC_EP_MAP_FULL;
        return map;
    }

    ranks      = malloc((size_t)size * sizeof(int));
    boot_ranks = malloc((size_t)size * sizeof(int));
    if ((NULL == ranks) || (NULL == boot_ranks)) {
        free(ranks);
        free(boot_ranks);
        UCC_ERROR("failed to allocate ucc ep map translation arrays");
        map.type = UCC_EP_MAP_FULL;
        return map;
    }
    for (i = 0; i < size; i++) {
        ranks[i] = i;
    }
    /* team rank i -> its rank in the bootstrap communicator (context endpoint) */
    ompi_group_translate_ranks(comm->c_local_group, size, ranks,
                               boot_comm->c_local_group, boot_ranks);
    free(ranks);

    /* Detect a strided pattern (covers contiguous halves, reversed orders,
       etc.) so we can avoid keeping a backing array around. */
    stride     = (size > 1) ? (boot_ranks[1] - boot_ranks[0]) : 1;
    is_strided = 1;
    for (i = 2; i < size; i++) {
        if (boot_ranks[i] - boot_ranks[i - 1] != stride) {
            is_strided = 0;
            break;
        }
    }

    if (is_strided) {
        map.type           = UCC_EP_MAP_STRIDED;
        map.strided.start  = (uint64_t)boot_ranks[0];
        map.strided.stride = (int64_t)stride;
        free(boot_ranks);
        return map;
    }

    /* Arbitrary permutation: address the context through the translation array
       (kept alive by the caller via *array_out). */
    map.type      = UCC_EP_MAP_CB;
    map.cb.cb     = ep_map_array_cb;
    map.cb.cb_ctx = (void *)boot_ranks;
    *array_out    = boot_ranks;
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
    mca_coll_ucc_component_t  *cm         = &mca_coll_ucc_component;
    mca_coll_ucc_module_t     *ucc_module = (mca_coll_ucc_module_t *)module;
    mca_coll_ucc_oob_domain_t *domain     = ucc_module->domain;
    ucc_status_t               status;
    int rc;
    /* Team create is collective over this communicator and addresses into the
       (possibly shared) context through an ep map; it does not run the OOB, so
       it needs no per-team OOB and works even when this communicator is a
       subset or reordering of the domain's bootstrap communicator.  The ep map
       must translate team ranks into context endpoints, i.e. into ranks of the
       domain's bootstrap communicator (see get_rank_map). */
    ucc_team_params_t team_params;

    team_params.mask     = UCC_TEAM_PARAM_FIELD_EP_MAP |
                           UCC_TEAM_PARAM_FIELD_EP     |
                           UCC_TEAM_PARAM_FIELD_EP_RANGE;
    team_params.ep_map   = get_rank_map(comm, domain->comm,
                                        &ucc_module->ep_map_ranks);
    team_params.ep       = ompi_comm_rank(comm);
    team_params.ep_range = UCC_COLLECTIVE_EP_RANGE_CONTIG;
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

    if (UCC_OK != ucc_team_create_post(&domain->ucc_context, 1,
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
    /* The attribute was never successfully set on this path, so the comm free
       callback will not run for this module: tear down the team (if any) and
       release the OOB domain reference here.  domain_release also unregisters
       progress / finalizes the library if this was the last domain. */
    if (NULL != ucc_module->ucc_team) {
        while (UCC_INPROGRESS == ucc_team_destroy(ucc_module->ucc_team)) {
            opal_progress();
        }
        ucc_module->ucc_team = NULL;
    }
    mca_coll_ucc_domain_release(ucc_module->domain);
    ucc_module->domain   = NULL;
    cm->ucc_enable       = 0;
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
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_module_t     *ucc_module;
    mca_coll_ucc_oob_domain_t *domain = NULL;
    ompi_communicator_t       *parent;
    *priority = 0;

    if (!cm->ucc_enable){
        return NULL;
    }

    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < cm->ucc_np
        || ompi_comm_size(comm) < 2){
        return NULL;
    }

    /*
     * Domain discovery.  The base makes the parent communicator available on
     * comm->c_coll->parent for the duration of selection (NULL for the paths
     * that have no usable parent context: MPI_Comm_create_from_group and
     * MPI_Intercomm_merge).  If the parent has a UCC module with an OOB
     * domain, inherit it: the new communicator is a subset/reordering of its
     * parent, so it is compatible with the parent's context through the ep
     * map, and the whole family shares one heavyweight context.  Otherwise
     * bootstrap a fresh domain over this communicator.
     */
    parent = comm->c_coll->parent;
    if (cm->keyval_created && NULL != parent && parent != comm &&
        NULL != parent->c_keyhash) {
        int                    flag = 0;
        mca_coll_ucc_module_t *parent_module = NULL;
        if (OMPI_SUCCESS == ompi_attr_get_c(parent->c_keyhash, ucc_comm_attr_keyval,
                                            (void **)&parent_module, &flag)
            && flag && NULL != parent_module && NULL != parent_module->domain) {
            domain = parent_module->domain;
            domain->refcount++;
        }
    }

    if (NULL == domain) {
        if (OMPI_SUCCESS != mca_coll_ucc_domain_create(comm, &domain)) {
            cm->ucc_enable = 0;
            return NULL;
        }
    }

    ucc_module = OBJ_NEW(mca_coll_ucc_module_t);
    if (!ucc_module) {
        mca_coll_ucc_domain_release(domain);
        cm->ucc_enable = 0;
        return NULL;
    }
    ucc_module->comm                      = comm;
    ucc_module->domain                    = domain;
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

OBJ_CLASS_INSTANCE(mca_coll_ucc_oob_domain_t, opal_list_item_t,
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
    /* Reset the request to the invalid state (and drop any f2c handle)
       before handing it back to the free list.  Without this the item is
       returned still marked active/inactive, and ompi_request_destruct()
       asserts on it when the free list is torn down at component close. */
    OMPI_REQUEST_FINI(*ompi_req);
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
