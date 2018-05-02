/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"

#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix.h"
#include "ompi/message/message.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "pml_ucx_request.h"

#include <inttypes.h>


#define PML_UCX_TRACE_SEND(_msg, _buf, _count, _datatype, _dst, _tag, _mode, _comm, ...) \
    PML_UCX_VERBOSE(8, _msg " buf %p count %zu type '%s' dst %d tag %d mode %s comm %d '%s'", \
                    __VA_ARGS__, \
                    (_buf), (_count), (_datatype)->name, (_dst), (_tag), \
                    mca_pml_ucx_send_mode_name(_mode), (_comm)->c_contextid, \
                    (_comm)->c_name);

#define PML_UCX_TRACE_RECV(_msg, _buf, _count, _datatype, _src, _tag, _comm, ...) \
    PML_UCX_VERBOSE(8, _msg " buf %p count %zu type '%s' src %d tag %d comm %d '%s'", \
                    __VA_ARGS__, \
                    (_buf), (_count), (_datatype)->name, (_src), (_tag), \
                    (_comm)->c_contextid, (_comm)->c_name);

#define PML_UCX_TRACE_PROBE(_msg, _src, _tag, _comm) \
    PML_UCX_VERBOSE(8, _msg " src %d tag %d comm %d '%s'", \
                    _src, (_tag), (_comm)->c_contextid, (_comm)->c_name);

#define PML_UCX_TRACE_MRECV(_msg, _buf, _count, _datatype, _message) \
    PML_UCX_VERBOSE(8, _msg " buf %p count %zu type '%s' msg *%p=%p (%p)", \
                    (_buf), (_count), (_datatype)->name, (void*)(_message), \
                    (void*)*(_message), (*(_message))->req_ptr);

#define MODEX_KEY "pml-ucx"

mca_pml_ucx_module_t ompi_pml_ucx = {
    {
        mca_pml_ucx_add_procs,
        mca_pml_ucx_del_procs,
        mca_pml_ucx_enable,
        NULL,
        mca_pml_ucx_add_comm,
        mca_pml_ucx_del_comm,
        mca_pml_ucx_irecv_init,
        mca_pml_ucx_irecv,
        mca_pml_ucx_recv,
        mca_pml_ucx_isend_init,
        mca_pml_ucx_isend,
        mca_pml_ucx_send,
        mca_pml_ucx_iprobe,
        mca_pml_ucx_probe,
        mca_pml_ucx_start,
        mca_pml_ucx_improbe,
        mca_pml_ucx_mprobe,
        mca_pml_ucx_imrecv,
        mca_pml_ucx_mrecv,
        mca_pml_ucx_dump,
        NULL, /* FT */
        1ul << (PML_UCX_CONTEXT_BITS),
        1ul << (PML_UCX_TAG_BITS - 1),
    },
    NULL,   /* ucp_context */
    NULL    /* ucp_worker */
};

#define PML_UCX_REQ_ALLOCA() \
    ((char *)alloca(ompi_pml_ucx.request_size) + ompi_pml_ucx.request_size);


static int mca_pml_ucx_send_worker_address(void)
{
    ucp_address_t *address;
    ucs_status_t status;
    size_t addrlen;
    int rc;

    status = ucp_worker_get_address(ompi_pml_ucx.ucp_worker, &address, &addrlen);
    if (UCS_OK != status) {
        PML_UCX_ERROR("Failed to get worker address");
        return OMPI_ERROR;
    }

    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL,
                    &mca_pml_ucx_component.pmlm_version, (void*)address, addrlen);
    if (OMPI_SUCCESS != rc) {
        PML_UCX_ERROR("Open MPI couldn't distribute EP connection details");
        return OMPI_ERROR;
    }

    ucp_worker_release_address(ompi_pml_ucx.ucp_worker, address);

    return OMPI_SUCCESS;
}

static int mca_pml_ucx_recv_worker_address(ompi_proc_t *proc,
                                           ucp_address_t **address_p,
                                           size_t *addrlen_p)
{
    int ret;

    *address_p = NULL;
    OPAL_MODEX_RECV(ret, &mca_pml_ucx_component.pmlm_version, &proc->super.proc_name,
                    (void**)address_p, addrlen_p);
    if (ret < 0) {
        PML_UCX_ERROR("Failed to receive UCX worker address: %s (%d)",
                      opal_strerror(ret), ret);
    }
    return ret;
}

int mca_pml_ucx_open(void)
{
    ucp_context_attr_t attr;
    ucp_params_t params;
    ucp_config_t *config;
    ucs_status_t status;

    PML_UCX_VERBOSE(1, "mca_pml_ucx_open");

    /* Read options */
    status = ucp_config_read("MPI", NULL, &config);
    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

    /* Initialize UCX context */
    params.field_mask      = UCP_PARAM_FIELD_FEATURES |
                             UCP_PARAM_FIELD_REQUEST_SIZE |
                             UCP_PARAM_FIELD_REQUEST_INIT |
                             UCP_PARAM_FIELD_REQUEST_CLEANUP |
                             UCP_PARAM_FIELD_TAG_SENDER_MASK |
                             UCP_PARAM_FIELD_MT_WORKERS_SHARED |
                             UCP_PARAM_FIELD_ESTIMATED_NUM_EPS;
    params.features        = UCP_FEATURE_TAG;
    params.request_size    = sizeof(ompi_request_t);
    params.request_init    = mca_pml_ucx_request_init;
    params.request_cleanup = mca_pml_ucx_request_cleanup;
    params.tag_sender_mask = PML_UCX_SPECIFIC_SOURCE_MASK;
    params.mt_workers_shared = 0; /* we do not need mt support for context
                                     since it will be protected by worker */
    params.estimated_num_eps = ompi_proc_world_size();

    status = ucp_init(&params, config, &ompi_pml_ucx.ucp_context);
    ucp_config_release(config);

    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

    /* Query UCX attributes */
    attr.field_mask        = UCP_ATTR_FIELD_REQUEST_SIZE;
    status = ucp_context_query(ompi_pml_ucx.ucp_context, &attr);
    if (UCS_OK != status) {
        ucp_cleanup(ompi_pml_ucx.ucp_context);
        ompi_pml_ucx.ucp_context = NULL;
        return OMPI_ERROR;
    }

    ompi_pml_ucx.request_size = attr.request_size;

    return OMPI_SUCCESS;
}

int mca_pml_ucx_close(void)
{
    PML_UCX_VERBOSE(1, "mca_pml_ucx_close");

    if (ompi_pml_ucx.ucp_context != NULL) {
        ucp_cleanup(ompi_pml_ucx.ucp_context);
        ompi_pml_ucx.ucp_context = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_pml_ucx_init(void)
{
    ucp_worker_params_t params;
    ucs_status_t status;
    ucp_worker_attr_t attr;
    int rc;

    PML_UCX_VERBOSE(1, "mca_pml_ucx_init");

    /* TODO check MPI thread mode */
    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    params.thread_mode = UCS_THREAD_MODE_SINGLE;
    if (ompi_mpi_thread_multiple) {
        params.thread_mode = UCS_THREAD_MODE_MULTI;
    } else {
        params.thread_mode = UCS_THREAD_MODE_SINGLE;
    }

    status = ucp_worker_create(ompi_pml_ucx.ucp_context, &params,
                               &ompi_pml_ucx.ucp_worker);
    if (UCS_OK != status) {
        PML_UCX_ERROR("Failed to create UCP worker");
        return OMPI_ERROR;
    }

    attr.field_mask = UCP_WORKER_ATTR_FIELD_THREAD_MODE;
    status = ucp_worker_query(ompi_pml_ucx.ucp_worker, &attr);
    if (UCS_OK != status) {
        ucp_worker_destroy(ompi_pml_ucx.ucp_worker);
        ompi_pml_ucx.ucp_worker = NULL;
        PML_UCX_ERROR("Failed to query UCP worker thread level");
        return OMPI_ERROR;
    }

    if (ompi_mpi_thread_multiple && attr.thread_mode != UCS_THREAD_MODE_MULTI) {
        /* UCX does not support multithreading, disqualify current PML for now */
        /* TODO: we should let OMPI to fallback to THREAD_SINGLE mode */
        ucp_worker_destroy(ompi_pml_ucx.ucp_worker);
        ompi_pml_ucx.ucp_worker = NULL;
        PML_UCX_ERROR("UCP worker does not support MPI_THREAD_MULTIPLE");
        return OMPI_ERROR;
    }

    rc = mca_pml_ucx_send_worker_address();
    if (rc < 0) {
        return rc;
    }

    /* Initialize the free lists */
    OBJ_CONSTRUCT(&ompi_pml_ucx.persistent_reqs, mca_pml_ucx_freelist_t);
    OBJ_CONSTRUCT(&ompi_pml_ucx.convs,           mca_pml_ucx_freelist_t);

    /* Create a completed request to be returned from isend */
    OBJ_CONSTRUCT(&ompi_pml_ucx.completed_send_req, ompi_request_t);
    mca_pml_ucx_completed_request_init(&ompi_pml_ucx.completed_send_req);

    opal_progress_register(mca_pml_ucx_progress);

    PML_UCX_VERBOSE(2, "created ucp context %p, worker %p",
                    (void *)ompi_pml_ucx.ucp_context,
                    (void *)ompi_pml_ucx.ucp_worker);
    return OMPI_SUCCESS;
}

int mca_pml_ucx_cleanup(void)
{
    PML_UCX_VERBOSE(1, "mca_pml_ucx_cleanup");

    opal_progress_unregister(mca_pml_ucx_progress);

    ompi_pml_ucx.completed_send_req.req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(&ompi_pml_ucx.completed_send_req);
    OBJ_DESTRUCT(&ompi_pml_ucx.completed_send_req);

    OBJ_DESTRUCT(&ompi_pml_ucx.convs);
    OBJ_DESTRUCT(&ompi_pml_ucx.persistent_reqs);

    if (ompi_pml_ucx.ucp_worker) {
        ucp_worker_destroy(ompi_pml_ucx.ucp_worker);
        ompi_pml_ucx.ucp_worker = NULL;
    }

    return OMPI_SUCCESS;
}

static ucp_ep_h mca_pml_ucx_add_proc_common(ompi_proc_t *proc)
{
    ucp_ep_params_t ep_params;
    ucp_address_t *address;
    ucs_status_t status;
    size_t addrlen;
    ucp_ep_h ep;
    int ret;

    ret = mca_pml_ucx_recv_worker_address(proc, &address, &addrlen);
    if (ret < 0) {
        return NULL;
    }

    PML_UCX_VERBOSE(2, "connecting to proc. %d", proc->super.proc_name.vpid);

    ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
    ep_params.address    = address;

    status = ucp_ep_create(ompi_pml_ucx.ucp_worker, &ep_params, &ep);
    free(address);
    if (UCS_OK != status) {
        PML_UCX_ERROR("ucp_ep_create(proc=%d) failed: %s",
                      proc->super.proc_name.vpid,
                      ucs_status_string(status));
        return NULL;
    }

    proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = ep;
    return ep;
}

static ucp_ep_h mca_pml_ucx_add_proc(ompi_communicator_t *comm, int dst)
{
    ompi_proc_t *proc0     = ompi_comm_peer_lookup(comm, 0);
    ompi_proc_t *proc_peer = ompi_comm_peer_lookup(comm, dst);
    int ret;

    /* Note, mca_pml_base_pml_check_selected, doesn't use 3rd argument */
    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("ucx",
                                                               &proc0,
                                                               dst))) {
        return NULL;
    }

    return mca_pml_ucx_add_proc_common(proc_peer);
}

int mca_pml_ucx_add_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ompi_proc_t *proc;
    ucp_ep_h ep;
    size_t i;
    int ret;

    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("ucx",
                                                               procs,
                                                               nprocs))) {
        return ret;
    }

    for (i = 0; i < nprocs; ++i) {
        proc = procs[(i + OMPI_PROC_MY_NAME->vpid) % nprocs];
        ep = mca_pml_ucx_add_proc_common(proc);
        if (ep == NULL) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

static inline ucp_ep_h mca_pml_ucx_get_ep(ompi_communicator_t *comm, int rank)
{
    ucp_ep_h ep;

    ep = ompi_comm_peer_lookup(comm, rank)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OPAL_LIKELY(ep != NULL)) {
        return ep;
    }

    ep = mca_pml_ucx_add_proc(comm, rank);
    if (OPAL_LIKELY(ep != NULL)) {
        return ep;
    }

    if (rank >= ompi_comm_size(comm)) {
        PML_UCX_ERROR("Rank number (%d) is larger than communicator size (%d)",
                      rank, ompi_comm_size(comm));
    } else {
        PML_UCX_ERROR("Failed to resolve UCX endpoint for rank %d", rank);
    }

    return NULL;
}

static void mca_pml_ucx_waitall(void **reqs, size_t *count_p)
{
    ucs_status_t status;
    size_t i;

    PML_UCX_VERBOSE(2, "waiting for %d disconnect requests", (int)*count_p);
    for (i = 0; i < *count_p; ++i) {
        do {
            opal_progress();
            status = ucp_request_test(reqs[i], NULL);
        } while (status == UCS_INPROGRESS);
        if (status != UCS_OK) {
            PML_UCX_ERROR("disconnect request failed: %s",
                          ucs_status_string(status));
        }
        ucp_request_free(reqs[i]);
        reqs[i] = NULL;
    }

    *count_p = 0;
}

int mca_pml_ucx_del_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ompi_proc_t *proc;
    size_t num_reqs, max_reqs;
    void *dreq, **dreqs;
    ucp_ep_h ep;
    size_t i;

    max_reqs = ompi_pml_ucx.num_disconnect;
    if (max_reqs > nprocs) {
        max_reqs = nprocs;
    }

    dreqs = malloc(sizeof(*dreqs) * max_reqs);
    if (dreqs == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    num_reqs = 0;

    for (i = 0; i < nprocs; ++i) {
        proc = procs[(i + OMPI_PROC_MY_NAME->vpid) % nprocs];
        ep   = proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        if (ep == NULL) {
            continue;
        }

        PML_UCX_VERBOSE(2, "disconnecting from rank %d", proc->super.proc_name.vpid);
        dreq = ucp_disconnect_nb(ep);
        if (dreq != NULL) {
            if (UCS_PTR_IS_ERR(dreq)) {
                PML_UCX_ERROR("ucp_disconnect_nb(%d) failed: %s",
                              proc->super.proc_name.vpid,
                              ucs_status_string(UCS_PTR_STATUS(dreq)));
            } else {
                dreqs[num_reqs++] = dreq;
            }
        }

        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = NULL;

        if ((int)num_reqs >= ompi_pml_ucx.num_disconnect) {
            mca_pml_ucx_waitall(dreqs, &num_reqs);
        }
    }

    mca_pml_ucx_waitall(dreqs, &num_reqs);
    free(dreqs);

    opal_pmix.fence(NULL, 0);

    return OMPI_SUCCESS;
}

int mca_pml_ucx_enable(bool enable)
{
    PML_UCX_FREELIST_INIT(&ompi_pml_ucx.persistent_reqs,
                          mca_pml_ucx_persistent_request_t,
                          128, -1, 128);
    PML_UCX_FREELIST_INIT(&ompi_pml_ucx.convs,
                          mca_pml_ucx_convertor_t,
                          128, -1, 128);
    return OMPI_SUCCESS;
}

int mca_pml_ucx_progress(void)
{
    ucp_worker_progress(ompi_pml_ucx.ucp_worker);
    return OMPI_SUCCESS;
}

int mca_pml_ucx_add_comm(struct ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_ucx_del_comm(struct ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_ucx_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int src, int tag, struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{
    mca_pml_ucx_persistent_request_t *req;

    req = (mca_pml_ucx_persistent_request_t *)PML_UCX_FREELIST_GET(&ompi_pml_ucx.persistent_reqs);
    if (req == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    PML_UCX_TRACE_RECV("irecv_init request *%p=%p", buf, count, datatype, src,
                       tag, comm, (void*)request, (void*)req);

    req->ompi.req_state = OMPI_REQUEST_INACTIVE;
    req->flags          = 0;
    req->buffer         = buf;
    req->count          = count;
    req->datatype       = mca_pml_ucx_get_datatype(datatype);

    PML_UCX_MAKE_RECV_TAG(req->tag, req->recv.tag_mask, tag, src, comm);

    *request = &req->ompi;
    return OMPI_SUCCESS;
}

int mca_pml_ucx_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                      int src, int tag, struct ompi_communicator_t* comm,
                      struct ompi_request_t **request)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ompi_request_t *req;

    PML_UCX_TRACE_RECV("irecv request *%p", buf, count, datatype, src, tag, comm,
                       (void*)request);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    req = (ompi_request_t*)ucp_tag_recv_nb(ompi_pml_ucx.ucp_worker, buf, count,
                                           mca_pml_ucx_get_datatype(datatype),
                                           ucp_tag, ucp_tag_mask,
                                           mca_pml_ucx_recv_completion);
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    PML_UCX_VERBOSE(8, "got request %p", (void*)req);
    *request = req;
    return OMPI_SUCCESS;
}

int mca_pml_ucx_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                     int tag, struct ompi_communicator_t* comm,
                     ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucs_status_t status;
    void *req;

    PML_UCX_TRACE_RECV("%s", buf, count, datatype, src, tag, comm, "recv");

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    req = PML_UCX_REQ_ALLOCA();
    status = ucp_tag_recv_nbr(ompi_pml_ucx.ucp_worker, buf, count,
                              mca_pml_ucx_get_datatype(datatype),
                              ucp_tag, ucp_tag_mask, req);

    for (;;) {
        status = ucp_request_test(req, &info);
        if (status != UCS_INPROGRESS) {
            mca_pml_ucx_set_recv_status_safe(mpi_status, status, &info);
            return OMPI_SUCCESS;
        }
        opal_progress();
    }
}

static inline const char *mca_pml_ucx_send_mode_name(mca_pml_base_send_mode_t mode)
{
    switch (mode) {
    case MCA_PML_BASE_SEND_SYNCHRONOUS:
        return "sync";
    case MCA_PML_BASE_SEND_COMPLETE:
        return "complete";
    case MCA_PML_BASE_SEND_BUFFERED:
        return "buffered";
    case MCA_PML_BASE_SEND_READY:
        return "ready";
    case MCA_PML_BASE_SEND_STANDARD:
        return "standard";
    case MCA_PML_BASE_SEND_SIZE:
        return "size";
    default:
        return "unknown";
    }
}

int mca_pml_ucx_isend_init(const void *buf, size_t count, ompi_datatype_t *datatype,
                           int dst, int tag, mca_pml_base_send_mode_t mode,
                           struct ompi_communicator_t* comm,
                           struct ompi_request_t **request)
{
    mca_pml_ucx_persistent_request_t *req;
    ucp_ep_h ep;

    req = (mca_pml_ucx_persistent_request_t *)PML_UCX_FREELIST_GET(&ompi_pml_ucx.persistent_reqs);
    if (req == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    PML_UCX_TRACE_SEND("isend_init request *%p=%p", buf, count, datatype, dst,
                       tag, mode, comm, (void*)request, (void*)req)

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        return OMPI_ERROR;
    }

    req->ompi.req_state = OMPI_REQUEST_INACTIVE;
    req->flags          = MCA_PML_UCX_REQUEST_FLAG_SEND;
    req->buffer         = (void *)buf;
    req->count          = count;
    req->tag            = PML_UCX_MAKE_SEND_TAG(tag, comm);
    req->send.mode      = mode;
    req->send.ep        = ep;
    if (MCA_PML_BASE_SEND_BUFFERED == mode) {
        req->ompi_datatype = datatype;
        OBJ_RETAIN(datatype);
    } else {
        req->datatype = mca_pml_ucx_get_datatype(datatype);
    }

    *request = &req->ompi;
    return OMPI_SUCCESS;
}

static ucs_status_ptr_t
mca_pml_ucx_bsend(ucp_ep_h ep, const void *buf, size_t count, 
                  ompi_datatype_t *datatype, uint64_t pml_tag)
{
    ompi_request_t *req;
    void *packed_data;
    size_t packed_length;
    size_t offset;
    uint32_t iov_count;
    struct iovec iov;
    opal_convertor_t opal_conv;

    OBJ_CONSTRUCT(&opal_conv, opal_convertor_t);
    opal_convertor_copy_and_prepare_for_send(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buf, 0,
                                             &opal_conv);
    opal_convertor_get_packed_size(&opal_conv, &packed_length);

    packed_data = mca_pml_base_bsend_request_alloc_buf(packed_length);
    if (OPAL_UNLIKELY(NULL == packed_data)) {
        OBJ_DESTRUCT(&opal_conv);
        PML_UCX_ERROR("bsend: failed to allocate buffer");
        return UCS_STATUS_PTR(OMPI_ERROR);
    }
    
    iov_count    = 1;
    iov.iov_base = packed_data;
    iov.iov_len  = packed_length;

    PML_UCX_VERBOSE(8, "bsend of packed buffer %p len %zu", packed_data, packed_length);
    offset = 0;
    opal_convertor_set_position(&opal_conv, &offset);
    if (0 > opal_convertor_pack(&opal_conv, &iov, &iov_count, &packed_length)) {
        mca_pml_base_bsend_request_free(packed_data);
        OBJ_DESTRUCT(&opal_conv);
        PML_UCX_ERROR("bsend: failed to pack user datatype");
        return UCS_STATUS_PTR(OMPI_ERROR);
    }

    OBJ_DESTRUCT(&opal_conv);

    req = (ompi_request_t*)ucp_tag_send_nb(ep, packed_data, packed_length,
                                           ucp_dt_make_contig(1), pml_tag,
                                           mca_pml_ucx_bsend_completion);
    if (NULL == req) {
        /* request was completed in place */
        mca_pml_base_bsend_request_free(packed_data);
        return NULL;
    }

    if (OPAL_UNLIKELY(UCS_PTR_IS_ERR(req))) {
        mca_pml_base_bsend_request_free(packed_data);
        PML_UCX_ERROR("ucx bsend failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return UCS_STATUS_PTR(OMPI_ERROR);
    }

    req->req_complete_cb_data = packed_data;
    return NULL;
}

static inline ucs_status_ptr_t mca_pml_ucx_common_send(ucp_ep_h ep, const void *buf,
                                                       size_t count,
                                                       ompi_datatype_t *datatype,
                                                       ucp_datatype_t ucx_datatype,
                                                       ucp_tag_t tag,
                                                       mca_pml_base_send_mode_t mode,
                                                       ucp_send_callback_t cb)
{
    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == mode)) {
        return mca_pml_ucx_bsend(ep, buf, count, datatype, tag);
    } else if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        return ucp_tag_send_sync_nb(ep, buf, count, ucx_datatype, tag, cb);
    } else {
        return ucp_tag_send_nb(ep, buf, count, ucx_datatype, tag, cb);
    }
}

int mca_pml_ucx_isend(const void *buf, size_t count, ompi_datatype_t *datatype,
                      int dst, int tag, mca_pml_base_send_mode_t mode,
                      struct ompi_communicator_t* comm,
                      struct ompi_request_t **request)
{
    ompi_request_t *req;
    ucp_ep_h ep;

    PML_UCX_TRACE_SEND("i%ssend request *%p", 
                       buf, count, datatype, dst, tag, mode, comm, 
                       mode == MCA_PML_BASE_SEND_BUFFERED ? "b" : "",
                       (void*)request)

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        return OMPI_ERROR;
    }

    req = (ompi_request_t*)mca_pml_ucx_common_send(ep, buf, count, datatype,
                                                   mca_pml_ucx_get_datatype(datatype),
                                                   PML_UCX_MAKE_SEND_TAG(tag, comm), mode,
                                                   mca_pml_ucx_send_completion);

    if (req == NULL) {
        PML_UCX_VERBOSE(8, "returning completed request");
        *request = &ompi_pml_ucx.completed_send_req;
        return OMPI_SUCCESS;
    } else if (!UCS_PTR_IS_ERR(req)) {
        PML_UCX_VERBOSE(8, "got request %p", (void*)req);
        *request = req;
        return OMPI_SUCCESS;
    } else {
        PML_UCX_ERROR("ucx send failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }
}

static inline __opal_attribute_always_inline__ int
mca_pml_ucx_send_nb(ucp_ep_h ep, const void *buf, size_t count,
                    ompi_datatype_t *datatype, ucp_datatype_t ucx_datatype,
                    ucp_tag_t tag, mca_pml_base_send_mode_t mode,
                    ucp_send_callback_t cb)
{
    ompi_request_t *req;

    req = (ompi_request_t*)mca_pml_ucx_common_send(ep, buf, count, datatype,
                                                   mca_pml_ucx_get_datatype(datatype),
                                                   tag, mode,
                                                   mca_pml_ucx_send_completion);

    if (OPAL_LIKELY(req == NULL)) {
        return OMPI_SUCCESS;
    } else if (!UCS_PTR_IS_ERR(req)) {
        PML_UCX_VERBOSE(8, "got request %p", (void*)req);
        ucp_worker_progress(ompi_pml_ucx.ucp_worker);
        ompi_request_wait(&req, MPI_STATUS_IGNORE);
        return OMPI_SUCCESS;
    } else {
        PML_UCX_ERROR("ucx send failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }
}

#if HAVE_DECL_UCP_TAG_SEND_NBR
static inline __opal_attribute_always_inline__ int
mca_pml_ucx_send_nbr(ucp_ep_h ep, const void *buf, size_t count,
                     ucp_datatype_t ucx_datatype, ucp_tag_t tag)

{
    void *req;
    ucs_status_t status;

    req    = PML_UCX_REQ_ALLOCA();
    status = ucp_tag_send_nbr(ep, buf, count, ucx_datatype, tag, req);
    if (OPAL_LIKELY(status == UCS_OK)) {
        return OMPI_SUCCESS;
    }

    ucp_worker_progress(ompi_pml_ucx.ucp_worker);
    while ((status = ucp_request_check_status(req)) == UCS_INPROGRESS) {
        opal_progress();
    }

    return OPAL_LIKELY(UCS_OK == status) ? OMPI_SUCCESS : OMPI_ERROR;
}
#endif

int mca_pml_ucx_send(const void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                     int tag, mca_pml_base_send_mode_t mode,
                     struct ompi_communicator_t* comm)
{
    ucp_ep_h ep;

    PML_UCX_TRACE_SEND("%s", buf, count, datatype, dst, tag, mode, comm,
                       mode == MCA_PML_BASE_SEND_BUFFERED ? "bsend" : "send");

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        return OMPI_ERROR;
    }

#if HAVE_DECL_UCP_TAG_SEND_NBR
    if (OPAL_LIKELY((MCA_PML_BASE_SEND_BUFFERED != mode) &&
                    (MCA_PML_BASE_SEND_SYNCHRONOUS != mode))) {
        return mca_pml_ucx_send_nbr(ep, buf, count,
                                    mca_pml_ucx_get_datatype(datatype),
                                    PML_UCX_MAKE_SEND_TAG(tag, comm));
    }
#endif

    return mca_pml_ucx_send_nb(ep, buf, count, datatype,
                               mca_pml_ucx_get_datatype(datatype),
                               PML_UCX_MAKE_SEND_TAG(tag, comm), mode,
                               mca_pml_ucx_send_completion);
}

int mca_pml_ucx_iprobe(int src, int tag, struct ompi_communicator_t* comm,
                         int *matched, ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("iprobe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    ucp_msg = ucp_tag_probe_nb(ompi_pml_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                               0, &info);
    if (ucp_msg != NULL) {
        *matched = 1;
        mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
    } else {
        opal_progress();
        *matched = 0;
    }
    return OMPI_SUCCESS;
}

int mca_pml_ucx_probe(int src, int tag, struct ompi_communicator_t* comm,
                        ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("probe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    for (;;) {
        ucp_msg = ucp_tag_probe_nb(ompi_pml_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                                   0, &info);
        if (ucp_msg != NULL) {
            mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
            return OMPI_SUCCESS;
        }

        opal_progress();
    }
}

int mca_pml_ucx_improbe(int src, int tag, struct ompi_communicator_t* comm,
                          int *matched, struct ompi_message_t **message,
                          ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("improbe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    ucp_msg = ucp_tag_probe_nb(ompi_pml_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                               1, &info);
    if (ucp_msg != NULL) {
        PML_UCX_MESSAGE_NEW(comm, ucp_msg, &info, message);
        PML_UCX_VERBOSE(8, "got message %p (%p)", (void*)*message, (void*)ucp_msg);
        *matched         = 1;
        mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
    } else  {
        opal_progress();
        *matched = 0;
    }
    return OMPI_SUCCESS;
}

int mca_pml_ucx_mprobe(int src, int tag, struct ompi_communicator_t* comm,
                         struct ompi_message_t **message,
                         ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("mprobe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    for (;;) {
        ucp_msg = ucp_tag_probe_nb(ompi_pml_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                                   1, &info);
        if (ucp_msg != NULL) {
            PML_UCX_MESSAGE_NEW(comm, ucp_msg, &info, message);
            PML_UCX_VERBOSE(8, "got message %p (%p)", (void*)*message, (void*)ucp_msg);
            mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
            return OMPI_SUCCESS;
        }

        opal_progress();
    }
}

int mca_pml_ucx_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                         struct ompi_message_t **message,
                         struct ompi_request_t **request)
{
    ompi_request_t *req;

    PML_UCX_TRACE_MRECV("imrecv", buf, count, datatype, message);

    req = (ompi_request_t*)ucp_tag_msg_recv_nb(ompi_pml_ucx.ucp_worker, buf, count,
                                               mca_pml_ucx_get_datatype(datatype),
                                               (*message)->req_ptr,
                                               mca_pml_ucx_recv_completion);
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx msg recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    PML_UCX_VERBOSE(8, "got request %p", (void*)req);
    PML_UCX_MESSAGE_RELEASE(message);
    *request = req;
    return OMPI_SUCCESS;
}

int mca_pml_ucx_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message,
                        ompi_status_public_t* status)
{
    ompi_request_t *req;

    PML_UCX_TRACE_MRECV("mrecv", buf, count, datatype, message);

    req = (ompi_request_t*)ucp_tag_msg_recv_nb(ompi_pml_ucx.ucp_worker, buf, count,
                                               mca_pml_ucx_get_datatype(datatype),
                                               (*message)->req_ptr,
                                               mca_pml_ucx_recv_completion);
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx msg recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    PML_UCX_MESSAGE_RELEASE(message);

    ompi_request_wait(&req, status);
    return OMPI_SUCCESS;
}

int mca_pml_ucx_start(size_t count, ompi_request_t** requests)
{
    mca_pml_ucx_persistent_request_t *preq;
    ompi_request_t *tmp_req;
    size_t i;

    for (i = 0; i < count; ++i) {
        preq = (mca_pml_ucx_persistent_request_t *)requests[i];

        if ((preq == NULL) || (OMPI_REQUEST_PML != preq->ompi.req_type)) {
            /* Skip irrelevant requests */
            continue;
        }

        PML_UCX_ASSERT(preq->ompi.req_state != OMPI_REQUEST_INVALID);
        preq->ompi.req_state = OMPI_REQUEST_ACTIVE;
        mca_pml_ucx_request_reset(&preq->ompi);

        if (preq->flags & MCA_PML_UCX_REQUEST_FLAG_SEND) {
            tmp_req = (ompi_request_t*)mca_pml_ucx_common_send(preq->send.ep,
                                                               preq->buffer,
                                                               preq->count,
                                                               preq->ompi_datatype,
                                                               preq->datatype,
                                                               preq->tag,
                                                               preq->send.mode,
                                                               mca_pml_ucx_psend_completion);
        } else {
            PML_UCX_VERBOSE(8, "start recv request %p", (void*)preq);
            tmp_req = (ompi_request_t*)ucp_tag_recv_nb(ompi_pml_ucx.ucp_worker,
                                                       preq->buffer, preq->count,
                                                       preq->datatype, preq->tag,
                                                       preq->recv.tag_mask,
                                                       mca_pml_ucx_precv_completion);
        }

        if (tmp_req == NULL) {
            /* Only send can complete immediately */
            PML_UCX_ASSERT(preq->flags & MCA_PML_UCX_REQUEST_FLAG_SEND);

            PML_UCX_VERBOSE(8, "send completed immediately, completing persistent request %p",
                            (void*)preq);
            mca_pml_ucx_set_send_status(&preq->ompi.req_status, UCS_OK);
            ompi_request_complete(&preq->ompi, true);
        } else if (!UCS_PTR_IS_ERR(tmp_req)) {
            if (REQUEST_COMPLETE(tmp_req)) {
                /* tmp_req is already completed */
                PML_UCX_VERBOSE(8, "completing persistent request %p", (void*)preq);
                mca_pml_ucx_persistent_request_complete(preq, tmp_req);
            } else {
                /* tmp_req would be completed by callback and trigger completion
                 * of preq */
                PML_UCX_VERBOSE(8, "temporary request %p will complete persistent request %p",
                                (void*)tmp_req, (void*)preq);
                tmp_req->req_complete_cb_data = preq;
                preq->tmp_req                 = tmp_req;
            }
        } else {
            PML_UCX_ERROR("ucx %s failed: %s",
                          (preq->flags & MCA_PML_UCX_REQUEST_FLAG_SEND) ? "send" : "recv",
                          ucs_status_string(UCS_PTR_STATUS(tmp_req)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

int mca_pml_ucx_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_SUCCESS;
}
