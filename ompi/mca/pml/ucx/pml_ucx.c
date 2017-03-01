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
        1ul << (PML_UCX_TAG_BITS - 1),
        1ul << (PML_UCX_CONTEXT_BITS),
    },
    NULL,
    NULL
};

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
        PML_UCX_ERROR("Failed to receive EP address");
    }
    return ret;
}

int mca_pml_ucx_open(void)
{
    ucp_params_t params;
    ucp_config_t *config;
    ucs_status_t status;

    PML_UCX_VERBOSE(1, "mca_pml_ucx_open");

    /* Read options */
    status = ucp_config_read("MPI", NULL, &config);
    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

    params.field_mask      = UCP_PARAM_FIELD_FEATURES |
                             UCP_PARAM_FIELD_REQUEST_SIZE |
                             UCP_PARAM_FIELD_REQUEST_INIT |
                             UCP_PARAM_FIELD_REQUEST_CLEANUP |
                             UCP_PARAM_FIELD_TAG_SENDER_MASK;
    params.features        = UCP_FEATURE_TAG;
    params.request_size    = sizeof(ompi_request_t);
    params.request_init    = mca_pml_ucx_request_init;
    params.request_cleanup = mca_pml_ucx_request_cleanup;

    status = ucp_init(&params, config, &ompi_pml_ucx.ucp_context);
    ucp_config_release(config);

    if (UCS_OK != status) {
        return OMPI_ERROR;
    }

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
    int rc;

    PML_UCX_VERBOSE(1, "mca_pml_ucx_init");

    /* TODO check MPI thread mode */
    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    params.thread_mode = UCS_THREAD_MODE_SINGLE;

    status = ucp_worker_create(ompi_pml_ucx.ucp_context, &params,
                               &ompi_pml_ucx.ucp_worker);
    if (UCS_OK != status) {
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

ucp_ep_h mca_pml_ucx_add_proc(ompi_communicator_t *comm, int dst)
{
    ucp_ep_params_t ep_params;
    ucp_address_t *address;
    ucs_status_t status;
    size_t addrlen;
    ucp_ep_h ep;
    int ret;

    ompi_proc_t *proc0      = ompi_comm_peer_lookup(comm, 0);
    ompi_proc_t *proc_peer = ompi_comm_peer_lookup(comm, dst);

    /* Note, mca_pml_base_pml_check_selected, doesn't use 3rd argument */
    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("ucx",
                                                              &proc0,
                                                              dst))) {
        return NULL;
    }

    ret = mca_pml_ucx_recv_worker_address(proc_peer, &address, &addrlen);
    if (ret < 0) {
        PML_UCX_ERROR("Failed to receive worker address from proc: %d", proc_peer->super.proc_name.vpid);
        return NULL;
    }

    PML_UCX_VERBOSE(2, "connecting to proc. %d", proc_peer->super.proc_name.vpid);

    ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
    ep_params.address    = address;

    status = ucp_ep_create(ompi_pml_ucx.ucp_worker, &ep_params, &ep);
    free(address);
    if (UCS_OK != status) {
        PML_UCX_ERROR("Failed to connect to proc: %d, %s", proc_peer->super.proc_name.vpid,
                                                           ucs_status_string(status));
        return NULL;
    }

    proc_peer->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = ep;

    return ep;
}

int mca_pml_ucx_add_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ucp_ep_params_t ep_params;
    ucp_address_t *address;
    ucs_status_t status;
    size_t addrlen;
    ucp_ep_h ep;
    size_t i;
    int ret;

    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("ucx",
                                                              procs,
                                                              nprocs))) {
        return ret;
    }

    for (i = 0; i < nprocs; ++i) {
        ret = mca_pml_ucx_recv_worker_address(procs[i], &address, &addrlen);
        if (ret < 0) {
            PML_UCX_ERROR("Failed to receive worker address from proc: %d", procs[i]->super.proc_name.vpid);
            return ret;
        }

        if (procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML]) {
            PML_UCX_VERBOSE(3, "already connected to proc. %d", procs[i]->super.proc_name.vpid);
            continue;
        }

        PML_UCX_VERBOSE(2, "connecting to proc. %d", procs[i]->super.proc_name.vpid);

        ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
        ep_params.address    = address;

        status = ucp_ep_create(ompi_pml_ucx.ucp_worker, &ep_params, &ep);
        free(address);

        if (UCS_OK != status) {
            PML_UCX_ERROR("Failed to connect to proc: %d, %s", procs[i]->super.proc_name.vpid,
                                                               ucs_status_string(status));
            return OMPI_ERROR;
        }

        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = ep;
    }

    return OMPI_SUCCESS;
}

int mca_pml_ucx_del_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ucp_ep_h ep;
    size_t i;

    for (i = 0; i < nprocs; ++i) {
        PML_UCX_VERBOSE(2, "disconnecting from rank %d", procs[i]->super.proc_name.vpid);
        ep = procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        if (ep != NULL) {
            ucp_ep_destroy(ep);
        }
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = NULL;
    }
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
    static int inprogress = 0;
    if (inprogress != 0) {
        return 0;
    }

    ++inprogress;
    ucp_worker_progress(ompi_pml_ucx.ucp_worker);
    --inprogress;
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

static void
mca_pml_ucx_blocking_recv_completion(void *request, ucs_status_t status,
                                     ucp_tag_recv_info_t *info)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "blocking receive request %p completed with status %s tag %"PRIx64" len %zu",
                    (void*)req, ucs_status_string(status), info->sender_tag,
                    info->length);

    mca_pml_ucx_set_recv_status(&req->req_status, status, info);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req,true);
}

int mca_pml_ucx_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                     int tag, struct ompi_communicator_t* comm,
                     ompi_status_public_t* mpi_status)
{
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ompi_request_t *req;

    PML_UCX_TRACE_RECV("%s", buf, count, datatype, src, tag, comm, "recv");

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    req = (ompi_request_t*)ucp_tag_recv_nb(ompi_pml_ucx.ucp_worker, buf, count,
                                           mca_pml_ucx_get_datatype(datatype),
                                           ucp_tag, ucp_tag_mask,
                                           mca_pml_ucx_blocking_recv_completion);
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    ucp_worker_progress(ompi_pml_ucx.ucp_worker);
    while ( !REQUEST_COMPLETE(req) ) {
        opal_progress();
    }

    if (mpi_status != MPI_STATUS_IGNORE) {
        *mpi_status = req->req_status;
    }

    req->req_complete = REQUEST_PENDING;
    ucp_request_release(req);
    return OMPI_SUCCESS;
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
        PML_UCX_ERROR("Failed to get ep for rank %d", dst);
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

static int 
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
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    iov_count    = 1;
    iov.iov_base = packed_data;
    iov.iov_len  = packed_length;

    PML_UCX_VERBOSE(8, "bsend of packed buffer %p len %d", packed_data, packed_length);
    offset = 0;
    opal_convertor_set_position(&opal_conv, &offset);
    if (0 > opal_convertor_pack(&opal_conv, &iov, &iov_count, &packed_length)) {
        mca_pml_base_bsend_request_free(packed_data);
        OBJ_DESTRUCT(&opal_conv);
        PML_UCX_ERROR("bsend: failed to pack user datatype");
        return OMPI_ERROR;
    }

    OBJ_DESTRUCT(&opal_conv);

    req = (ompi_request_t*)ucp_tag_send_nb(ep, packed_data, packed_length,
                                           ucp_dt_make_contig(1), pml_tag,
                                           mca_pml_ucx_bsend_completion);
    if (NULL == req) {
        /* request was completed in place */
        mca_pml_base_bsend_request_free(packed_data);
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(UCS_PTR_IS_ERR(req))) {
        mca_pml_base_bsend_request_free(packed_data);
        PML_UCX_ERROR("ucx bsend failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    req->req_complete_cb_data = packed_data;
    return OMPI_SUCCESS;
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

    /* TODO special care to sync/buffered send */

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        PML_UCX_ERROR("Failed to get ep for rank %d", dst);
        return OMPI_ERROR;
    }

    /* Special care to sync/buffered send */
    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == mode)) {
        *request = &ompi_pml_ucx.completed_send_req;
        return mca_pml_ucx_bsend(ep, buf, count, datatype, 
                                 PML_UCX_MAKE_SEND_TAG(tag, comm));
    }

    req = (ompi_request_t*)ucp_tag_send_nb(ep, buf, count,
                                           mca_pml_ucx_get_datatype(datatype),
                                           PML_UCX_MAKE_SEND_TAG(tag, comm),
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

int mca_pml_ucx_send(const void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                     int tag, mca_pml_base_send_mode_t mode,
                     struct ompi_communicator_t* comm)
{
    ompi_request_t *req;
    ucp_ep_h ep;

    PML_UCX_TRACE_SEND("%s", buf, count, datatype, dst, tag, mode, comm, 
                       mode == MCA_PML_BASE_SEND_BUFFERED ? "bsend" : "send");

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        PML_UCX_ERROR("Failed to get ep for rank %d", dst);
        return OMPI_ERROR;
    }

    /* Special care to sync/buffered send */
    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == mode)) {
        return mca_pml_ucx_bsend(ep, buf, count, datatype,
                                 PML_UCX_MAKE_SEND_TAG(tag, comm));
    }

    req = (ompi_request_t*)ucp_tag_send_nb(ep, buf, count,
                                           mca_pml_ucx_get_datatype(datatype),
                                           PML_UCX_MAKE_SEND_TAG(tag, comm),
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
    } else if (UCS_PTR_STATUS(ucp_msg) == UCS_ERR_NO_MESSAGE) {
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
    int rc;

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
            if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == preq->send.mode)) {
                PML_UCX_VERBOSE(8, "start bsend request %p", (void*)preq);
                rc = mca_pml_ucx_bsend(preq->send.ep, preq->buffer, preq->count,
                                       preq->ompi_datatype,  preq->tag);
                if (OMPI_SUCCESS != rc) {
                    return rc;
                }
                /* pretend that we got immediate completion */
                tmp_req = NULL;
            } else {
                PML_UCX_VERBOSE(8, "start send request %p", (void*)preq);
                tmp_req = (ompi_request_t*)ucp_tag_send_nb(preq->send.ep, preq->buffer,
                                                           preq->count, preq->datatype,
                                                           preq->tag,
                                                           mca_pml_ucx_psend_completion);
            }
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
