/*
 * Copyright (C) 2001-2011 Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2018-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018 IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Huawei Technologies Co., Ltd.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx.h"

#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "ompi/attribute/attribute.h"
#include "ompi/message/message.h"
#include "ompi/runtime/ompi_spc.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "opal/mca/common/ucx/common_ucx.h"
#if OPAL_CUDA_SUPPORT
#include "opal/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */
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
    .super = {
        .pml_add_procs     = mca_pml_ucx_add_procs,
        .pml_del_procs     = mca_pml_ucx_del_procs,
        .pml_enable        = mca_pml_ucx_enable,
        .pml_progress      = NULL,
        .pml_add_comm      = mca_pml_ucx_add_comm,
        .pml_del_comm      = mca_pml_ucx_del_comm,
        .pml_irecv_init    = mca_pml_ucx_irecv_init,
        .pml_irecv         = mca_pml_ucx_irecv,
        .pml_recv          = mca_pml_ucx_recv,
        .pml_isend_init    = mca_pml_ucx_isend_init,
        .pml_isend         = mca_pml_ucx_isend,
        .pml_send          = mca_pml_ucx_send,
        .pml_iprobe        = mca_pml_ucx_iprobe,
        .pml_probe         = mca_pml_ucx_probe,
        .pml_start         = mca_common_ucx_start,
        .pml_improbe       = mca_pml_ucx_improbe,
        .pml_mprobe        = mca_pml_ucx_mprobe,
        .pml_imrecv        = mca_pml_ucx_imrecv,
        .pml_mrecv         = mca_pml_ucx_mrecv,
        .pml_dump          = mca_pml_ucx_dump,
        .pml_ft_event      = NULL,
        .pml_max_contextid = (1ul << (PML_UCX_CONTEXT_BITS)) - 1,
        .pml_max_tag       = (1ul << (PML_UCX_TAG_BITS - 1)) - 1,
        .pml_flags         = 0 /* flags */
    }
};

#define PML_UCX_REQ_ALLOCA() \
    ((char *)alloca(ompi_pml_ucx.request_size) + ompi_pml_ucx.request_size);

static ucp_ep_h mca_pml_ucx_add_proc_common(ompi_proc_t *proc)
{
    size_t addrlen = 0;
    ucp_ep_params_t ep_params;
    ucp_address_t *address;
    ucs_status_t status;
    ucp_ep_h ep;
    int ret;

    opal_process_name_t *proc_name = &proc->super.proc_name;

    ret = opal_common_ucx_recv_worker_address(proc_name, &address, &addrlen);
    if (ret < 0) {
        return NULL;
    }

    PML_UCX_VERBOSE(2, "connecting to proc. %d", proc_name->vpid);

    ep_params.field_mask = UCP_EP_PARAM_FIELD_REMOTE_ADDRESS;
    ep_params.address    = address;

    status = ucp_ep_create(opal_common_ucx.ucp_worker, &ep_params, &ep);
    free(address);
    if (UCS_OK != status) {
        PML_UCX_ERROR("ucp_ep_create(proc=%d) failed: %s", proc_name->vpid,
                      ucs_status_string(status));
        return NULL;
    }

    proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = ep;
    return ep;
}

int mca_pml_ucx_add_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ompi_proc_t *proc;
    ucp_ep_h ep;
    size_t i;

    for (i = 0; i < nprocs; ++i) {
        proc = procs[(i + OMPI_PROC_MY_NAME->vpid) % nprocs];
        ep = mca_pml_ucx_add_proc_common(proc);
        if (ep == NULL) {
            return OMPI_ERROR;
        }
    }

    opal_common_ucx_mca_proc_added();
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__
static inline ucp_ep_h mca_pml_ucx_get_ep(ompi_communicator_t *comm, int rank)
{
    ompi_proc_t *proc_peer = ompi_comm_peer_lookup(comm, rank);
    ucp_ep_h ep;

    ep = proc_peer->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    if (OPAL_LIKELY(NULL != ep)) {
        return ep;
    }

    return mca_pml_ucx_add_proc_common(proc_peer);
}

int mca_pml_ucx_del_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    ompi_proc_t *proc;
    opal_common_ucx_del_proc_t *del_procs;
    size_t i;
    int ret;

    del_procs = malloc(sizeof(*del_procs) * nprocs);
    if (del_procs == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < nprocs; ++i) {
        proc = procs[i];
        del_procs[i].ep   = proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        del_procs[i].vpid = proc->super.proc_name.vpid;

        /* mark peer as disconnected */
        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = NULL;
    }

    ret = opal_common_ucx_del_procs(del_procs, nprocs, OMPI_PROC_MY_NAME->vpid,
                                    ompi_pml_ucx.num_disconnect, opal_common_ucx.ucp_worker);
    free(del_procs);

    return ret;
}

int mca_pml_ucx_enable(bool enable)
{
    mca_common_ucx_datatype_t *ucx_datatype   = &ompi_common_ucx.datatype_init;
    ucx_datatype->op_param.send.op_attr_mask  = UCP_OP_ATTR_FIELD_CALLBACK;
    ucx_datatype->op_param.send.cb.send       = mca_pml_ucx_send_nbx_completion;
    ucx_datatype->op_param.bsend.op_attr_mask = UCP_OP_ATTR_FIELD_CALLBACK;
    ucx_datatype->op_param.bsend.cb.send      = mca_pml_ucx_bsend_nbx_completion;
    ucx_datatype->op_param.recv.op_attr_mask  = UCP_OP_ATTR_FIELD_CALLBACK |
                                                UCP_OP_ATTR_FLAG_NO_IMM_CMPL;
    ucx_datatype->op_param.recv.cb.recv       = mca_pml_ucx_recv_nbx_completion;

    mca_common_ucx_enable();

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

static ompi_request_t*
mca_pml_ucx_persistent_request_start(mca_common_ucx_persistent_request_t *req);

int mca_pml_ucx_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int src, int tag, struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{
    mca_pml_ucx_persistent_request_t *req;
    int rc;

    rc = mca_common_ucx_persistent_request_init(OMPI_REQUEST_PML, comm,
            mca_pml_ucx_persistent_request_start,
            (mca_common_ucx_persistent_request_t**)&req);
    if (rc != OMPI_SUCCESS) {
        return rc;
    }

    PML_UCX_TRACE_RECV("irecv_init request *%p=%p", buf, count, datatype, src,
                       tag, comm, (void*)request, (void*)req);

    req->flags                    = 0;
    req->buffer                   = buf;
    req->count                    = count;
    req->datatype.datatype        = mca_common_ucx_get_datatype(datatype);

    PML_UCX_MAKE_RECV_TAG(req->tag, req->recv.tag_mask, tag, src, comm);

    *request = &req->super.ompi;
    return OMPI_SUCCESS;
}

int mca_pml_ucx_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                      int src, int tag, struct ompi_communicator_t* comm,
                      struct ompi_request_t **request)
{
#if HAVE_DECL_UCP_TAG_RECV_NBX
    mca_common_ucx_datatype_t *op_data = mca_common_ucx_get_op_data(datatype);
    ucp_request_param_t *param  = &op_data->op_param.recv;
#endif

    ucp_tag_t ucp_tag, ucp_tag_mask;
    ompi_request_t *req;

    PML_UCX_TRACE_RECV("irecv request *%p", buf, count, datatype, src, tag, comm,
                       (void*)request);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
#if HAVE_DECL_UCP_TAG_RECV_NBX
    req = (ompi_request_t*)ucp_tag_recv_nbx(opal_common_ucx.ucp_worker, buf,
                                            mca_common_ucx_get_data_size(op_data, count),
                                            ucp_tag, ucp_tag_mask, param);
#else
    req = (ompi_request_t*)ucp_tag_recv_nb(opal_common_ucx.ucp_worker, buf, count,
                                           mca_common_ucx_get_datatype(datatype),
                                           ucp_tag, ucp_tag_mask,
                                           mca_pml_ucx_recv_completion);
#endif
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    PML_UCX_VERBOSE(8, "got request %p", (void*)req);
    req->req_mpi_object.comm = comm;
    *request                 = req;
    return OMPI_SUCCESS;
}

int mca_pml_ucx_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                     int tag, struct ompi_communicator_t* comm,
                     ompi_status_public_t* mpi_status)
{
    /* coverity[bad_alloc_arithmetic] */
    void *req = PML_UCX_REQ_ALLOCA();
#if HAVE_DECL_UCP_TAG_RECV_NBX
    mca_common_ucx_datatype_t *op_data     = mca_common_ucx_get_op_data(datatype);
    ucp_request_param_t *recv_param = &op_data->op_param.recv;
    ucp_request_param_t param;

    param.op_attr_mask = UCP_OP_ATTR_FIELD_REQUEST |
                         (recv_param->op_attr_mask & UCP_OP_ATTR_FIELD_DATATYPE);
    param.datatype     = recv_param->datatype;
    param.request      = req;
#endif
    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucs_status_t status;
    int result;

    PML_UCX_TRACE_RECV("%s", buf, count, datatype, src, tag, comm, "recv");

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
#if HAVE_DECL_UCP_TAG_RECV_NBX
    ucp_tag_recv_nbx(opal_common_ucx.ucp_worker, buf,
                     mca_common_ucx_get_data_size(op_data, count),
                     ucp_tag, ucp_tag_mask, &param);
#else
    ucp_tag_recv_nbr(opal_common_ucx.ucp_worker, buf, count,
                     mca_common_ucx_get_datatype(datatype),
                     ucp_tag, ucp_tag_mask, req);
#endif
    MCA_COMMON_UCX_PROGRESS_LOOP(opal_common_ucx.ucp_worker) {
        status = ucp_request_test(req, &info);
        if (status != UCS_INPROGRESS) {
            result = mca_pml_ucx_set_recv_status_safe(mpi_status, status, &info);

#if SPC_ENABLE == 1
            size_t dt_size;
            ompi_datatype_type_size(datatype, &dt_size);
            SPC_USER_OR_MPI(tag, dt_size*count,
                            OMPI_SPC_BYTES_RECEIVED_USER, OMPI_SPC_BYTES_RECEIVED_MPI);
#endif
            return result;
        }
    }
}

__opal_attribute_always_inline__
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
    int rc;

    rc = mca_common_ucx_persistent_request_init(OMPI_REQUEST_PML, comm,
            mca_pml_ucx_persistent_request_start,
            (mca_common_ucx_persistent_request_t**)&req);
    if (rc != OMPI_SUCCESS) {
        return rc;
    }

    PML_UCX_TRACE_SEND("isend_init request *%p=%p", buf, count, datatype, dst,
                       tag, mode, comm, (void*)request, (void*)req)

    ep = mca_pml_ucx_get_ep(comm, dst);
    if (OPAL_UNLIKELY(NULL == ep)) {
        return OMPI_ERROR;
    }

    req->flags                    = MCA_PML_UCX_REQUEST_FLAG_SEND;
    req->buffer                   = (void *)buf;
    req->count                    = count;
    req->tag                      = PML_UCX_MAKE_SEND_TAG(tag, comm);
    req->send.mode                = mode;
    req->send.ep                  = ep;

    if (MCA_PML_BASE_SEND_BUFFERED == mode) {
        req->datatype.ompi_datatype = datatype;
        OBJ_RETAIN(datatype);
    } else {
        req->datatype.datatype = mca_common_ucx_get_datatype(datatype);
    }

    *request = &req->super.ompi;
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

__opal_attribute_always_inline__
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

#if HAVE_DECL_UCP_TAG_SEND_NBX
__opal_attribute_always_inline__
static inline ucs_status_ptr_t
mca_pml_ucx_common_send_nbx(ucp_ep_h ep, const void *buf,
                            size_t count,
                            ompi_datatype_t *datatype,
                            ucp_tag_t tag,
                            mca_pml_base_send_mode_t mode,
                            ucp_request_param_t *param)
{
    mca_common_ucx_datatype_t *op_data = mca_common_ucx_get_op_data(datatype);

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == mode)) {
        return mca_pml_ucx_bsend(ep, buf, count, datatype, tag);
    } else if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        return ucp_tag_send_sync_nb(ep, buf, count,
                                    mca_common_ucx_get_datatype(datatype), tag,
                                    (ucp_send_callback_t)param->cb.send);
    } else {
        return ucp_tag_send_nbx(ep, buf,
                                mca_common_ucx_get_data_size(op_data, count),
                                tag, param);
    }
}
#endif

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

#if HAVE_DECL_UCP_TAG_SEND_NBX
    req = (ompi_request_t*)mca_pml_ucx_common_send_nbx(ep, buf, count, datatype,
                                                       PML_UCX_MAKE_SEND_TAG(tag, comm), mode,
                                                       &mca_common_ucx_get_op_data(datatype)->op_param.send);
#else
    req = (ompi_request_t*)mca_pml_ucx_common_send(ep, buf, count, datatype,
                                                   mca_common_ucx_get_datatype(datatype),
                                                   PML_UCX_MAKE_SEND_TAG(tag, comm), mode,
                                                   mca_pml_ucx_send_completion);
#endif

#if SPC_ENABLE == 1
    size_t dt_size;
    ompi_datatype_type_size(datatype, &dt_size);
    SPC_USER_OR_MPI(tag, dt_size*count,
                    OMPI_SPC_BYTES_SENT_USER, OMPI_SPC_BYTES_SENT_MPI);
#endif

    if (req == NULL) {
        PML_UCX_VERBOSE(8, "returning completed request");
        *request = &ompi_common_ucx.completed_request;
        return OMPI_SUCCESS;
    } else if (!UCS_PTR_IS_ERR(req)) {
        PML_UCX_VERBOSE(8, "got request %p", (void*)req);
        req->req_mpi_object.comm = comm;
        *request                 = req;
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
                                                   mca_common_ucx_get_datatype(datatype),
                                                   tag, mode, cb);
    if (OPAL_LIKELY(req == NULL)) {
        return OMPI_SUCCESS;
    } else if (!UCS_PTR_IS_ERR(req)) {
        PML_UCX_VERBOSE(8, "got request %p", (void*)req);
        MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCP,
                opal_common_ucx.ucp_worker, "ucx send", ompi_request_free(&req));
    } else {
        PML_UCX_ERROR("ucx send failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }
}

#if HAVE_DECL_UCP_TAG_SEND_NBR
static inline __opal_attribute_always_inline__ int
mca_pml_ucx_send_nbr(ucp_ep_h ep, const void *buf, size_t count,
                     ompi_datatype_t *datatype, ucp_tag_t tag)
{
    /* coverity[bad_alloc_arithmetic] */
    ucs_status_ptr_t req = PML_UCX_REQ_ALLOCA();
#if HAVE_DECL_UCP_TAG_SEND_NBX
    mca_common_ucx_datatype_t *op_data = mca_common_ucx_get_op_data(datatype);
    ucp_request_param_t param   = {
        .op_attr_mask = UCP_OP_ATTR_FIELD_REQUEST |
                        (op_data->op_param.send.op_attr_mask & UCP_OP_ATTR_FIELD_DATATYPE) |
                        UCP_OP_ATTR_FLAG_FAST_CMPL,
        .datatype     = op_data->op_param.send.datatype,
        .request      = req
    };

    req = ucp_tag_send_nbx(ep, buf,
                           mca_common_ucx_get_data_size(op_data, count),
                           tag, &param);
    if (OPAL_LIKELY(req == UCS_OK)) {
        return OMPI_SUCCESS;
    } else if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("%s failed: %d, %s", __func__, UCS_PTR_STATUS(req),
                      ucs_status_string(UCS_PTR_STATUS(req)));
        return OPAL_ERROR;
    }
#else
    ucs_status_t status;
    status = ucp_tag_send_nbr(ep, buf, count,
                              mca_common_ucx_get_datatype(datatype), tag, req);
    if (OPAL_LIKELY(status == UCS_OK)) {
        return OMPI_SUCCESS;
    }
#endif

    MCA_COMMON_UCX_WAIT_LOOP(req, OPAL_COMMON_UCX_REQUEST_TYPE_UCP,
            opal_common_ucx.ucp_worker, "ucx send nbr", (void)0);
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

#if SPC_ENABLE == 1
    size_t dt_size;
    ompi_datatype_type_size(datatype, &dt_size);
    SPC_USER_OR_MPI(tag, dt_size*count,
                    OMPI_SPC_BYTES_SENT_USER, OMPI_SPC_BYTES_SENT_MPI);
#endif

#if HAVE_DECL_UCP_TAG_SEND_NBR
    if (OPAL_LIKELY((MCA_PML_BASE_SEND_BUFFERED != mode) &&
                    (MCA_PML_BASE_SEND_SYNCHRONOUS != mode))) {
        return mca_pml_ucx_send_nbr(ep, buf, count, datatype,
                                    PML_UCX_MAKE_SEND_TAG(tag, comm));
    }
#endif

    return mca_pml_ucx_send_nb(ep, buf, count, datatype,
                               mca_common_ucx_get_datatype(datatype),
                               PML_UCX_MAKE_SEND_TAG(tag, comm), mode,
                               mca_pml_ucx_send_completion);
}

int mca_pml_ucx_iprobe(int src, int tag, struct ompi_communicator_t* comm,
                       int *matched, ompi_status_public_t* mpi_status)
{
    static unsigned progress_count = 0;

    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("iprobe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    ucp_msg = ucp_tag_probe_nb(opal_common_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                               0, &info);
    if (ucp_msg != NULL) {
        *matched = 1;
        mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
    } else  {
        (++progress_count % opal_common_ucx.progress_iterations) ?
            (void)ucp_worker_progress(opal_common_ucx.ucp_worker) : opal_progress();
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

    MCA_COMMON_UCX_PROGRESS_LOOP(opal_common_ucx.ucp_worker) {
        ucp_msg = ucp_tag_probe_nb(opal_common_ucx.ucp_worker, ucp_tag,
                                   ucp_tag_mask, 0, &info);
        if (ucp_msg != NULL) {
            mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
            return OMPI_SUCCESS;
        }
    }
}

int mca_pml_ucx_improbe(int src, int tag, struct ompi_communicator_t* comm,
                        int *matched, struct ompi_message_t **message,
                        ompi_status_public_t* mpi_status)
{
    static unsigned progress_count = 0;

    ucp_tag_t ucp_tag, ucp_tag_mask;
    ucp_tag_recv_info_t info;
    ucp_tag_message_h ucp_msg;

    PML_UCX_TRACE_PROBE("improbe", src, tag, comm);

    PML_UCX_MAKE_RECV_TAG(ucp_tag, ucp_tag_mask, tag, src, comm);
    ucp_msg = ucp_tag_probe_nb(opal_common_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                               1, &info);
    if (ucp_msg != NULL) {
        PML_UCX_MESSAGE_NEW(comm, ucp_msg, &info, message);
        PML_UCX_VERBOSE(8, "got message %p (%p)", (void*)*message, (void*)ucp_msg);
        *matched         = 1;
        mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
    } else  {
        (++progress_count % opal_common_ucx.progress_iterations) ?
            (void)ucp_worker_progress(opal_common_ucx.ucp_worker) : opal_progress();
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
    MCA_COMMON_UCX_PROGRESS_LOOP(opal_common_ucx.ucp_worker) {
        ucp_msg = ucp_tag_probe_nb(opal_common_ucx.ucp_worker, ucp_tag, ucp_tag_mask,
                                   1, &info);
        if (ucp_msg != NULL) {
            PML_UCX_MESSAGE_NEW(comm, ucp_msg, &info, message);
            PML_UCX_VERBOSE(8, "got message %p (%p)", (void*)*message, (void*)ucp_msg);
            mca_pml_ucx_set_recv_status_safe(mpi_status, UCS_OK, &info);
            return OMPI_SUCCESS;
        }
    }
}

int mca_pml_ucx_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                         struct ompi_message_t **message,
                         struct ompi_request_t **request)
{
    ompi_request_t *req;

    PML_UCX_TRACE_MRECV("imrecv", buf, count, datatype, message);

    req = (ompi_request_t*)ucp_tag_msg_recv_nb(opal_common_ucx.ucp_worker, buf, count,
                                               mca_common_ucx_get_datatype(datatype),
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

    req = (ompi_request_t*)ucp_tag_msg_recv_nb(opal_common_ucx.ucp_worker, buf, count,
                                               mca_common_ucx_get_datatype(datatype),
                                               (*message)->req_ptr,
                                               mca_pml_ucx_recv_completion);
    if (UCS_PTR_IS_ERR(req)) {
        PML_UCX_ERROR("ucx msg recv failed: %s", ucs_status_string(UCS_PTR_STATUS(req)));
        return OMPI_ERROR;
    }

    PML_UCX_MESSAGE_RELEASE(message);

    return ompi_request_wait(&req, status);
}

static ompi_request_t*
mca_pml_ucx_persistent_request_start(mca_common_ucx_persistent_request_t *req)
{
    ompi_request_t *tmp_req;

    mca_pml_ucx_persistent_request_t *preq =
            (mca_pml_ucx_persistent_request_t*)req;

    if (preq->flags & MCA_PML_UCX_REQUEST_FLAG_SEND) {
        tmp_req = (ompi_request_t*)mca_pml_ucx_common_send(preq->send.ep,
                                                           preq->buffer,
                                                           preq->count,
                                                           preq->datatype.ompi_datatype,
                                                           preq->datatype.datatype,
                                                           preq->tag,
                                                           preq->send.mode,
                                                           mca_pml_ucx_psend_completion);
    } else {
        PML_UCX_VERBOSE(8, "start recv request %p", (void*)preq);
        tmp_req = (ompi_request_t*)ucp_tag_recv_nb(opal_common_ucx.ucp_worker,
                                                   preq->buffer, preq->count,
                                                   preq->datatype.datatype,
                                                   preq->tag,
                                                   preq->recv.tag_mask,
                                                   mca_pml_ucx_precv_completion);
    }

    return tmp_req;
}

int mca_pml_ucx_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_SUCCESS;
}
