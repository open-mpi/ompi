/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_yalla.h"
#include "pml_yalla_request.h"

#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/message/message.h"
#include "opal/memoryhooks/memory.h"

#define MODEX_KEY "yalla-mxm"

mca_pml_yalla_module_t ompi_pml_yalla = {
    {
        mca_pml_yalla_add_procs,
        mca_pml_yalla_del_procs,
        mca_pml_yalla_enable,
        NULL,
        mca_pml_yalla_add_comm,
        mca_pml_yalla_del_comm,
        mca_pml_yalla_irecv_init,
        mca_pml_yalla_irecv,
        mca_pml_yalla_recv,
        mca_pml_yalla_isend_init,
        mca_pml_yalla_isend,
        mca_pml_yalla_send,
        mca_pml_yalla_iprobe,
        mca_pml_yalla_probe,
        mca_pml_yalla_start,
        mca_pml_yalla_improbe,
        mca_pml_yalla_mprobe,
        mca_pml_yalla_imrecv,
        mca_pml_yalla_mrecv,
        mca_pml_yalla_dump,
        NULL, /* FT */
        1ul << (sizeof(mxm_ctxid_t)*8) - 1,
        1ul << (sizeof(mxm_tag_t)*8 - 1) - 1,
    },
    NULL,
    NULL
};

static int send_ep_address(void)
{
    mxm_error_t error;
    void *address;
    size_t addrlen;
    int rc;

    addrlen = 0;
    mxm_ep_get_address(ompi_pml_yalla.mxm_ep, NULL, &addrlen);

    address = alloca(addrlen);
    error = mxm_ep_get_address(ompi_pml_yalla.mxm_ep, address, &addrlen);
    if (MXM_OK != error) {
        PML_YALLA_ERROR("Failed to get EP address");
        return OMPI_ERROR;
    }

    rc = ompi_modex_send(&mca_pml_yalla_component.pmlm_version, address, addrlen);
    if (OMPI_SUCCESS != rc) {
        PML_YALLA_ERROR("Open MPI couldn't distribute EP connection details");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static int recv_ep_address(ompi_proc_t *proc, void **address_p, size_t *addrlen_p)
{
    int ret = ompi_modex_recv(&mca_pml_yalla_component.pmlm_version, proc, address_p,
                              addrlen_p);

    if (ret < 0) {
        PML_YALLA_ERROR("Failed to receive EP address");
    }
    return ret;
}

static void mca_pml_yalla_mem_release_cb(void *buf, size_t length,
                                         void *cbdata, bool from_alloc)
{
    mxm_mem_unmap(ompi_pml_yalla.mxm_context, buf, length,
                  from_alloc ? MXM_MEM_UNMAP_MARK_INVALID : 0);
}

int mca_pml_yalla_init(void)
{
    mxm_context_opts_t *ctx_opts;
    mxm_ep_opts_t *ep_opts;
    mxm_error_t error;
    int rc;

    PML_YALLA_VERBOSE(1, "mca_pml_yalla_init");

    /* Set memory hooks */
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
         opal_mem_hooks_support_level()))
    {
        PML_YALLA_VERBOSE(1, "enabling on-demand memory mapping");
        opal_setenv("MXM_PML_MEM_ON_DEMAND_MAP", "y", false, &environ);
        ompi_pml_yalla.using_mem_hooks = 1;
    } else {
        PML_YALLA_VERBOSE(1, "disabling on-demand memory mapping");
        ompi_pml_yalla.using_mem_hooks = 0;
    }
    opal_setenv("MXM_PML_SINGLE_THREAD", ompi_mpi_thread_multiple ? "n" : "y",
                false, &environ);

    /* Read options */
    error = mxm_config_read_opts(&ctx_opts, &ep_opts, "PML", NULL, 0);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    error = mxm_init(ctx_opts, &ompi_pml_yalla.mxm_context);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    if (ompi_pml_yalla.using_mem_hooks) {
        opal_mem_hooks_register_release(mca_pml_yalla_mem_release_cb, NULL);
    }

    error = mxm_ep_create(ompi_pml_yalla.mxm_context, ep_opts, &ompi_pml_yalla.mxm_ep);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    mxm_config_free_context_opts(ctx_opts);
    mxm_config_free_ep_opts(ep_opts);

    rc = send_ep_address();
    if (rc < 0) {
        return rc;
    }

    OBJ_CONSTRUCT(&ompi_pml_yalla.send_reqs, mca_pml_yalla_freelist_t);
    OBJ_CONSTRUCT(&ompi_pml_yalla.bsend_reqs, mca_pml_yalla_freelist_t);
    OBJ_CONSTRUCT(&ompi_pml_yalla.recv_reqs, mca_pml_yalla_freelist_t);
    OBJ_CONSTRUCT(&ompi_pml_yalla.convs, mca_pml_yalla_freelist_t);

    opal_progress_register(mca_pml_yalla_progress);

    PML_YALLA_VERBOSE(2, "created mxm context %p ep %p", ompi_pml_yalla.mxm_context,
                      ompi_pml_yalla.mxm_ep);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_cleanup(void)
{
    PML_YALLA_VERBOSE(1, "mca_pml_yalla_cleanup");

    opal_progress_unregister(mca_pml_yalla_progress);

    OBJ_DESTRUCT(&ompi_pml_yalla.convs);
    OBJ_DESTRUCT(&ompi_pml_yalla.recv_reqs);
    OBJ_DESTRUCT(&ompi_pml_yalla.bsend_reqs);
    OBJ_DESTRUCT(&ompi_pml_yalla.send_reqs);

    if (ompi_pml_yalla.mxm_ep) {
        mxm_ep_destroy(ompi_pml_yalla.mxm_ep);
        ompi_pml_yalla.mxm_ep = NULL;
    }
    if (ompi_pml_yalla.using_mem_hooks) {
        opal_mem_hooks_unregister_release(mca_pml_yalla_mem_release_cb);
    }
    if (ompi_pml_yalla.mxm_context) {
        mxm_cleanup(ompi_pml_yalla.mxm_context);
        ompi_pml_yalla.mxm_context = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_pml_yalla_add_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    size_t i;
    int ret;
    void *address;
    mxm_conn_h conn;
    size_t addrlen;
    mxm_error_t error;

    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("yalla",
                                                              procs,
                                                              nprocs))) {
        return ret;
    }

    for (i = 0; i < nprocs; ++i) {
        ret = recv_ep_address(procs[i], &address, &addrlen);
        if (ret < 0) {
            return ret;
        }

        if (procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML]) {
            PML_YALLA_VERBOSE(3, "already connected to proc. %d", procs[i]->proc_name.vpid);
            continue;
        }

        PML_YALLA_VERBOSE(2, "connecting to proc. %d", procs[i]->proc_name.vpid);
        error = mxm_ep_connect(ompi_pml_yalla.mxm_ep, address, &conn);
        free(address);

        if (MXM_OK != error) {
            PML_YALLA_ERROR("Failed to connect");
            return OMPI_ERROR;
        }

        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = conn;
    }

    return OMPI_SUCCESS;
}

int mca_pml_yalla_del_procs(struct ompi_proc_t **procs, size_t nprocs)
{
    size_t i;

    if (ompi_mpi_finalized) {
        PML_YALLA_VERBOSE(3, "using bulk powerdown");
        mxm_ep_powerdown(ompi_pml_yalla.mxm_ep);
    }

    for (i = 0; i < nprocs; ++i) {
        mxm_ep_disconnect(procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML]);
        PML_YALLA_VERBOSE(2, "disconnected from rank %d", procs[i]->proc_name.vpid);
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML] = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_pml_yalla_enable(bool enable)
{
    mca_pml_yalla_init_reqs();
    mca_pml_yalla_init_datatype();
    return OMPI_SUCCESS;
}

int mca_pml_yalla_progress(void)
{
    mxm_progress(ompi_pml_yalla.mxm_context);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_add_comm(struct ompi_communicator_t* comm)
{
    mxm_error_t error;
    mxm_mq_h mq;

    error = mxm_mq_create(ompi_pml_yalla.mxm_context, comm->c_contextid, &mq);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    comm->c_pml_comm = (void*)mq;
    PML_YALLA_VERBOSE(2, "created mq ctxid %d for comm %s", comm->c_contextid,
                      comm->c_name);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_del_comm(struct ompi_communicator_t* comm)
{
    mxm_mq_h mq = (void*)comm->c_pml_comm;

    PML_YALLA_VERBOSE(2, "destroying mq ctxid %d of comm %s", comm->c_contextid,
                      comm->c_name);
    mxm_mq_destroy(mq);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int src, int tag, struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{
    mca_pml_yalla_recv_request_t *rreq;

    rreq = MCA_PML_YALLA_RREQ_INIT(buf, count, datatype, src, tag, comm,
                                   OMPI_REQUEST_INACTIVE);
    rreq->super.ompi.req_persistent = true;
    rreq->super.flags = 0;
    *request = &rreq->super.ompi;
    PML_YALLA_VERBOSE(9, "init recv request %p src %d tag %d comm %s", *request,
                      src, tag, comm->c_name);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        int src, int tag, struct ompi_communicator_t* comm,
                        struct ompi_request_t **request)
{
    mca_pml_yalla_recv_request_t *rreq;
    mxm_error_t error;

    rreq = MCA_PML_YALLA_RREQ_INIT(buf, count, datatype, src, tag, comm,
                                   OMPI_REQUEST_ACTIVE);
    rreq->super.ompi.req_persistent = false;
    rreq->super.flags = 0;

    PML_YALLA_VERBOSE(8, "receive request *%p=%p from %d tag %d dtype %s count %Zu",
                      request, rreq, src, tag, datatype->name, count);

    error = mxm_req_recv(&rreq->mxm);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    *request = &rreq->super.ompi;
    return OMPI_SUCCESS;
}

int mca_pml_yalla_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                       int tag, struct ompi_communicator_t* comm,
                       ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_RECV_REQ(&rreq, buf, count, datatype, src, tag, comm, recv);
    PML_YALLA_INIT_BLOCKING_MXM_RECV_REQ(&rreq);

    PML_YALLA_VERBOSE(8, "receive from %d tag %d dtype %s count %Zu", src, tag,
                      datatype->name, count);

    error = mxm_req_recv(&rreq);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    PML_YALLA_WAIT_MXM_REQ(&rreq.base);
    PML_YALLA_VERBOSE(8, "receive completed with status %s source %d rtag %d(%d/0x%x) len %Zu",
                      mxm_error_string(rreq.base.error),
                      rreq.completion.sender_imm, rreq.completion.sender_tag,
                      rreq.tag, rreq.tag_mask,
                      rreq.completion.actual_len);
    PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.actual_len, status);
    PML_YALLA_FREE_BLOCKING_MXM_REQ(&rreq.base);

    return OMPI_SUCCESS;
}

int mca_pml_yalla_isend_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int dst, int tag, mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{
    mca_pml_yalla_send_request_t *sreq;

    sreq = MCA_PML_YALLA_SREQ_INIT(buf, count, datatype, dst, tag, mode, comm,
                                   OMPI_REQUEST_INACTIVE);
    sreq->super.ompi.req_persistent = true;
    sreq->super.flags = MCA_PML_YALLA_REQUEST_FLAG_SEND;
    if (mode == MCA_PML_BASE_SEND_BUFFERED) {
        sreq->super.flags |= MCA_PML_YALLA_REQUEST_FLAG_BSEND;
    }

    *request = &sreq->super.ompi;
    PML_YALLA_VERBOSE(9, "init send request %p dst %d tag %d comm %s", *request,
                      dst, tag, comm->c_name);
    return OMPI_SUCCESS;
}

static int mca_pml_yalla_bsend(mxm_send_req_t *mxm_sreq)
{
    mca_pml_yalla_bsend_request_t *bsreq = PML_YALLA_FREELIST_GET(&ompi_pml_yalla.bsend_reqs);
    mxm_error_t error;
    size_t length;

    /* Create a new send request using MPI internal buffer */

    bsreq->mxm.base.state     = mxm_sreq->base.state;
    bsreq->mxm.base.mq        = mxm_sreq->base.mq;
    bsreq->mxm.base.conn      = mxm_sreq->base.conn;

    bsreq->mxm.base.data_type = MXM_REQ_DATA_BUFFER;
    switch (mxm_sreq->base.data_type) {
    case MXM_REQ_DATA_BUFFER:
        length = mxm_sreq->base.data.buffer.length;
        bsreq->mxm.base.data.buffer.ptr = mca_pml_base_bsend_request_alloc_buf(length);
        bsreq->mxm.base.data.buffer.length = length;
        memcpy(bsreq->mxm.base.data.buffer.ptr, mxm_sreq->base.data.buffer.ptr, length);
        break;
    case MXM_REQ_DATA_STREAM:
        length = mxm_sreq->base.data.stream.length;
        bsreq->mxm.base.data.buffer.ptr = mca_pml_base_bsend_request_alloc_buf(length);
        bsreq->mxm.base.data.buffer.length = length;
        mxm_sreq->base.data.stream.cb(bsreq->mxm.base.data.buffer.ptr, length,
                                      0, mxm_sreq->base.context);
        break;
    default:
        return OMPI_ERROR;
    }

    bsreq->mxm.opcode         = mxm_sreq->opcode;
    bsreq->mxm.flags          = mxm_sreq->flags;
    bsreq->mxm.op.send        = mxm_sreq->op.send;

    error = mxm_req_send(&bsreq->mxm);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    /* Make the completion handler believe it's ok to release the original request */
    mxm_sreq->base.state = MXM_REQ_COMPLETED;

    return OMPI_SUCCESS;
}

int mca_pml_yalla_isend(void *buf, size_t count, ompi_datatype_t *datatype,
                        int dst, int tag, mca_pml_base_send_mode_t mode,
                        struct ompi_communicator_t* comm,
                        struct ompi_request_t **request)
{
    mca_pml_yalla_send_request_t *sreq;
    mxm_error_t error;
    int rc;

    sreq = MCA_PML_YALLA_SREQ_INIT(buf, count, datatype, dst, tag, mode, comm,
                                   OMPI_REQUEST_ACTIVE);
    sreq->super.ompi.req_persistent = false;
    sreq->super.flags = 0;

    PML_YALLA_VERBOSE(8, "send request *%p=%p to %d mode %d tag %d dtype %s count %Zu",
                      request, sreq, dst, mode, tag, datatype->name, count);

    if (mode == MCA_PML_BASE_SEND_BUFFERED) {
        rc = mca_pml_yalla_bsend(&sreq->mxm);
        ompi_request_complete(&sreq->super.ompi, true);
        *request = &sreq->super.ompi;
        return rc;
    }

    error = mxm_req_send(&sreq->mxm);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    *request = &sreq->super.ompi;
    return OMPI_SUCCESS;
}

int mca_pml_yalla_send(void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                       int tag, mca_pml_base_send_mode_t mode,
                       struct ompi_communicator_t* comm)
{
    mxm_send_req_t sreq;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_SEND_REQ(&sreq, buf, count, datatype, dst, tag, mode, comm, send);
    PML_YALLA_INIT_BLOCKING_MXM_SEND_REQ(&sreq);

    PML_YALLA_VERBOSE(8, "send to %d tag %d dtype %s count %Zu", dst, tag,
                      datatype->name, count);

    if (mode == MCA_PML_BASE_SEND_BUFFERED) {
        return mca_pml_yalla_bsend(&sreq);
    }

    error = mxm_req_send(&sreq);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    PML_YALLA_WAIT_MXM_REQ(&sreq.base);
    if (MXM_OK != sreq.base.error) {
        return OMPI_ERROR;
    }

    PML_YALLA_FREE_BLOCKING_MXM_REQ(&sreq.base);

    return OMPI_SUCCESS;
}

int mca_pml_yalla_iprobe(int src, int tag, struct ompi_communicator_t* comm,
                         int *matched, ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_PROBE_REQ(&rreq, src, tag, comm);

    error = mxm_req_probe(&rreq);
    switch (error) {
    case MXM_OK:
        *matched = 1;
        PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.sender_len, status);
        return OMPI_SUCCESS;
    case MXM_ERR_NO_MESSAGE:
        *matched = 0;
        return OMPI_SUCCESS;
    default:
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_pml_yalla_probe(int src, int tag, struct ompi_communicator_t* comm,
                        ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_PROBE_REQ(&rreq, src, tag, comm);
    for (;;) {
        error = mxm_req_probe(&rreq);
        switch (error) {
        case MXM_OK:
            PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.sender_len, status);
            return OMPI_SUCCESS;
        case MXM_ERR_NO_MESSAGE:
            continue;
        default:
            return OMPI_ERROR;
        }

        opal_progress();
    }
}

int mca_pml_yalla_improbe(int src, int tag, struct ompi_communicator_t* comm,
                          int *matched, struct ompi_message_t **message,
                          ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_message_h mxm_msg;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_PROBE_REQ(&rreq, src, tag, comm);

    error = mxm_req_mprobe(&rreq, &mxm_msg);
    switch (error) {
    case MXM_OK:
        *matched = 1;
        PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.sender_len, status);
        PML_YALLA_SET_MESSAGE(&rreq, comm, mxm_msg, message);
        return OMPI_SUCCESS;
    case MXM_ERR_NO_MESSAGE:
        *matched = 0;
        return OMPI_SUCCESS;
    default:
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_pml_yalla_mprobe(int src, int tag, struct ompi_communicator_t* comm,
                         struct ompi_message_t **message,
                         ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_message_h mxm_msg;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_PROBE_REQ(&rreq, src, tag, comm);
    for (;;) {
        error = mxm_req_mprobe(&rreq, &mxm_msg);
        switch (error) {
        case MXM_OK:
            PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.sender_len, status);
            PML_YALLA_SET_MESSAGE(&rreq, comm, mxm_msg, message);
            return OMPI_SUCCESS;
        case MXM_ERR_NO_MESSAGE:
            continue;
        default:
            return OMPI_ERROR;
        }

        opal_progress();
    }
    return OMPI_SUCCESS;
}

int mca_pml_yalla_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                         struct ompi_message_t **message,
                         struct ompi_request_t **request)
{
    mca_pml_yalla_recv_request_t *rreq;
    mxm_error_t error;

    rreq = MCA_PML_YALLA_RREQ_INIT(buf, count, datatype, -1, 0, (*message)->comm,
                                   OMPI_REQUEST_ACTIVE);
    rreq->super.ompi.req_persistent = false;
    rreq->super.flags = 0;

    PML_YALLA_VERBOSE(8, "receive request *%p=%p message *%p=%p dtype %s count %Zu",
                      request, rreq, message, *message, datatype->name, count);

    error = mxm_message_recv(&rreq->mxm, (*message)->req_ptr);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    PML_YALLA_MESSAGE_RELEASE(message);

    *request = &rreq->super.ompi;
    return OMPI_SUCCESS;
}

int mca_pml_yalla_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message,
                        ompi_status_public_t* status)
{
    mxm_recv_req_t rreq;
    mxm_error_t error;

    PML_YALLA_INIT_MXM_RECV_REQ(&rreq, buf, count, datatype, -1, 0, (*message)->comm, recv);
    PML_YALLA_INIT_BLOCKING_MXM_RECV_REQ(&rreq);

    PML_YALLA_VERBOSE(8, "receive message *%p=%p dtype %s count %Zu", message,
                      *message, datatype->name, count);

    error = mxm_message_recv(&rreq, (*message)->req_ptr);
    if (MXM_OK != error) {
        return OMPI_ERROR;
    }

    PML_YALLA_MESSAGE_RELEASE(message);

    PML_YALLA_WAIT_MXM_REQ(&rreq.base);
    PML_YALLA_VERBOSE(8, "receive completed with status %s source %d rtag %d(%d/0x%x) len %Zu",
                      mxm_error_string(rreq.base.error),
                      rreq.completion.sender_imm, rreq.completion.sender_tag,
                      rreq.tag, rreq.tag_mask,
                      rreq.completion.actual_len);
    PML_YALLA_SET_RECV_STATUS(&rreq, rreq.completion.actual_len, status);
    return OMPI_SUCCESS;
}

int mca_pml_yalla_start(size_t count, ompi_request_t** requests)
{
    mca_pml_yalla_base_request_t *req;
    mca_pml_yalla_send_request_t *sreq;
    mxm_error_t error;
    size_t i;
    int rc;

    for (i = 0; i < count; ++i) {
        req = (mca_pml_yalla_base_request_t *)requests[i];

        if ((req == NULL) || (OMPI_REQUEST_PML != req->ompi.req_type)) {
            /* Skip irrelevant requests */
            continue;
        }

        PML_YALLA_ASSERT(req->ompi.req_state != OMPI_REQUEST_INVALID);
        PML_YALLA_RESET_OMPI_REQ(&req->ompi, OMPI_REQUEST_ACTIVE);
        PML_YALLA_RESET_PML_REQ(req);

        if (req->flags & MCA_PML_YALLA_REQUEST_FLAG_SEND) {
            sreq = (mca_pml_yalla_send_request_t *)req;
            if (req->flags & MCA_PML_YALLA_REQUEST_FLAG_BSEND) {
                PML_YALLA_VERBOSE(8, "start bsend request %p", sreq);
                rc = mca_pml_yalla_bsend(&sreq->mxm);
                ompi_request_complete(&sreq->super.ompi, true);
                if (OMPI_SUCCESS != rc) {
                    return rc;
                }
            } else {
                PML_YALLA_VERBOSE(8, "start send request %p", sreq);
                error = mxm_req_send(&sreq->mxm);
                if (MXM_OK != error) {
                    return OMPI_ERROR;
                }
            }
        } else {
            PML_YALLA_VERBOSE(8, "start recv request %p", req);
            error = mxm_req_recv(&((mca_pml_yalla_recv_request_t *)req)->mxm);
            if (MXM_OK != error) {
                return OMPI_ERROR;
            }
        }
    }
    return OMPI_SUCCESS;
}

int mca_pml_yalla_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_SUCCESS;
}
