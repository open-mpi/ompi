/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_yalla_datatype.h"
#include "pml_yalla_request.h"


static mca_pml_yalla_convertor_t *mca_pml_yalla_get_send_convertor(void *buf, size_t count,
                                                                   ompi_datatype_t *datatype)
{
    mca_pml_yalla_convertor_t *convertor = (mca_pml_yalla_convertor_t *)PML_YALLA_FREELIST_GET(&ompi_pml_yalla.convs);

    convertor->datatype = datatype;
    OBJ_RETAIN(datatype);
    opal_convertor_copy_and_prepare_for_send(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buf, 0,
                                             &convertor->convertor);
    return convertor;
}

static mca_pml_yalla_convertor_t *mca_pml_yalla_get_recv_convertor(void *buf, size_t count,
                                                                   ompi_datatype_t *datatype)
{
    mca_pml_yalla_convertor_t *convertor = (mca_pml_yalla_convertor_t *)PML_YALLA_FREELIST_GET(&ompi_pml_yalla.convs);

    convertor->datatype = datatype;
    OBJ_RETAIN(datatype);
    opal_convertor_copy_and_prepare_for_recv(ompi_proc_local_proc->super.proc_convertor,
                                             &datatype->super, count, buf, 0,
                                             &convertor->convertor);
    return convertor;
}

static void mca_pml_yalla_noncontig_req_init(mxm_req_base_t *mxm_req,
                                             mca_pml_yalla_convertor_t *convertor,
                                             mxm_stream_cb_t stream_cb)
{
    mxm_req->data_type      = MXM_REQ_DATA_STREAM;
    mxm_req->data.stream.cb = stream_cb;
    opal_convertor_get_packed_size(&convertor->convertor, &mxm_req->data.stream.length);
}

static size_t mca_pml_yalla_stream_unpack(void *buffer, size_t length, size_t offset,
                                          opal_convertor_t *convertor)
{
    uint32_t iov_count;
    struct iovec iov;

    iov_count    = 1;
    iov.iov_base = buffer;
    iov.iov_len  = length;

    opal_convertor_set_position(convertor, &offset);
    opal_convertor_unpack(convertor, &iov, &iov_count, &length);
    return length;
}

static size_t mca_pml_yalla_stream_pack(void *buffer, size_t length, size_t offset,
                                        opal_convertor_t *convertor)
{
    uint32_t iov_count;
    struct iovec iov;

    iov_count    = 1;
    iov.iov_base = buffer;
    iov.iov_len  = length;

    opal_convertor_set_position(convertor, &offset);
    opal_convertor_pack(convertor, &iov, &iov_count, &length);
    return length;
}

static size_t mxm_pml_yalla_irecv_stream_cb(void *buffer, size_t length,
                                            size_t offset, void *context)
{
    mca_pml_yalla_base_request_t *req = context;
    return mca_pml_yalla_stream_unpack(buffer, length, offset, &req->convertor->convertor);
}

static size_t mxm_pml_yalla_recv_stream_cb(void *buffer, size_t length,
                                           size_t offset, void *context)
{
    mca_pml_yalla_convertor_t *convertor = context;
    return mca_pml_yalla_stream_unpack(buffer, length, offset, &convertor->convertor);
}

static size_t mxm_pml_yalla_isend_stream_cb(void *buffer, size_t length,
                                            size_t offset, void *context)
{
    mca_pml_yalla_base_request_t *req = context;
    return mca_pml_yalla_stream_pack(buffer, length, offset, &req->convertor->convertor);
}

static size_t mxm_pml_yalla_send_stream_cb(void *buffer, size_t length,
                                           size_t offset, void *context)
{
    mca_pml_yalla_convertor_t *convertor = context;
    return mca_pml_yalla_stream_pack(buffer, length, offset, &convertor->convertor);
}

void mca_pml_yalla_set_noncontig_data_irecv(mxm_req_base_t *mxm_req, void *buf,
                                            size_t count, ompi_datatype_t *datatype,
                                            mca_pml_yalla_recv_request_t *rreq)
{
    rreq->super.convertor = mca_pml_yalla_get_recv_convertor(buf, count, datatype);
    mca_pml_yalla_noncontig_req_init(mxm_req, rreq->super.convertor, mxm_pml_yalla_irecv_stream_cb);
}

void mca_pml_yalla_set_noncontig_data_recv(mxm_req_base_t *mxm_req, void *buf,
                                           size_t count, ompi_datatype_t *datatype)
{
    mca_pml_yalla_convertor_t *convertor;

    convertor = mca_pml_yalla_get_recv_convertor(buf, count, datatype);
    mca_pml_yalla_noncontig_req_init(mxm_req, convertor, mxm_pml_yalla_recv_stream_cb);
    mxm_req->context = convertor;
}

void mca_pml_yalla_set_noncontig_data_isend(mxm_req_base_t *mxm_req, void *buf,
                                            size_t count, ompi_datatype_t *datatype,
                                            mca_pml_yalla_send_request_t *sreq)
{
    sreq->super.convertor = mca_pml_yalla_get_send_convertor(buf, count, datatype);
    mca_pml_yalla_noncontig_req_init(mxm_req, sreq->super.convertor, mxm_pml_yalla_isend_stream_cb);
}

void mca_pml_yalla_set_noncontig_data_send(mxm_req_base_t *mxm_req, void *buf,
                                           size_t count, ompi_datatype_t *datatype)
{
    mca_pml_yalla_convertor_t *convertor;

    convertor = mca_pml_yalla_get_send_convertor(buf, count, datatype);
    mca_pml_yalla_noncontig_req_init(mxm_req, convertor, mxm_pml_yalla_send_stream_cb);
    mxm_req->context = convertor;
}

static void mca_pml_yalla_convertor_construct(mca_pml_yalla_convertor_t *convertor)
{
    OBJ_CONSTRUCT(&convertor->convertor, opal_convertor_t);
}

static void mca_pml_yalla_convertor_destruct(mca_pml_yalla_convertor_t *convertor)
{
    OBJ_DESTRUCT(&convertor->convertor);
}

void mca_pml_yalla_init_datatype(void)
{
    PML_YALLA_FREELIST_INIT(&ompi_pml_yalla.convs, mca_pml_yalla_convertor_t,
                         128, -1, 128);
}

OBJ_CLASS_INSTANCE(mca_pml_yalla_convertor_t,
                   opal_free_list_item_t,
                   mca_pml_yalla_convertor_construct,
                   mca_pml_yalla_convertor_destruct);

