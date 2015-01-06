/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_ob1.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "ompi/peruse/peruse-internal.h"
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif  /* HAVE_ALLOCA_H */

int mca_pml_ob1_isend_init(void *buf,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int dst,
                           int tag,
                           mca_pml_base_send_mode_t sendmode,
                           ompi_communicator_t * comm,
                           ompi_request_t ** request)
{
    mca_pml_ob1_send_request_t *sendreq = NULL;
    MCA_PML_OB1_SEND_REQUEST_ALLOC(comm, dst, sendreq);
    if (NULL == sendreq)
        return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, true);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &(sendreq)->req_send.req_base,
                             PERUSE_SEND);

    *request = (ompi_request_t *) sendreq;
    return OMPI_SUCCESS;
}

/* try to get a small message out on to the wire quickly */
static inline int mca_pml_ob1_send_inline (void *buf, size_t count,
                                           ompi_datatype_t * datatype,
                                           int dst, int tag, int16_t seqn,
                                           ompi_proc_t *dst_proc, mca_bml_base_endpoint_t* endpoint,
                                           ompi_communicator_t * comm)
{
    mca_pml_ob1_match_hdr_t match;
    mca_bml_base_btl_t *bml_btl;
    opal_convertor_t convertor;
    size_t size;
    int rc;

    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    if( NULL == bml_btl->btl->btl_sendi)
        return OMPI_ERR_NOT_AVAILABLE;

    ompi_datatype_type_size (datatype, &size);
    if ((size * count) > 256) {  /* some random number */
        return OMPI_ERR_NOT_AVAILABLE;
    }

    if (count > 0) {
        /* initialize just enough of the convertor to avoid a SEGV in opal_convertor_cleanup */
        OBJ_CONSTRUCT(&convertor, opal_convertor_t);

        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the datatype.   */
        opal_convertor_copy_and_prepare_for_send (dst_proc->super.proc_convertor,
                                                  (const struct opal_datatype_t *) datatype,
                                                  count, buf, 0, &convertor);
        opal_convertor_get_packed_size (&convertor, &size);
    } else {
        size = 0;
    }

    mca_pml_ob1_match_hdr_prepare (&match, MCA_PML_OB1_HDR_TYPE_MATCH, 0,
                                   comm->c_contextid, comm->c_my_rank,
                                   tag, seqn);

    ob1_hdr_hton(&match, MCA_PML_OB1_HDR_TYPE_MATCH, dst_proc);

    /* try to send immediately */
    rc = mca_bml_base_sendi (bml_btl, &convertor, &match, OMPI_PML_OB1_MATCH_HDR_LEN,
                             size, MCA_BTL_NO_ORDER, MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                             MCA_PML_OB1_HDR_TYPE_MATCH, NULL);
    if (count > 0) {
        opal_convertor_cleanup (&convertor);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
	return rc;
    }

    return (int) size;
}

int mca_pml_ob1_isend(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      ompi_communicator_t * comm,
                      ompi_request_t ** request)
{
    mca_pml_ob1_comm_t* ob1_comm = comm->c_pml_comm;
    mca_pml_ob1_send_request_t *sendreq = NULL;
    ompi_proc_t *dst_proc = ompi_comm_peer_lookup (comm, dst);
    mca_bml_base_endpoint_t* endpoint = (mca_bml_base_endpoint_t*)
                                        dst_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    int16_t seqn;
    int rc;

    seqn = (uint16_t) OPAL_THREAD_ADD32(&ob1_comm->procs[dst].send_sequence, 1);

    if (MCA_PML_BASE_SEND_SYNCHRONOUS != sendmode) {
        rc = mca_pml_ob1_send_inline (buf, count, datatype, dst, tag, seqn, dst_proc,
                                      endpoint, comm);
        if (OPAL_LIKELY(0 <= rc)) {
            /* NTH: it is legal to return ompi_request_empty since the only valid
             * field in a send completion status is whether or not the send was
             * cancelled (which it can't be at this point anyway). */
            *request = &ompi_request_empty;
            return OMPI_SUCCESS;
        }
    }

    MCA_PML_OB1_SEND_REQUEST_ALLOC(comm, dst, sendreq);
    if (NULL == sendreq)
        return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &(sendreq)->req_send.req_base,
                             PERUSE_SEND);

    MCA_PML_OB1_SEND_REQUEST_START_W_SEQ(sendreq, endpoint, seqn, rc);
    *request = (ompi_request_t *) sendreq;
    return rc;
}

int mca_pml_ob1_send(void *buf,
                     size_t count,
                     ompi_datatype_t * datatype,
                     int dst,
                     int tag,
                     mca_pml_base_send_mode_t sendmode,
                     ompi_communicator_t * comm)
{
    mca_pml_ob1_comm_t* ob1_comm = comm->c_pml_comm;
    ompi_proc_t *dst_proc = ompi_comm_peer_lookup (comm, dst);
    mca_bml_base_endpoint_t* endpoint = (mca_bml_base_endpoint_t*)
                                        dst_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    mca_pml_ob1_send_request_t *sendreq =
        alloca(mca_pml_base_send_requests.fl_frag_size);
    int16_t seqn;
    int rc;

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_BUFFERED == sendmode)) {
        /* large buffered sends *need* a real request so use isend instead */
        ompi_request_t *brequest;

        rc = mca_pml_ob1_isend (buf, count, datatype, dst, tag, sendmode, comm, &brequest);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            return rc;
        }

        /* free the request and return. don't care if it completes now */
        ompi_request_free (&brequest);
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return OMPI_ERR_UNREACH;
    }

    seqn = (uint16_t) OPAL_THREAD_ADD32(&ob1_comm->procs[dst].send_sequence, 1);

    if (MCA_PML_BASE_SEND_SYNCHRONOUS != sendmode) {
        rc = mca_pml_ob1_send_inline (buf, count, datatype, dst, tag, seqn, dst_proc,
                                      endpoint, comm);
        if (OPAL_LIKELY(0 <= rc)) {
            return OMPI_SUCCESS;
        }
    }

    OBJ_CONSTRUCT(sendreq, mca_pml_ob1_send_request_t);
    sendreq->req_send.req_base.req_proc = dst_proc;
    sendreq->rdma_frag = NULL;

    MCA_PML_OB1_SEND_REQUEST_INIT(sendreq,
                                  buf,
                                  count,
                                  datatype,
                                  dst, tag,
                                  comm, sendmode, false);

    PERUSE_TRACE_COMM_EVENT (PERUSE_COMM_REQ_ACTIVATE,
                             &sendreq->req_send.req_base,
                             PERUSE_SEND);

    MCA_PML_OB1_SEND_REQUEST_START_W_SEQ(sendreq, endpoint, seqn, rc);
    if (OPAL_LIKELY(rc == OMPI_SUCCESS)) {
        ompi_request_wait_completion(&sendreq->req_send.req_base.req_ompi);

        rc = sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR;
        MCA_PML_BASE_SEND_REQUEST_FINI(&sendreq->req_send);
    }
    OBJ_DESTRUCT(sendreq);

    return rc;
}
