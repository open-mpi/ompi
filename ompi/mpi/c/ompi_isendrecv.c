/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND. */
/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021-2024 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/communicator/comm_request.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"
#include "ompi/runtime/ompi_spc.h"


struct ompi_isendrecv_context_t {
    opal_object_t super;
    int nreqs;
    int source;
    ompi_request_t *subreq[2];
};

typedef struct ompi_isendrecv_context_t ompi_isendrecv_context_t;
OBJ_CLASS_INSTANCE(ompi_isendrecv_context_t, opal_object_t, NULL, NULL);

static int ompi_isendrecv_complete_func (ompi_comm_request_t *request)
{
    ompi_isendrecv_context_t *context =
        (ompi_isendrecv_context_t *) request->context;

    /*
     * Copy the status from the receive side of the sendrecv request?
     * But what if the send failed?
     *
     * Probably need to bring up in the MPI forum.  
     */

    if (MPI_PROC_NULL != context->source) {
        OMPI_COPY_STATUS(&request->super.req_status,
                         context->subreq[0]->req_status, false);
    } else {
        OMPI_COPY_STATUS(&request->super.req_status,
                         ompi_request_empty.req_status, false);
    }

    if(NULL != context->subreq[0]) {
        ompi_request_free(&context->subreq[0]);
    }
    if(NULL != context->subreq[1]) {
        ompi_request_free(&context->subreq[1]);
    }

    return OMPI_SUCCESS;
}

int ompi_isendrecv(const void * sendbuf, size_t sendcount, MPI_Datatype sendtype, int dest, int sendtag, void * recvbuf, size_t recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Request * request)
{
    ompi_isendrecv_context_t *context = NULL;
    ompi_comm_request_t *crequest;
    int rc = MPI_SUCCESS;
    int nreqs = 0;
    uint32_t flags;

    crequest = ompi_comm_request_get ();
    if (NULL == crequest) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = OBJ_NEW(ompi_isendrecv_context_t);
    if (NULL == context) {
        ompi_comm_request_return (crequest);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    crequest->context = &context->super;
    context->subreq[0] = MPI_REQUEST_NULL;
    context->subreq[1] = MPI_REQUEST_NULL;
    context->source = source;

    if (source != MPI_PROC_NULL) { /* post recv */
        rc = MCA_PML_CALL(irecv(recvbuf, recvcount, recvtype,
                                source, recvtag, comm, &context->subreq[nreqs++]));
        if (MPI_SUCCESS != rc) {
            OBJ_RELEASE(context);
            ompi_comm_request_return (crequest);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Isendrecv");
    }

    if (dest != MPI_PROC_NULL) { /* send */
        rc = MCA_PML_CALL(isend(sendbuf, sendcount, sendtype, dest,
                                sendtag, MCA_PML_BASE_SEND_STANDARD, comm, &context->subreq[nreqs++]));
        if (MPI_SUCCESS != rc) {
            OBJ_RELEASE(context);
            ompi_comm_request_return (crequest);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Isendrecv");
    }

    /*
     *  schedule the operation
     */

    context->nreqs = nreqs;
    assert(nreqs <= 2);

    flags = OMPI_COMM_REQ_FLAG_RETAIN_SUBREQ;

    rc = ompi_comm_request_schedule_append_w_flags(crequest, ompi_isendrecv_complete_func,
                                                   context->subreq, nreqs, flags);
    if (MPI_SUCCESS != rc) {
        OBJ_RELEASE(context);
        ompi_comm_request_return (crequest);
    }

    /* kick off the request */

    ompi_comm_request_start (crequest);
    *request = &crequest->super;

    return rc;
}
