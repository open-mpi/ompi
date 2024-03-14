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
 * Copyright (c) 2021      Triad National Security, LLC. All rights
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Isendrecv = PMPI_Isendrecv
#endif
#define MPI_Isendrecv PMPI_Isendrecv
#endif

static const char FUNC_NAME[] = "MPI_Isendrecv";

struct ompi_isendrecv_context_t {
    opal_object_t super;
    int nreqs;
    int source;
    ompi_request_t *subreq[2];
};

typedef struct ompi_isendrecv_context_t ompi_isendrecv_context_t;
#if OMPI_BUILD_MPI_PROFILING
OBJ_CLASS_INSTANCE(ompi_isendrecv_context_t, opal_object_t, NULL, NULL);
#else
OBJ_CLASS_DECLARATION(ompi_isendrecv_context_t);
#endif  /* OMPI_BUILD_MPI_PROFILING */

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


int MPI_Isendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                 int dest, int sendtag, void *recvbuf, int recvcount,
                 MPI_Datatype recvtype, int source, int recvtag,
                 MPI_Comm comm,  MPI_Request *request)
{
    ompi_isendrecv_context_t *context = NULL;
    ompi_comm_request_t *crequest;
    int rc = MPI_SUCCESS;
    int nreqs = 0;
    uint32_t flags;

    SPC_RECORD(OMPI_SPC_ISENDRECV, 1);

    MEMCHECKER(
        memchecker_datatype(sendtype);
        memchecker_datatype(recvtype);
        memchecker_call(&opal_memchecker_base_isdefined, sendbuf, sendcount, sendtype);
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        OMPI_CHECK_DATATYPE_FOR_SEND(rc, sendtype, sendcount);
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, recvtype, recvcount);
        OMPI_CHECK_USER_BUFFER(rc, sendbuf, sendtype, sendcount);
        OMPI_CHECK_USER_BUFFER(rc, recvbuf, recvtype, recvcount);

        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM, FUNC_NAME);
        } else if (dest != MPI_PROC_NULL && ompi_comm_peer_invalid(comm, dest)) {
            rc = MPI_ERR_RANK;
        } else if (sendtag < 0 || sendtag > mca_pml.pml_max_tag) {
            rc = MPI_ERR_TAG;
        } else if (source != MPI_PROC_NULL && source != MPI_ANY_SOURCE && ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        } else if (((recvtag < 0) && (recvtag !=  MPI_ANY_TAG)) || (recvtag > mca_pml.pml_max_tag)) {
            rc = MPI_ERR_TAG;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }

        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

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
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    if (dest != MPI_PROC_NULL) { /* send */
        rc = MCA_PML_CALL(isend(sendbuf, sendcount, sendtype, dest,
                                sendtag, MCA_PML_BASE_SEND_STANDARD, comm, &context->subreq[nreqs++]));
        if (MPI_SUCCESS != rc) {
            OBJ_RELEASE(context);
            ompi_comm_request_return (crequest);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
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

    OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);

    /* kick off the request */

    ompi_comm_request_start (crequest);
    *request = &crequest->super;

    return rc;
}
