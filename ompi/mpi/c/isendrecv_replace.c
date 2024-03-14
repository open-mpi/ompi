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
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
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
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/proc/proc.h"
#include "ompi/memchecker.h"
#include "ompi/runtime/ompi_spc.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Isendrecv_replace = PMPI_Isendrecv_replace
#endif
#define MPI_Isendrecv_replace PMPI_Isendrecv_replace
#endif

static const char FUNC_NAME[] = "MPI_Isendrecv_replace";

struct ompi_isendrecv_replace_context_t {
    opal_object_t super;
    opal_convertor_t convertor;
    size_t packed_size;
    unsigned char packed_data[2048];
    struct iovec iov;
    int nreqs;
    int source;
    ompi_request_t *subreq[2];
};

typedef struct ompi_isendrecv_replace_context_t ompi_isendrecv_replace_context_t;

#if OMPI_BUILD_MPI_PROFILING
static void ompi_isendrecv_context_constructor(ompi_isendrecv_replace_context_t *context)
{
    context->packed_size = 0;
    OBJ_CONSTRUCT(&context->convertor, opal_convertor_t);
}

static void ompi_isendrecv_context_destructor(ompi_isendrecv_replace_context_t *context)
{
    if (context->packed_size > sizeof(context->packed_data)) {
        PMPI_Free_mem(context->iov.iov_base);
    }
    OBJ_DESTRUCT(&context->convertor);
}

OBJ_CLASS_INSTANCE(ompi_isendrecv_replace_context_t, 
                   opal_object_t, 
                   ompi_isendrecv_context_constructor,
                   ompi_isendrecv_context_destructor);
#else
OBJ_CLASS_DECLARATION(ompi_isendrecv_replace_context_t);
#endif /* OMPI_BUILD_MPI_PROFILING */

static int ompi_isendrecv_replace_complete_func (ompi_comm_request_t *request)
{
    ompi_isendrecv_replace_context_t *context =
        (ompi_isendrecv_replace_context_t *) request->context;

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


int MPI_Isendrecv_replace(void * buf, int count, MPI_Datatype datatype,
                         int dest, int sendtag, int source, int recvtag,
                         MPI_Comm comm, MPI_Request *request)

{
    int rc = MPI_SUCCESS;
    size_t max_data;
    uint32_t iov_count;
    ompi_comm_request_t *crequest = NULL;
    ompi_isendrecv_replace_context_t *context = NULL;
    int nreqs = 0;
    uint32_t flags;

    SPC_RECORD(OMPI_SPC_ISENDRECV_REPLACE, 1);

    MEMCHECKER(
               memchecker_datatype(datatype);
               memchecker_call(&opal_memchecker_base_isdefined, buf, count, datatype);
               memchecker_comm(comm);
               );

    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, datatype, count);

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

    /* simple case */
    if ( source == MPI_PROC_NULL || dest == MPI_PROC_NULL || count == 0 ) {
        rc = PMPI_Isendrecv(buf, count, datatype, dest, sendtag, buf, count, datatype, source, recvtag, comm, request);
        return rc;
    }

    ompi_proc_t* proc = ompi_comm_peer_lookup(comm, dest);
    if(proc == NULL) {
        rc = MPI_ERR_RANK;
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }

    crequest = ompi_comm_request_get ();
    if (NULL == crequest) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = OBJ_NEW(ompi_isendrecv_replace_context_t);
    if (NULL == context) {
        ompi_comm_request_return (crequest);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->iov.iov_base = context->packed_data;
    context->iov.iov_len = sizeof(context->packed_data);

    crequest->context = &context->super;
    context->subreq[0] = NULL;
    context->subreq[1] = NULL;
    context->source = source;

    /* initialize convertor to unpack recv buffer */
    OBJ_CONSTRUCT(&context->convertor, opal_convertor_t);
    opal_convertor_copy_and_prepare_for_send( proc->super.proc_convertor, &(datatype->super),
                                              count, buf, 0, &context->convertor );

    /* setup a buffer for recv */
    opal_convertor_get_packed_size( &context->convertor, &context->packed_size );
    if( context->packed_size > sizeof(context->packed_data) ) {
        rc = PMPI_Alloc_mem(context->packed_size, MPI_INFO_NULL, &context->iov.iov_base);
        if(OMPI_SUCCESS != rc) {
            OBJ_RELEASE(context);
            ompi_comm_request_return (crequest);
            OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
        }
        context->iov.iov_len = context->packed_size;
    }
    max_data = context->packed_size;
    iov_count = 1;
    rc = opal_convertor_pack(&context->convertor, &context->iov, &iov_count, &max_data);
    if ( 0 > rc ) {
        OBJ_RELEASE(context);
        ompi_comm_request_return (crequest);
        rc = MPI_ERR_UNKNOWN;
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }

    if (source != MPI_PROC_NULL) { /* post recv */
        rc = MCA_PML_CALL(irecv(buf, count, datatype,
                                source, recvtag, comm, &context->subreq[nreqs++]));
        if (MPI_SUCCESS != rc) {
            OBJ_RELEASE(context);
            ompi_comm_request_return (crequest);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    if (dest != MPI_PROC_NULL) { /* send */
        rc = MCA_PML_CALL(isend(context->iov.iov_base, context->packed_size, MPI_PACKED, dest,
                               sendtag, MCA_PML_BASE_SEND_STANDARD, comm, 
                                &context->subreq[nreqs++]));
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

    rc = ompi_comm_request_schedule_append_w_flags(crequest, 
                                                   ompi_isendrecv_replace_complete_func, 
                                                   context->subreq, 
                                                   nreqs,
                                                   flags);
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
