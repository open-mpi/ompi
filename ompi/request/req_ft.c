/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/request/request.h"
#include "ompi/errhandler/errcode.h"

#include "ompi/runtime/params.h"

#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/pml.h"

#include "opal/mca/backtrace/backtrace.h"

/**************************
 * Support Routines
 **************************/
bool ompi_request_is_failed_fn(ompi_request_t *req)
{
    mca_pml_base_request_t* pml_req = NULL;

    if( !ompi_ftmpi_enabled ) {
        return false;
    }

    /* Pick our behavior depending upon the type of request */
    switch(req->req_type) {
    case OMPI_REQUEST_PML:
        pml_req = (mca_pml_base_request_t*)req;
        /*
         * Toggle 'off' the MPI_ANY_SOURCE MPI_ERR_PROC_FAILED_PENDING flag
         * We will recheck, but in the case the request is complete we
         * need to remove the error code.
         */
        if( MPI_ERR_PROC_FAILED_PENDING == req->req_status.MPI_ERROR ) {
            req->req_status.MPI_ERROR = MPI_SUCCESS;
        }
        break;
    case OMPI_REQUEST_COLL:
    case OMPI_REQUEST_COMM:
        /* Supported by bubbling up errors from REQUEST_PML subrequests
         * thus this type of requests never need to be interrupted */
        return false;
    default:
        /* not supported yet, pretend everything is always fine */
        return false;
    }

    /*
     * Sanity check
     */
    assert( NULL != req->req_mpi_object.comm );

    /*
     * If the request is complete, then just skip it
     */
    if( REQUEST_COMPLETE(req) ) {
        return false;
    }

    /*
     * Has this communicator been 'revoked'?
     *
     * If so unless we are in the FT part (propagate revoke, agreement or
     * shrink) this should fail.
     */
    if( OPAL_UNLIKELY(ompi_comm_is_revoked(req->req_mpi_object.comm) &&
                      req->req_type == OMPI_REQUEST_PML && !ompi_request_tag_is_ft(pml_req->req_tag)) ) {
        /* Do not set req->req_status.MPI_SOURCE */
        req->req_status.MPI_ERROR  = MPI_ERR_REVOKED;

        opal_output_verbose(10, ompi_ftmpi_output_handle,
                            "%s ompi_request_is_failed: %p (peer %d, tag %d) is on communicator %s(%s) that has been revoked!",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), (void*)req, pml_req->req_peer, pml_req->req_tag,
                            req->req_mpi_object.comm->c_name, ompi_comm_print_cid(req->req_mpi_object.comm));
        goto return_with_error;
    }

    /*
     * Collectives Check:
     * If the request is part of a collective, then the whole communicator
     * must be ok to continue. If not then return first failed process
     */
    if(OPAL_UNLIKELY( ompi_comm_coll_revoked(req->req_mpi_object.comm) &&
                      (req->req_type == OMPI_REQUEST_PML && ompi_request_tag_is_collective(pml_req->req_tag)) )) {
        /* Do not set req->req_status.MPI_SOURCE */
        req->req_status.MPI_ERROR  = MPI_ERR_PROC_FAILED;

        opal_output_verbose(10, ompi_ftmpi_output_handle,
                            "%s ompi_request_is_failed: Request %p (peer %d) is part of a collective (tag %d), and some process died. (mpi_source %3d)",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), (void*)req, pml_req->req_peer, pml_req->req_tag, req->req_status.MPI_SOURCE );
        goto return_with_error;
    }

    /* Corner-cases: two processes that can't fail (NULL and myself) */
    if((pml_req->req_peer == MPI_PROC_NULL) ||
       (OMPI_COMM_IS_INTRA(req->req_mpi_object.comm) && pml_req->req_peer == req->req_mpi_object.comm->c_local_group->grp_my_rank)) {
        return false;
    }

    /* If any_source but not FT related then the request is always marked for return */
    if( OPAL_UNLIKELY(MPI_ANY_SOURCE == pml_req->req_peer && !ompi_comm_is_any_source_enabled(req->req_mpi_object.comm)) ) {
        if( !ompi_request_tag_is_ft(pml_req->req_tag) ) {
            req->req_status.MPI_ERROR  = MPI_ERR_PROC_FAILED_PENDING;
            /* If it is a probe/mprobe, escalate the error */
            if( (MCA_PML_REQUEST_MPROBE == pml_req->req_type) ||
                (MCA_PML_REQUEST_PROBE == pml_req->req_type) ) {
                req->req_status.MPI_ERROR  = MPI_ERR_PROC_FAILED;
            }
            opal_output_verbose(10, ompi_ftmpi_output_handle,
                                "%s ompi_request_is_failed: Request %p (peer %d, tag %d) in comm %s(%s) peer ANY_SOURCE %s!",
                                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), (void*)req, pml_req->req_peer, pml_req->req_tag,
                                req->req_mpi_object.comm->c_name, ompi_comm_print_cid(req->req_mpi_object.comm),
                                ompi_mpi_errnum_get_string(req->req_status.MPI_ERROR));
            goto return_with_error;
        }
    }

    /* Any type of request with a dead process must be terminated with error */
    if( OPAL_UNLIKELY(!ompi_comm_is_proc_active(req->req_mpi_object.comm, pml_req->req_peer,
                                  OMPI_COMM_IS_INTER(req->req_mpi_object.comm))) ) {
        req->req_status.MPI_SOURCE = pml_req->req_peer;
        req->req_status.MPI_ERROR  = MPI_ERR_PROC_FAILED;
        assert(MPI_ANY_SOURCE != pml_req->req_peer); /* this case is handled above, so... */
        opal_output_verbose(10, ompi_ftmpi_output_handle,
                            "%s ompi_request_is_failed: Request %p (peer %d, tag %d) in comm %s(%s) mpi_source %3d failed - Ret %s",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), (void*)req, pml_req->req_peer, pml_req->req_tag,
                            req->req_mpi_object.comm->c_name, ompi_comm_print_cid(req->req_mpi_object.comm),
                            req->req_status.MPI_SOURCE,
                            ompi_mpi_errnum_get_string(req->req_status.MPI_ERROR));
        goto return_with_error;
    }

    OPAL_OUTPUT_VERBOSE((100, ompi_ftmpi_output_handle,
                         "%s ompi_request_is_failed: Request %p (peer %d tag %d) is still ok. (mpi_source %3d)",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), (void*)req, pml_req->req_peer, pml_req->req_tag, req->req_status.MPI_SOURCE ));

    return false;

 return_with_error:
    if( MPI_ERR_PROC_FAILED_PENDING != req->req_status.MPI_ERROR ) {
        int cancelled = req->req_status._cancelled;
#if OPAL_ENABLE_DEBUG
        int verbose = opal_output_get_verbosity(ompi_ftmpi_output_handle);
        if( verbose > 90 ) {
            opal_backtrace_print(stderr, NULL, 1);
        }
        if( verbose > 50 ) {
            MCA_PML_CALL(dump(req->req_mpi_object.comm, ompi_ftmpi_output_handle));
        }
#endif /* OPAL_ENABLE_DEBUG */
        /* Cancel and force completion immmediately
         * However, for Revoked and Collective error we can't complete
         * with an error before the buffer is unpinned (i.e. the request gets
         * wire cancelled).
         */
        ompi_request_cancel(req);
        req->req_status._cancelled = cancelled; /* This request is not user cancelled here, it is completed in error */
        return REQUEST_COMPLETE(req); /* If this request is not complete yet, it is stil ok and needs more spinning */
    }
    return (MPI_SUCCESS != req->req_status.MPI_ERROR);
}
