/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND. */
/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
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
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
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
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"
#include "ompi/runtime/ompi_spc.h"

int ompi_sendrecv(const void * sendbuf, size_t sendcount, MPI_Datatype sendtype, int dest, int sendtag, void * recvbuf, size_t recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Status *status)
{
    ompi_request_t* req = MPI_REQUEST_NULL;
    int rc = MPI_SUCCESS;
    int rcs = MPI_SUCCESS;

    if (source != MPI_PROC_NULL) { /* post recv */
        rc = MCA_PML_CALL(irecv(recvbuf, recvcount, recvtype,
                                source, recvtag, comm, &req));
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Sendrecv");
    }

    if (dest != MPI_PROC_NULL) { /* send */
        rc = MCA_PML_CALL(send(sendbuf, sendcount, sendtype, dest,
                               sendtag, MCA_PML_BASE_SEND_STANDARD, comm));
        if (OPAL_UNLIKELY(MPI_SUCCESS != rc)) {
            rcs = rc;
#if OPAL_ENABLE_FT_MPI
            /* If this is a PROC_FAILED error, we still need to proceed with
             * the receive, so that we do not propagate errors to the sender in
             * the case src != dst, and only dst is dead. In this case the
             * recv is guaranteed to complete (either in error if the source is
             * dead, or successfully if the source is live). */
            if (OPAL_UNLIKELY(MPI_ERR_PROC_FAILED != rc))
            /* if intentionally spills outside ifdef */
#endif
            ompi_request_cancel(req);
        }
    }

    if (source != MPI_PROC_NULL) { /* wait for recv */
        rc = ompi_request_wait(&req, status);
#if OPAL_ENABLE_FT_MPI
        /* Sendrecv never returns ERR_PROC_FAILED_PENDING because it is
         * blocking. Lets cancel that irecv to complete it NOW and promote
         * the error to ERR_PROC_FAILED */
        if( OPAL_UNLIKELY(MPI_ERR_PROC_FAILED_PENDING == rc) ) {
            ompi_request_cancel(req);
            ompi_request_wait(&req, MPI_STATUS_IGNORE);
            rc = MPI_ERR_PROC_FAILED;
        }
#endif
    } else {
        if (MPI_STATUS_IGNORE != status) {
            OMPI_COPY_STATUS(status, ompi_request_empty.req_status, false);
            /*
             * Per MPI-1, the MPI_ERROR field is not defined for single-completion calls
             */
            MEMCHECKER(
                opal_memchecker_base_mem_undefined(&status->MPI_ERROR, sizeof(int));
            );
        }
        rc = MPI_SUCCESS;
    }
    if( OPAL_UNLIKELY(MPI_SUCCESS != rcs && MPI_SUCCESS == rc) ) {
        rc = rcs;
    }
}
