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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"
#include "ompi/runtime/ompi_spc.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Waitany = PMPI_Waitany
#endif
#define MPI_Waitany PMPI_Waitany
#endif

static const char FUNC_NAME[] = "MPI_Waitany";


int MPI_Waitany(int count, MPI_Request requests[], int *indx, MPI_Status *status)
{
    SPC_RECORD(OMPI_SPC_WAITANY, 1);

    MEMCHECKER(
        int j;
        for (j = 0; j < count; j++){
            memchecker_request(&requests[j]);
        }
    );

    if ( MPI_PARAM_CHECK ) {
        int i, rc = MPI_SUCCESS;
        MPI_Request check_req = NULL;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((NULL == requests) && (0 != count)) {
            rc = MPI_ERR_REQUEST;
        } else {
            for (i = 0; i < count; i++) {
                if (NULL == requests[i]) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
                if (requests[i] == &ompi_request_empty) {
                    continue;
                } else if (NULL == requests[i]->req_mpi_object.comm) {
                    continue;
                } else if (NULL == check_req) {
                    check_req = requests[i];
                }
                else {
                    if (!ompi_comm_instances_same(requests[i]->req_mpi_object.comm,
                                                     check_req->req_mpi_object.comm)) {
                        rc = MPI_ERR_REQUEST;
                        break;
                    }
                }
            }
        }
        if ((NULL == indx && count > 0) ||
            count < 0) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    }

    if (OPAL_UNLIKELY(0 == count)) {
        *indx = MPI_UNDEFINED;
        if (MPI_STATUS_IGNORE != status) {
            OMPI_COPY_STATUS(status, ompi_status_empty, false);
        }
        return MPI_SUCCESS;
    }

    if (OMPI_SUCCESS == ompi_request_wait_any(count, requests, indx, status)) {
        return MPI_SUCCESS;
    }

    return ompi_errhandler_request_invoke(count, requests, FUNC_NAME);
}
