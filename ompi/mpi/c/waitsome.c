/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
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
#pragma weak MPI_Waitsome = PMPI_Waitsome
#endif
#define MPI_Waitsome PMPI_Waitsome
#endif

static const char FUNC_NAME[] = "MPI_Waitsome";


int MPI_Waitsome(int incount, MPI_Request requests[],
                 int *outcount, int indices[],
                 MPI_Status statuses[])
{
    SPC_RECORD(OMPI_SPC_WAITSOME, 1);

    MEMCHECKER(
        int j;
        for (j = 0; j < incount; j++){
            memchecker_request(&requests[j]);
        }
    );

    if ( MPI_PARAM_CHECK ) {
        int indx, rc = MPI_SUCCESS;
        MPI_Request check_req = NULL;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((NULL == requests) && (0 != incount)) {
            rc = MPI_ERR_REQUEST;
        } else {
            for (indx = 0; indx < incount; ++indx) {
                if (NULL == requests[indx]) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
                if (&ompi_request_empty == requests[indx]) {
                    continue;
                } else if (NULL == requests[indx]->req_mpi_object.comm) {
                    continue;
                } else if (NULL == check_req) {
                    check_req = requests[indx];
                }
                else {
                    if (!ompi_comm_instances_same(requests[indx]->req_mpi_object.comm,
                                                     check_req->req_mpi_object.comm)) {
                        rc = MPI_ERR_REQUEST;
                        break;
                    }
                }
            }
        }
        if (((NULL == outcount || NULL == indices) && incount > 0) ||
            incount < 0) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    }

    if (OPAL_UNLIKELY(0 == incount)) {
        *outcount = MPI_UNDEFINED;
        return MPI_SUCCESS;
    }

    if (OMPI_SUCCESS == ompi_request_wait_some( incount, requests,
                                                outcount, indices, statuses )) {
        return MPI_SUCCESS;
    }

    if (MPI_SUCCESS !=
        ompi_errhandler_request_invoke(incount, requests, FUNC_NAME)) {
        return MPI_ERR_IN_STATUS;
    }

    return MPI_SUCCESS;
}
