/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Startall = PMPI_Startall
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Startall";


int MPI_Startall(int count, MPI_Request requests[]) 
{
    int i;
    int ret = OMPI_SUCCESS;

    MEMCHECKER(
        int j;
        for (j = 0; j < count; j++){
            memchecker_request(&requests[j]);
        }
    );

    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == requests) {
            rc = MPI_ERR_REQUEST;
        } else if (count < 0) {
            rc = MPI_ERR_ARG;
        } else {
            for (i = 0; i < count; ++i) {
                if (NULL == requests[i] ||
                    (OMPI_REQUEST_PML != requests[i]->req_type &&
                     OMPI_REQUEST_NOOP != requests[i]->req_type)) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
            }
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    for (i = 0; i < count; ++i) {
        if (OMPI_REQUEST_NOOP == requests[i]->req_type) {
            /**
             * We deal with a MPI_PROC_NULL request. If the request is
             * already active, fall back to the error case in the default.
             * Otherwise, mark it active so we can correctly handle it in
             * the wait*.
             */
            if( OMPI_REQUEST_INACTIVE == requests[i]->req_state ) {
                requests[i]->req_state = OMPI_REQUEST_ACTIVE;
            }
        }
    }
    ret = MCA_PML_CALL(start(count, requests));

    OPAL_CR_EXIT_LIBRARY();
    return ret;
}

