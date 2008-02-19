/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Testany = PMPI_Testany
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Testany";


int MPI_Testany(int count, MPI_Request requests[], int *index, int *completed, MPI_Status *status) 
{
    MEMCHECKER(
        int j;
        for (j = 0; j < count; j++){
            memchecker_request(&requests[j]);
        }
    );

    if ( MPI_PARAM_CHECK ) {
        int i, rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((NULL == requests) && (0 != count)) {
            rc = MPI_ERR_REQUEST;
        } else {
            for (i = 0; i < count; i++) {
                if (NULL == requests[i]) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
            }
        }
        if ((NULL == index) || (NULL == completed) || (0 > count)) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    if (OMPI_SUCCESS == ompi_request_test_any(count, requests, 
                                              index, completed, status)) {
        OPAL_CR_EXIT_LIBRARY();
        return MPI_SUCCESS;
    }

    OPAL_CR_EXIT_LIBRARY();
    return ompi_errhandler_request_invoke(count, requests, FUNC_NAME);
}
