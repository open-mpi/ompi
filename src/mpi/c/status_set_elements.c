/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Status_set_elements = PMPI_Status_set_elements
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Status_set_elements";


int MPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype,
                            int count) 
{
    int rc = MPI_SUCCESS;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == datatype || MPI_DATATYPE_NULL == datatype) {
            rc = MPI_ERR_TYPE;
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    if (status != MPI_STATUS_IGNORE) {
        status->_count = count;
    }
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
