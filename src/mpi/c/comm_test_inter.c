/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_test_inter = PMPI_Comm_test_inter
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_test_inter";


int MPI_Comm_test_inter(MPI_Comm comm, int *flag) {

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        
        if ( MPI_COMM_NULL == comm || ompi_comm_invalid ( comm ) ) {
             return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                             FUNC_NAME);
        }
        
        if ( NULL == flag ) {
             return OMPI_ERRHANDLER_INVOKE ( comm, MPI_ERR_ARG, 
                                             FUNC_NAME);
        }
    }

    *flag = (comm->c_flags & OMPI_COMM_INTER);
    return MPI_SUCCESS;
}
