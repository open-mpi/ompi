/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mpi/f77/fint_2_int.h"
#include "mpi/f77/constants.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Status_f2c = PMPI_Status_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Status_f2c";


int MPI_Status_f2c(MPI_Fint *f_status, MPI_Status *c_status) 
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        /* MPI-2:4.12.5 says that if you pass in
           MPI_STATUS[ES]_IGNORE, it's erroneous */
        
        if (NULL == f_status || 
#if OMPI_WANT_F77_BINDINGS || OMPI_WANT_F90_BINDINGS
            /* This section is #if'ed out if we are not building the
               fortran bindings because these macros check values
               against constants that only exist if the fortran
               bindings exist. */
            OMPI_IS_FORTRAN_STATUS_IGNORE(f_status) ||
            OMPI_IS_FORTRAN_STATUSES_IGNORE(f_status) ||
#endif
            NULL == c_status) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                          MPI_ERR_IN_STATUS, FUNC_NAME);
        }
    }

    /* We can't use OMPI_FINT_2_INT here because of some complications
       with include files.  :-( So just do the casting manually. */

    c_status->MPI_SOURCE = (int) f_status[0];
    c_status->MPI_TAG = (int) f_status[1];
    c_status->MPI_ERROR = (int) f_status[2];
    c_status->_count = (int) f_status[3];

    return MPI_SUCCESS;
}
