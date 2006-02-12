/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpi/f77/fint_2_int.h"
#include "ompi/mpi/f77/constants.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Status_f2c = PMPI_Status_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Status_f2c";


int MPI_Status_f2c(MPI_Fint *f_status, MPI_Status *c_status) 
{
    int i, *c_ints;

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
    c_ints = (int*)c_status;
    for( i = 0; i < (int)(sizeof(MPI_Status) / sizeof(int)); i++ )
        c_ints[i] = (int)f_status[i];

    /*
    c_status->MPI_SOURCE = (int) f_status[0];
    c_status->MPI_TAG = (int) f_status[1];
    c_status->MPI_ERROR = (int) f_status[2];
    c_status->_count = (int) f_status[3];
    c_status->_cancelled = (int) f_status[4];
    */
    return MPI_SUCCESS;
}
