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

#include "ompi/mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_CREATE = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create_ = mpi_errhandler_create_f
#pragma weak pmpi_errhandler_create__ = mpi_errhandler_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_CREATE,
                           pmpi_errhandler_create,
                           pmpi_errhandler_create_,
                           pmpi_errhandler_create__,
                           pmpi_errhandler_create_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_CREATE = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create_ = mpi_errhandler_create_f
#pragma weak mpi_errhandler_create__ = mpi_errhandler_create_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_CREATE,
                           mpi_errhandler_create,
                           mpi_errhandler_create_,
                           mpi_errhandler_create__,
                           mpi_errhandler_create_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_create_f(ompi_errhandler_fortran_handler_fn_t* function, 
			     MPI_Fint *errhandler, MPI_Fint *ierr)
{
    MPI_Errhandler c_errhandler;

    /* See the note in src/mpi/f77/prototypes_mpi.h about the use of
       (void*) for function pointers in this function */

    *ierr = OMPI_INT_2_FINT(MPI_Errhandler_create(
                    (MPI_Handler_function *) function,
                    &c_errhandler));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *errhandler = MPI_Errhandler_c2f(c_errhandler);
    }
}
