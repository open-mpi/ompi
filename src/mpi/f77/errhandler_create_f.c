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

#include "mpi/f77/bindings.h"

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
                           (void *function, MPI_Fint *errhandler, MPI_Fint *ierr),
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
                           (void *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_create_f(void *function, 
			     MPI_Fint *errhandler, MPI_Fint *ierr)
{
    MPI_Errhandler c_errhandler;

    /* See the note in src/mpi/f77/prototypes_mpi.h about the use of
       (void*) for function pointers in this function */

    *ierr = OMPI_INT_2_FINT(MPI_Errhandler_create(
                    (MPI_Handler_function *) function,
                    &c_errhandler));
    *errhandler = MPI_Errhandler_c2f(c_errhandler);
}
