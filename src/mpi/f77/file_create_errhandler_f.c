/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_CREATE_ERRHANDLER = mpi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler = mpi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler_ = mpi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler__ = mpi_file_create_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_CREATE_ERRHANDLER,
                           pmpi_file_create_errhandler,
                           pmpi_file_create_errhandler_,
                           pmpi_file_create_errhandler__,
                           pmpi_file_create_errhandler_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CREATE_ERRHANDLER = mpi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler = mpi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler_ = mpi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler__ = mpi_file_create_errhandler_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_CREATE_ERRHANDLER,
                           mpi_file_create_errhandler,
                           mpi_file_create_errhandler_,
                           mpi_file_create_errhandler__,
                           mpi_file_create_errhandler_f,
                           (MPI_Fint *function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_file_create_errhandler_f(MPI_Fint *function, 
				  MPI_Fint *errhandler, MPI_Fint *ierr)
{
    MPI_Errhandler c_errhandler;
    
    *ierr = 
	OMPI_INT_2_FINT(MPI_File_create_errhandler((MPI_File_errhandler_fn *)
						   function,
						   &c_errhandler));

    *errhandler = MPI_Errhandler_c2f(c_errhandler);
}
