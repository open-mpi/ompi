/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_CALL_ERRHANDLER = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler_ = mpi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler__ = mpi_file_call_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_CALL_ERRHANDLER,
                           pmpi_file_call_errhandler,
                           pmpi_file_call_errhandler_,
                           pmpi_file_call_errhandler__,
                           pmpi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CALL_ERRHANDLER = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler_ = mpi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler__ = mpi_file_call_errhandler_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_CALL_ERRHANDLER,
                           mpi_file_call_errhandler,
                           mpi_file_call_errhandler_,
                           mpi_file_call_errhandler__,
                           mpi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_file_call_errhandler_f(MPI_Fint *fh, MPI_Fint *errorcode,
				MPI_Fint *ierr)
{
    MPI_File c_fh;

    c_fh = MPI_File_f2c(*fh);

    *ierr = OMPI_INT_2_FINT(MPI_File_call_errhandler(c_fh,
						     OMPI_FINT_2_INT(*errorcode)
						     ));
}
