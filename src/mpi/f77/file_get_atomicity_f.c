/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_ATOMICITY = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity_ = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity__ = mpi_file_get_atomicity_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_ATOMICITY,
                           pmpi_file_get_atomicity,
                           pmpi_file_get_atomicity_,
                           pmpi_file_get_atomicity__,
                           pmpi_file_get_atomicity_f,
                           (MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_ATOMICITY = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity_ = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity__ = mpi_file_get_atomicity_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_ATOMICITY,
                           mpi_file_get_atomicity,
                           mpi_file_get_atomicity_,
                           mpi_file_get_atomicity__,
                           mpi_file_get_atomicity_f,
                           (MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_file_get_atomicity_f(MPI_Fint *fh, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_File c_fh;
    OMPI_SINGLE_NAME_DECL(flag);

    c_fh = MPI_File_f2c(*fh);
    *ierr = OMPI_INT_2_FINT(MPI_File_get_atomicity(c_fh, 
					   OMPI_SINGLE_NAME_CONVERT(flag)));

    OMPI_SINGLE_INT_2_FINT(flag);
}
