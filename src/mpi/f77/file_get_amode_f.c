/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_AMODE = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode_ = mpi_file_get_amode_f
#pragma weak pmpi_file_get_amode__ = mpi_file_get_amode_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_AMODE,
                           pmpi_file_get_amode,
                           pmpi_file_get_amode_,
                           pmpi_file_get_amode__,
                           pmpi_file_get_amode_f,
                           (MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr),
                           (fh, amode, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_AMODE = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode_ = mpi_file_get_amode_f
#pragma weak mpi_file_get_amode__ = mpi_file_get_amode_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_AMODE,
                           mpi_file_get_amode,
                           mpi_file_get_amode_,
                           mpi_file_get_amode__,
                           mpi_file_get_amode_f,
                           (MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr),
                           (fh, amode, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_get_amode_f(MPI_Fint *fh, MPI_Fint *amode, MPI_Fint *ierr)
{
    MPI_File c_fh;
    OMPI_SINGLE_NAME_DECL(amode);
    
    c_fh = MPI_File_f2c(*fh);
    *ierr = OMPI_INT_2_FINT(MPI_File_get_amode(c_fh,
					       OMPI_SINGLE_NAME_CONVERT(amode)
					       ));

    OMPI_SINGLE_INT_2_FINT(amode);
}
