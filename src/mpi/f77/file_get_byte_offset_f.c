/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_BYTE_OFFSET = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset_ = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset__ = mpi_file_get_byte_offset_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_BYTE_OFFSET,
                           pmpi_file_get_byte_offset,
                           pmpi_file_get_byte_offset_,
                           pmpi_file_get_byte_offset__,
                           pmpi_file_get_byte_offset_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *disp, MPI_Fint *ierr),
                           (fh, offset, disp, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_BYTE_OFFSET = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset_ = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset__ = mpi_file_get_byte_offset_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_BYTE_OFFSET,
                           mpi_file_get_byte_offset,
                           mpi_file_get_byte_offset_,
                           mpi_file_get_byte_offset__,
                           mpi_file_get_byte_offset_f,
                           (MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *disp, MPI_Fint *ierr),
                           (fh, offset, disp, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_get_byte_offset_f(MPI_Fint *fh, MPI_Fint *offset, MPI_Fint *disp, MPI_Fint *ierr)
{

}
