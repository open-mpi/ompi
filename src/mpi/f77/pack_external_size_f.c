/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_PACK_EXTERNAL_SIZE = mpi_pack_external_size_f
#pragma weak pmpi_pack_external_size = mpi_pack_external_size_f
#pragma weak pmpi_pack_external_size_ = mpi_pack_external_size_f
#pragma weak pmpi_pack_external_size__ = mpi_pack_external_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK_EXTERNAL_SIZE,
                           pmpi_pack_external_size,
                           pmpi_pack_external_size_,
                           pmpi_pack_external_size__,
                           pmpi_pack_external_size_f,
                           (char *datarep, MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *size, MPI_Fint *ierr),
                           (datarep, incount, datatype, size, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_EXTERNAL_SIZE = mpi_pack_external_size_f
#pragma weak mpi_pack_external_size = mpi_pack_external_size_f
#pragma weak mpi_pack_external_size_ = mpi_pack_external_size_f
#pragma weak mpi_pack_external_size__ = mpi_pack_external_size_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PACK_EXTERNAL_SIZE,
                           mpi_pack_external_size,
                           mpi_pack_external_size_,
                           mpi_pack_external_size__,
                           mpi_pack_external_size_f,
                           (char *datarep, MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *size, MPI_Fint *ierr),
                           (datarep, incount, datatype, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_pack_external_size_f(char *datarep, MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *size, MPI_Fint *ierr)
{
  MPI_Datatype type = MPI_Type_f2c(*datatype);

  *ierr = MPI_Pack_external_size (datarep, *incount,
                                  type, (MPI_Aint *)size);

}
