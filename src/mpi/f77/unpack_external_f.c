/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_UNPACK_EXTERNAL = mpi_unpack_external_f
#pragma weak pmpi_unpack_external = mpi_unpack_external_f
#pragma weak pmpi_unpack_external_ = mpi_unpack_external_f
#pragma weak pmpi_unpack_external__ = mpi_unpack_external_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_UNPACK_EXTERNAL,
                           pmpi_unpack_external,
                           pmpi_unpack_external_,
                           pmpi_unpack_external__,
                           pmpi_unpack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr),
                           (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_UNPACK_EXTERNAL = mpi_unpack_external_f
#pragma weak mpi_unpack_external = mpi_unpack_external_f
#pragma weak mpi_unpack_external_ = mpi_unpack_external_f
#pragma weak mpi_unpack_external__ = mpi_unpack_external_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_UNPACK_EXTERNAL,
                           mpi_unpack_external,
                           mpi_unpack_external_,
                           mpi_unpack_external__,
                           mpi_unpack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr),
                           (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_unpack_external_f (char *datarep, char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr)
{

}
