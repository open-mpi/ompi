/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_UNPACK = mpi_unpack_f
#pragma weak pmpi_unpack = mpi_unpack_f
#pragma weak pmpi_unpack_ = mpi_unpack_f
#pragma weak pmpi_unpack__ = mpi_unpack_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_UNPACK,
                           pmpi_unpack,
                           pmpi_unpack_,
                           pmpi_unpack__,
                           pmpi_unpack_f,
                           (char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, insize, position, outbuf, outcount, datatype, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_UNPACK = mpi_unpack_f
#pragma weak mpi_unpack = mpi_unpack_f
#pragma weak mpi_unpack_ = mpi_unpack_f
#pragma weak mpi_unpack__ = mpi_unpack_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_UNPACK,
                           mpi_unpack,
                           mpi_unpack_,
                           mpi_unpack__,
                           mpi_unpack_f,
                           (char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, insize, position, outbuf, outcount, datatype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_unpack_f(char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *ierr)
{

}
