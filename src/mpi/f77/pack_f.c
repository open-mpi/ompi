/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_PACK = mpi_pack_f
#pragma weak pmpi_pack = mpi_pack_f
#pragma weak pmpi_pack_ = mpi_pack_f
#pragma weak pmpi_pack__ = mpi_pack_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_PACK,
                           pmpi_pack,
                           pmpi_pack_,
                           pmpi_pack__,
                           pmpi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK = mpi_pack_f
#pragma weak mpi_pack = mpi_pack_f
#pragma weak mpi_pack_ = mpi_pack_f
#pragma weak mpi_pack__ = mpi_pack_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_PACK,
                           mpi_pack,
                           mpi_pack_,
                           mpi_pack__,
                           mpi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_pack_f(char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr)
{

}
