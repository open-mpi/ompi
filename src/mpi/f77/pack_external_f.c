/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_PACK_EXTERNAL = mpi_pack_external_f
#pragma weak pmpi_pack_external = mpi_pack_external_f
#pragma weak pmpi_pack_external_ = mpi_pack_external_f
#pragma weak pmpi_pack_external__ = mpi_pack_external_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_PACK_EXTERNAL,
                           pmpi_pack_external,
                           pmpi_pack_external_,
                           pmpi_pack_external__,
                           pmpi_pack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *ierr),
                           (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_EXTERNAL = mpi_pack_external_f
#pragma weak mpi_pack_external = mpi_pack_external_f
#pragma weak mpi_pack_external_ = mpi_pack_external_f
#pragma weak mpi_pack_external__ = mpi_pack_external_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_PACK_EXTERNAL,
                           mpi_pack_external,
                           mpi_pack_external_,
                           mpi_pack_external__,
                           mpi_pack_external_f,
                           (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *ierr),
                           (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_pack_external_f(char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *ierr)
{

}
