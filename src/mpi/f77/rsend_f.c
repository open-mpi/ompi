/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_RSEND = mpi_rsend_f
#pragma weak pmpi_rsend = mpi_rsend_f
#pragma weak pmpi_rsend_ = mpi_rsend_f
#pragma weak pmpi_rsend__ = mpi_rsend_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_RSEND,
                           pmpi_rsend,
                           pmpi_rsend_,
                           pmpi_rsend__,
                           pmpi_rsend_f,
                           (char *ibuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (ibuf, count, datatype, dest, tag, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RSEND = mpi_rsend_f
#pragma weak mpi_rsend = mpi_rsend_f
#pragma weak mpi_rsend_ = mpi_rsend_f
#pragma weak mpi_rsend__ = mpi_rsend_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_RSEND,
                           mpi_rsend,
                           mpi_rsend_,
                           mpi_rsend__,
                           mpi_rsend_f,
                           (char *ibuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (ibuf, count, datatype, dest, tag, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_rsend_f(char *ibuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{

}
