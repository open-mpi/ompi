/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_RSEND_INIT = mpi_rsend_init_f
#pragma weak pmpi_rsend_init = mpi_rsend_init_f
#pragma weak pmpi_rsend_init_ = mpi_rsend_init_f
#pragma weak pmpi_rsend_init__ = mpi_rsend_init_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_RSEND_INIT,
                           pmpi_rsend_init,
                           pmpi_rsend_init_,
                           pmpi_rsend_init__,
                           pmpi_rsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RSEND_INIT = mpi_rsend_init_f
#pragma weak mpi_rsend_init = mpi_rsend_init_f
#pragma weak mpi_rsend_init_ = mpi_rsend_init_f
#pragma weak mpi_rsend_init__ = mpi_rsend_init_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_RSEND_INIT,
                           mpi_rsend_init,
                           mpi_rsend_init_,
                           mpi_rsend_init__,
                           mpi_rsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_rsend_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{

}
