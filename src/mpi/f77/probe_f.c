/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_PROBE = mpi_probe_f
#pragma weak pmpi_probe = mpi_probe_f
#pragma weak pmpi_probe_ = mpi_probe_f
#pragma weak pmpi_probe__ = mpi_probe_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_PROBE,
                           pmpi_probe,
                           pmpi_probe_,
                           pmpi_probe__,
                           pmpi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PROBE = mpi_probe_f
#pragma weak mpi_probe = mpi_probe_f
#pragma weak mpi_probe_ = mpi_probe_f
#pragma weak mpi_probe__ = mpi_probe_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_PROBE,
                           mpi_probe,
                           mpi_probe_,
                           mpi_probe__,
                           mpi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_probe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{

}
