/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_PROBE = mpi_probe_f
#pragma weak pmpi_probe = mpi_probe_f
#pragma weak pmpi_probe_ = mpi_probe_f
#pragma weak pmpi_probe__ = mpi_probe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PROBE,
                           pmpi_probe,
                           pmpi_probe_,
                           pmpi_probe__,
                           pmpi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PROBE = mpi_probe_f
#pragma weak mpi_probe = mpi_probe_f
#pragma weak mpi_probe_ = mpi_probe_f
#pragma weak mpi_probe__ = mpi_probe_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PROBE,
                           mpi_probe,
                           mpi_probe_,
                           mpi_probe__,
                           mpi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_probe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{   
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Probe(*source, *tag, c_comm, (MPI_Status*) *status);
}
