/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_IPROBE = mpi_iprobe_f
#pragma weak pmpi_iprobe = mpi_iprobe_f
#pragma weak pmpi_iprobe_ = mpi_iprobe_f
#pragma weak pmpi_iprobe__ = mpi_iprobe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IPROBE,
                           pmpi_iprobe,
                           pmpi_iprobe_,
                           pmpi_iprobe__,
                           pmpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IPROBE = mpi_iprobe_f
#pragma weak mpi_iprobe = mpi_iprobe_f
#pragma weak mpi_iprobe_ = mpi_iprobe_f
#pragma weak mpi_iprobe__ = mpi_iprobe_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IPROBE,
                           mpi_iprobe,
                           mpi_iprobe_,
                           mpi_iprobe__,
                           mpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_iprobe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Iprobe(*source, *tag, c_comm, flag, (MPI_Status*)status);
}
