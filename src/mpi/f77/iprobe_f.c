/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_IPROBE = mpi_iprobe_f
#pragma weak pmpi_iprobe = mpi_iprobe_f
#pragma weak pmpi_iprobe_ = mpi_iprobe_f
#pragma weak pmpi_iprobe__ = mpi_iprobe_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_IPROBE,
                           pmpi_iprobe,
                           pmpi_iprobe_,
                           pmpi_iprobe__,
                           pmpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IPROBE = mpi_iprobe_f
#pragma weak mpi_iprobe = mpi_iprobe_f
#pragma weak mpi_iprobe_ = mpi_iprobe_f
#pragma weak mpi_iprobe__ = mpi_iprobe_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_IPROBE,
                           mpi_iprobe,
                           mpi_iprobe_,
                           mpi_iprobe__,
                           mpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif

void mpi_iprobe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{

}
