/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_PCONTROL = mpi_pcontrol_f
#pragma weak pmpi_pcontrol = mpi_pcontrol_f
#pragma weak pmpi_pcontrol_ = mpi_pcontrol_f
#pragma weak pmpi_pcontrol__ = mpi_pcontrol_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_PCONTROL,
                           pmpi_pcontrol,
                           pmpi_pcontrol_,
                           pmpi_pcontrol__,
                           pmpi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PCONTROL = mpi_pcontrol_f
#pragma weak mpi_pcontrol = mpi_pcontrol_f
#pragma weak mpi_pcontrol_ = mpi_pcontrol_f
#pragma weak mpi_pcontrol__ = mpi_pcontrol_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_PCONTROL,
                           mpi_pcontrol,
                           mpi_pcontrol_,
                           mpi_pcontrol__,
                           mpi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#endif

void mpi_pcontrol_f(MPI_Fint *level)
{

}
