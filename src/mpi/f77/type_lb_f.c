/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_LB = mpi_type_lb_f
#pragma weak pmpi_type_lb = mpi_type_lb_f
#pragma weak pmpi_type_lb_ = mpi_type_lb_f
#pragma weak pmpi_type_lb__ = mpi_type_lb_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_LB,
                           pmpi_type_lb,
                           pmpi_type_lb_,
                           pmpi_type_lb__,
                           pmpi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_LB = mpi_type_lb_f
#pragma weak mpi_type_lb = mpi_type_lb_f
#pragma weak mpi_type_lb_ = mpi_type_lb_f
#pragma weak mpi_type_lb__ = mpi_type_lb_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_LB,
                           mpi_type_lb,
                           mpi_type_lb_,
                           mpi_type_lb__,
                           mpi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_lb_f(MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr)
{

}
