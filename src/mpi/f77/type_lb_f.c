/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_LB = mpi_type_lb_f
#pragma weak pmpi_type_lb = mpi_type_lb_f
#pragma weak pmpi_type_lb_ = mpi_type_lb_f
#pragma weak pmpi_type_lb__ = mpi_type_lb_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_LB,
                           pmpi_type_lb,
                           pmpi_type_lb_,
                           pmpi_type_lb__,
                           pmpi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_LB = mpi_type_lb_f
#pragma weak mpi_type_lb = mpi_type_lb_f
#pragma weak mpi_type_lb_ = mpi_type_lb_f
#pragma weak mpi_type_lb__ = mpi_type_lb_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_LB,
                           mpi_type_lb,
                           mpi_type_lb_,
                           mpi_type_lb__,
                           mpi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_lb_f(MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    MPI_Aint c_lb;

    *ierr = MPI_Type_lb(c_type, &c_lb);

    if (MPI_SUCCESS == *ierr) {
      *lb = (MPI_Fint)c_lb;
    }
}
