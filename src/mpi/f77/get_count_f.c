/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET_COUNT = mpi_get_count_f
#pragma weak pmpi_get_count = mpi_get_count_f
#pragma weak pmpi_get_count_ = mpi_get_count_f
#pragma weak pmpi_get_count__ = mpi_get_count_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_COUNT,
                           pmpi_get_count,
                           pmpi_get_count_,
                           pmpi_get_count__,
                           pmpi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_COUNT = mpi_get_count_f
#pragma weak mpi_get_count = mpi_get_count_f
#pragma weak mpi_get_count_ = mpi_get_count_f
#pragma weak mpi_get_count__ = mpi_get_count_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_COUNT,
                           mpi_get_count,
                           mpi_get_count_,
                           mpi_get_count__,
                           mpi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_get_count_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Status   c_status;

    *ierr = MPI_Status_f2c(status, &c_status);

    if (*ierr == MPI_SUCCESS)
      *ierr = MPI_Get_count(&c_status, c_type, count);
}
