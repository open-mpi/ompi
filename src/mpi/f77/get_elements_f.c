/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_GET_ELEMENTS = mpi_get_elements_f
#pragma weak pmpi_get_elements = mpi_get_elements_f
#pragma weak pmpi_get_elements_ = mpi_get_elements_f
#pragma weak pmpi_get_elements__ = mpi_get_elements_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_GET_ELEMENTS,
                           pmpi_get_elements,
                           pmpi_get_elements_,
                           pmpi_get_elements__,
                           pmpi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ELEMENTS = mpi_get_elements_f
#pragma weak mpi_get_elements = mpi_get_elements_f
#pragma weak mpi_get_elements_ = mpi_get_elements_f
#pragma weak mpi_get_elements__ = mpi_get_elements_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_GET_ELEMENTS,
                           mpi_get_elements,
                           mpi_get_elements_,
                           mpi_get_elements__,
                           mpi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

void mpi_get_elements_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{

}
