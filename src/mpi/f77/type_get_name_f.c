/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_NAME = mpi_type_get_name_f
#pragma weak pmpi_type_get_name = mpi_type_get_name_f
#pragma weak pmpi_type_get_name_ = mpi_type_get_name_f
#pragma weak pmpi_type_get_name__ = mpi_type_get_name_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_NAME,
                           pmpi_type_get_name,
                           pmpi_type_get_name_,
                           pmpi_type_get_name__,
                           pmpi_type_get_name_f,
                           (MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (type, type_name, resultlen, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_NAME = mpi_type_get_name_f
#pragma weak mpi_type_get_name = mpi_type_get_name_f
#pragma weak mpi_type_get_name_ = mpi_type_get_name_f
#pragma weak mpi_type_get_name__ = mpi_type_get_name_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_GET_NAME,
                           mpi_type_get_name,
                           mpi_type_get_name_,
                           mpi_type_get_name__,
                           mpi_type_get_name_f,
                           (MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (type, type_name, resultlen, ierr) )
#endif

void mpi_type_get_name_f(MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr)
{

}
