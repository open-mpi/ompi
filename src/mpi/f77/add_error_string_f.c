/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ADD_ERROR_STRING = mpi_add_error_string_f
#pragma weak pmpi_add_error_string = mpi_add_error_string_f
#pragma weak pmpi_add_error_string_ = mpi_add_error_string_f
#pragma weak pmpi_add_error_string__ = mpi_add_error_string_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_STRING,
                           pmpi_add_error_string,
                           pmpi_add_error_string_,
                           pmpi_add_error_string__,
                           pmpi_add_error_string_f,
                           (MPI_Fint *errorcode, char *string, MPI_Fint *ierr),
                           (errorcode, string, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_STRING = mpi_add_error_string_f
#pragma weak mpi_add_error_string = mpi_add_error_string_f
#pragma weak mpi_add_error_string_ = mpi_add_error_string_f
#pragma weak mpi_add_error_string__ = mpi_add_error_string_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_STRING,
                           mpi_add_error_string,
                           mpi_add_error_string_,
                           mpi_add_error_string__,
                           mpi_add_error_string_f,
                           (MPI_Fint *errorcode, char *string, MPI_Fint *ierr),
                           (errorcode, string, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_add_error_string_f(MPI_Fint *errorcode, char *string, MPI_Fint *ierr)
{

}
