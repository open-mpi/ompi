/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ADD_ERROR_CODE = mpi_add_error_code_f
#pragma weak pmpi_add_error_code = mpi_add_error_code_f
#pragma weak pmpi_add_error_code_ = mpi_add_error_code_f
#pragma weak pmpi_add_error_code__ = mpi_add_error_code_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_CODE,
                           pmpi_add_error_code,
                           pmpi_add_error_code_,
                           pmpi_add_error_code__,
                           pmpi_add_error_code_f,
                           (MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (errorclass, errorcode, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_CODE = mpi_add_error_code_f
#pragma weak mpi_add_error_code = mpi_add_error_code_f
#pragma weak mpi_add_error_code_ = mpi_add_error_code_f
#pragma weak mpi_add_error_code__ = mpi_add_error_code_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_CODE,
                           mpi_add_error_code,
                           mpi_add_error_code_,
                           mpi_add_error_code__,
                           mpi_add_error_code_f,
                           (MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (errorclass, errorcode, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_add_error_code_f(MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr)
{
  /* This function not yet implemented */
}
