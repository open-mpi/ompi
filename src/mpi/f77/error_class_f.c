/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ERROR_CLASS = mpi_error_class_f
#pragma weak pmpi_error_class = mpi_error_class_f
#pragma weak pmpi_error_class_ = mpi_error_class_f
#pragma weak pmpi_error_class__ = mpi_error_class_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ERROR_CLASS,
                           pmpi_error_class,
                           pmpi_error_class_,
                           pmpi_error_class__,
                           pmpi_error_class_f,
                           (MPI_Fint *errorcode, MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorcode, errorclass, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERROR_CLASS = mpi_error_class_f
#pragma weak mpi_error_class = mpi_error_class_f
#pragma weak mpi_error_class_ = mpi_error_class_f
#pragma weak mpi_error_class__ = mpi_error_class_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ERROR_CLASS,
                           mpi_error_class,
                           mpi_error_class_,
                           mpi_error_class__,
                           mpi_error_class_f,
                           (MPI_Fint *errorcode, MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorcode, errorclass, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_error_class_f(MPI_Fint *errorcode, MPI_Fint *errorclass, MPI_Fint *ierr)
{

}
