/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_ADD_ERROR_CLASS = mpi_add_error_class_f
#pragma weak pmpi_add_error_class = mpi_add_error_class_f
#pragma weak pmpi_add_error_class_ = mpi_add_error_class_f
#pragma weak pmpi_add_error_class__ = mpi_add_error_class_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_CLASS,
                           pmpi_add_error_class,
                           pmpi_add_error_class_,
                           pmpi_add_error_class__,
                           pmpi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_CLASS = mpi_add_error_class_f
#pragma weak mpi_add_error_class = mpi_add_error_class_f
#pragma weak mpi_add_error_class_ = mpi_add_error_class_f
#pragma weak mpi_add_error_class__ = mpi_add_error_class_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_CLASS,
                           mpi_add_error_class,
                           mpi_add_error_class_,
                           mpi_add_error_class__,
                           mpi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif

void mpi_add_error_class_f(MPI_Fint *errorclass, MPI_Fint *ierr)
{

}
