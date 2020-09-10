/*
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_STATUS_F082F = ompi_status_f082f_f
#pragma weak pmpi_status_f082f = ompi_status_f082f_f
#pragma weak pmpi_status_f082f_ = ompi_status_f082f_f
#pragma weak pmpi_status_f082f__ = ompi_status_f082f_f

#pragma weak PMPI_Status_f082f_f = ompi_status_f082f_f
#pragma weak PMPI_Status_f082f_f08 = ompi_status_f082f_f
#else
OMPI_GENERATE_F77_BINDINGS(PMPI_STATUS_F082F,
                           pmpi_status_f082f,
                           pmpi_status_f082f_,
                           pmpi_status_f082f__,
                           pompi_status_f082f_f,
                           (const MPI_F08_status *f08_status, MPI_Fint *f_status, MPI_Fint *ierr),
                           (f08_status, f_status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_F082F = ompi_status_f082f_f
#pragma weak mpi_status_f082f = ompi_status_f082f_f
#pragma weak mpi_status_f082f_ = ompi_status_f082f_f
#pragma weak mpi_status_f082f__ = ompi_status_f082f_f

#pragma weak MPI_Status_f082f_f = ompi_status_f082f_f
#pragma weak MPI_Status_f082f_f08 = ompi_status_f082f_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS(MPI_STATUS_F082F,
                           mpi_status_f082f,
                           mpi_status_f082f_,
                           mpi_status_f082f__,
                           ompi_status_f082f_f,
                           (const MPI_F08_status *f08_status, MPI_Fint *f_status, MPI_Fint *ierr),
                           (f08_status, f_status, ierr) )
#else
#define ompi_status_f082f_f pompi_status_f082f_f
#endif
#endif


void ompi_status_f082f_f(const MPI_F08_status *f08_status, MPI_Fint *f_status, MPI_Fint *ierr)
{
    int c_ierr;

    c_ierr = PMPI_Status_f082f(f08_status, f_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
