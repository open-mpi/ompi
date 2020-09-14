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
#pragma weak PMPI_STATUS_F2F08 = ompi_status_f2f08_f
#pragma weak pmpi_status_f2f08 = ompi_status_f2f08_f
#pragma weak pmpi_status_f2f08_ = ompi_status_f2f08_f
#pragma weak pmpi_status_f2f08__ = ompi_status_f2f08_f

#pragma weak PMPI_Status_f2f08_f = ompi_status_f2f08_f
#pragma weak PMPI_Status_f2f08_f08 = ompi_status_f2f08_f
#else
OMPI_GENERATE_F77_BINDINGS(PMPI_STATUS_F2F08,
                           pmpi_status_f2f08,
                           pmpi_status_f2f08_,
                           pmpi_status_f2f08__,
                           pompi_status_f2f08_f,
                           (const MPI_Fint *f_status, MPI_F08_status *f08_status, MPI_Fint *ierr),
                           (f_status, f08_status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_F2F08 = ompi_status_f2f08_f
#pragma weak mpi_status_f2f08 = ompi_status_f2f08_f
#pragma weak mpi_status_f2f08_ = ompi_status_f2f08_f
#pragma weak mpi_status_f2f08__ = ompi_status_f2f08_f

#pragma weak MPI_Status_f2f08_f = ompi_status_f2f08_f
#pragma weak MPI_Status_f2f08_f08 = ompi_status_f2f08_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS(MPI_STATUS_F2F08,
                           mpi_status_f2f08,
                           mpi_status_f2f08_,
                           mpi_status_f2f08__,
                           ompi_status_f2f08_f,
                           (const MPI_Fint *f_status, MPI_F08_status *f08_status, MPI_Fint *ierr),
                           (f_status, f08_status, ierr) )
#else
#define ompi_status_f2f08_f pompi_status_f2f08_f
#endif
#endif


void ompi_status_f2f08_f(const MPI_Fint *f_status, MPI_F08_status *f08_status, MPI_Fint *ierr)
{
    int c_ierr;

    c_ierr = PMPI_Status_f2f08(f_status, f08_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
