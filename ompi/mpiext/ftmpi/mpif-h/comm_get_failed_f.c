/*
 * Copyright (c) 2022      The University of Tennessee and the University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"
#include "ompi/mpiext/ftmpi/mpif-h/prototypes_mpi.h"

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_COMM_GET_FAILED = ompix_comm_get_failed_f
#pragma weak pmpix_comm_get_failed = ompix_comm_get_failed_f
#pragma weak pmpix_comm_get_failed_ = ompix_comm_get_failed_f
#pragma weak pmpix_comm_get_failed__ = ompix_comm_get_failed_f
#pragma weak PMPIX_Comm_get_failed_f = ompix_comm_get_failed_f
#pragma weak PMPIX_Comm_get_failed_f08 = ompix_comm_get_failed_f

#pragma weak MPIX_COMM_GET_FAILED = ompix_comm_get_failed_f
#pragma weak mpix_comm_get_failed = ompix_comm_get_failed_f
#pragma weak mpix_comm_get_failed_ = ompix_comm_get_failed_f
#pragma weak mpix_comm_get_failed__ = ompix_comm_get_failed_f
#pragma weak MPIX_Comm_get_failed_f = ompix_comm_get_failed_f
#pragma weak MPIX_Comm_get_failed_f08 = ompix_comm_get_failed_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_GET_FAILED,
                        pmpix_comm_get_failed,
                        pmpix_comm_get_failed_,
                        pmpix_comm_get_failed__,
                        ompix_comm_get_failed_f,
                        (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                        (comm, group, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_GET_FAILED,
                        mpix_comm_get_failed,
                        mpix_comm_get_failed_,
                        mpix_comm_get_failed__,
                        ompix_comm_get_failed_f,
                        (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                        (comm, group, ierr))
#endif

void ompix_comm_get_failed_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr)
{
    MPI_Group c_group;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_get_failed(c_comm,
                                                         &c_group));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *group = PMPI_Group_c2f (c_group);
    }
}
