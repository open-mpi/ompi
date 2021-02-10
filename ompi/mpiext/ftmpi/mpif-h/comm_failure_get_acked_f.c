/*
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2019 The University of Tennessee and the University
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
#pragma weak PMPIX_COMM_FAILURE_GET_ACKED = ompix_comm_failure_get_acked_f
#pragma weak pmpix_comm_failure_get_acked = ompix_comm_failure_get_acked_f
#pragma weak pmpix_comm_failure_get_acked_ = ompix_comm_failure_get_acked_f
#pragma weak pmpix_comm_failure_get_acked__ = ompix_comm_failure_get_acked_f
#pragma weak PMPIX_Comm_failure_get_acked_f = ompix_comm_failure_get_acked_f
#pragma weak PMPIX_Comm_failure_get_acked_f08 = ompix_comm_failure_get_acked_f

#pragma weak MPIX_COMM_FAILURE_GET_ACKED = ompix_comm_failure_get_acked_f
#pragma weak mpix_comm_failure_get_acked = ompix_comm_failure_get_acked_f
#pragma weak mpix_comm_failure_get_acked_ = ompix_comm_failure_get_acked_f
#pragma weak mpix_comm_failure_get_acked__ = ompix_comm_failure_get_acked_f
#pragma weak MPIX_Comm_failure_get_acked_f = ompix_comm_failure_get_acked_f
#pragma weak MPIX_Comm_failure_get_acked_f08 = ompix_comm_failure_get_acked_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_FAILURE_GET_ACKED,
                        pmpix_comm_failure_get_acked,
                        pmpix_comm_failure_get_acked_,
                        pmpix_comm_failure_get_acked__,
                        ompix_comm_failure_get_acked_f,
                        (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                        (comm, group, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_FAILURE_GET_ACKED,
                        mpix_comm_failure_get_acked,
                        mpix_comm_failure_get_acked_,
                        mpix_comm_failure_get_acked__,
                        ompix_comm_failure_get_acked_f,
                        (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr),
                        (comm, group, ierr))
#endif

void ompix_comm_failure_get_acked_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *ierr)
{
    MPI_Group c_group;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_failure_get_acked(c_comm,
                                                         &c_group));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *group = PMPI_Group_c2f (c_group);
    }
}
