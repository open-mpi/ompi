/*
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
#pragma weak PMPIX_COMM_FAILURE_ACK = ompix_comm_failure_ack_f
#pragma weak pmpix_comm_failure_ack = ompix_comm_failure_ack_f
#pragma weak pmpix_comm_failure_ack_ = ompix_comm_failure_ack_f
#pragma weak pmpix_comm_failure_ack__ = ompix_comm_failure_ack_f
#pragma weak PMPIX_Comm_failure_ack_f = ompix_comm_failure_ack_f
#pragma weak PMPIX_Comm_failure_ack_f08 = ompix_comm_failure_ack_f

#pragma weak MPIX_COMM_FAILURE_ACK = ompix_comm_failure_ack_f
#pragma weak mpix_comm_failure_ack = ompix_comm_failure_ack_f
#pragma weak mpix_comm_failure_ack_ = ompix_comm_failure_ack_f
#pragma weak mpix_comm_failure_ack__ = ompix_comm_failure_ack_f
#pragma weak MPIX_Comm_failure_ack_f = ompix_comm_failure_ack_f
#pragma weak MPIX_Comm_failure_ack_f08 = ompix_comm_failure_ack_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_FAILURE_ACK,
                        pmpix_comm_failure_ack,
                        pmpix_comm_failure_ack_,
                        pmpix_comm_failure_ack__,
                        ompix_comm_failure_ack_f,
                        (MPI_Fint *comm, MPI_Fint *ierr),
                        (comm, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_FAILURE_ACK,
                        mpix_comm_failure_ack,
                        mpix_comm_failure_ack_,
                        mpix_comm_failure_ack__,
                        ompix_comm_failure_ack_f,
                        (MPI_Fint *comm, MPI_Fint *ierr),
                        (comm, ierr))
#endif

void ompix_comm_failure_ack_f(MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_failure_ack(c_comm));
}
