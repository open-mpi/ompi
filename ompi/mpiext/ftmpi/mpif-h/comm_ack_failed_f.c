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
#pragma weak PMPIX_COMM_ACK_FAILED = ompix_comm_ack_failed_f
#pragma weak pmpix_comm_ack_failed = ompix_comm_ack_failed_f
#pragma weak pmpix_comm_ack_failed_ = ompix_comm_ack_failed_f
#pragma weak pmpix_comm_ack_failed__ = ompix_comm_ack_failed_f
#pragma weak PMPIX_Comm_ack_failed_f = ompix_comm_ack_failed_f
#pragma weak PMPIX_Comm_ack_failed_f08 = ompix_comm_ack_failed_f

#pragma weak MPIX_COMM_ACK_FAILED = ompix_comm_ack_failed_f
#pragma weak mpix_comm_ack_failed = ompix_comm_ack_failed_f
#pragma weak mpix_comm_ack_failed_ = ompix_comm_ack_failed_f
#pragma weak mpix_comm_ack_failed__ = ompix_comm_ack_failed_f
#pragma weak MPIX_Comm_ack_failed_f = ompix_comm_ack_failed_f
#pragma weak MPIX_Comm_ack_failed_f08 = ompix_comm_ack_failed_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_ACK_FAILED,
                        pmpix_comm_ack_failed,
                        pmpix_comm_ack_failed_,
                        pmpix_comm_ack_failed__,
                        ompix_comm_ack_failed_f,
                        (MPI_Fint *comm, MPI_Fint *num_to_ack, MPI_Fint *num_acked, MPI_Fint *ierr),
                        (comm, num_to_ack, num_acked, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_ACK_FAILED,
                        mpix_comm_ack_failed,
                        mpix_comm_ack_failed_,
                        mpix_comm_ack_failed__,
                        ompix_comm_ack_failed_f,
                        (MPI_Fint *comm, MPI_Fint *num_to_ack, MPI_Fint *num_acked, MPI_Fint *ierr),
                        (comm, num_to_ack, num_acked, ierr))
#endif

void ompix_comm_ack_failed_f(MPI_Fint *comm, MPI_Fint *num_to_ack, MPI_Fint *num_acked, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_ack_failed(c_comm, OMPI_FINT_2_INT(*num_to_ack), OMPI_PFINT_2_PINT(num_acked)));
}
