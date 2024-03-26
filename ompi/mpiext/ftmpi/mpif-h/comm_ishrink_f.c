/*
 * Copyright (c) 2010-2022 The University of Tennessee and the University
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

#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_COMM_ISHRINK = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink_ = ompix_comm_ishrink_f
#pragma weak pmpix_comm_ishrink__ = ompix_comm_ishrink_f
#pragma weak PMPIX_Comm_ishrink_f = ompix_comm_ishrink_f
#pragma weak PMPIX_Comm_ishrink_f08 = ompix_comm_ishrink_f

#pragma weak MPIX_COMM_ISHRINK = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink_ = ompix_comm_ishrink_f
#pragma weak mpix_comm_ishrink__ = ompix_comm_ishrink_f
#pragma weak MPIX_Comm_ishrink_f = ompix_comm_ishrink_f
#pragma weak MPIX_Comm_ishrink_f08 = ompix_comm_ishrink_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_ISHRINK,
                        pmpix_comm_ishrink,
                        pmpix_comm_ishrink_,
                        pmpix_comm_ishrink__,
                        ompix_comm_ishrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, newcomm, request, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_ISHRINK,
                        mpix_comm_ishrink,
                        mpix_comm_ishrink_,
                        mpix_comm_ishrink__,
                        ompix_comm_ishrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, newcomm, request, ierr))
#endif

void ompix_comm_ishrink_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;
    MPI_Comm c_newcomm;

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_ishrink(c_comm,
                                               &c_newcomm,
                                               &c_req));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *request = PMPI_Request_c2f(c_req);
        *newcomm = PMPI_Comm_c2f(c_newcomm);
    }
    else {
        *newcomm = PMPI_Comm_c2f(&ompi_mpi_comm_null.comm);
    }
}
