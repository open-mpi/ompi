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
#pragma weak PMPIX_COMM_IAGREE = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree_ = ompix_comm_iagree_f
#pragma weak pmpix_comm_iagree__ = ompix_comm_iagree_f
#pragma weak PMPIX_Comm_iagree_f = ompix_comm_iagree_f
#pragma weak PMPIX_Comm_iagree_f08 = ompix_comm_iagree_f

#pragma weak MPIX_COMM_IAGREE = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree_ = ompix_comm_iagree_f
#pragma weak mpix_comm_iagree__ = ompix_comm_iagree_f
#pragma weak MPIX_Comm_iagree_f = ompix_comm_iagree_f
#pragma weak MPIX_Comm_iagree_f08 = ompix_comm_iagree_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_IAGREE,
                        mpix_comm_iagree,
                        mpix_comm_iagree_,
                        mpix_comm_iagree__,
                        ompix_comm_iagree_f,
                        (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, flag, request, ierr))

OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_IAGREE,
                        pmpix_comm_iagree,
                        pmpix_comm_iagree_,
                        pmpix_comm_iagree__,
                        ompix_comm_iagree_f,
                        (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *request, MPI_Fint *ierr),
                        (comm, flag, request, ierr))
#endif

void ompix_comm_iagree_f(MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;
    OMPI_LOGICAL_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_iagree(c_comm,
                                              OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                                              &c_req));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *request = PMPI_Request_c2f(c_req);
    }
}
