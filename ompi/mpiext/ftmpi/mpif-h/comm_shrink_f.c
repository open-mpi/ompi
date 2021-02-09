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
#pragma weak PMPIX_COMM_SHRINK = ompix_comm_shrink_f
#pragma weak pmpix_comm_shrink = ompix_comm_shrink_f
#pragma weak pmpix_comm_shrink_ = ompix_comm_shrink_f
#pragma weak pmpix_comm_shrink__ = ompix_comm_shrink_f
#pragma weak PMPIX_Comm_shrink_f = ompix_comm_shrink_f
#pragma weak PMPIX_Comm_shrink_f08 = ompix_comm_shrink_f

#pragma weak MPIX_COMM_SHRINK = ompix_comm_shrink_f
#pragma weak mpix_comm_shrink = ompix_comm_shrink_f
#pragma weak mpix_comm_shrink_ = ompix_comm_shrink_f
#pragma weak mpix_comm_shrink__ = ompix_comm_shrink_f
#pragma weak MPIX_Comm_shrink_f = ompix_comm_shrink_f
#pragma weak MPIX_Comm_shrink_f08 = ompix_comm_shrink_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_SHRINK,
                        pmpix_comm_shrink,
                        pmpix_comm_shrink_,
                        pmpix_comm_shrink__,
                        ompix_comm_shrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                        (comm, newcomm, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_SHRINK,
                        mpix_comm_shrink,
                        mpix_comm_shrink_,
                        mpix_comm_shrink__,
                        ompix_comm_shrink_f,
                        (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                        (comm, newcomm, ierr))
#endif

void ompix_comm_shrink_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr)
{
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_shrink(c_comm,
                                              &c_newcomm));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newcomm = PMPI_Comm_c2f(c_newcomm);
    }
}
