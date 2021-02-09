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
#pragma weak PMPIX_COMM_AGREE = ompix_comm_agree_f
#pragma weak pmpix_comm_agree = ompix_comm_agree_f
#pragma weak pmpix_comm_agree_ = ompix_comm_agree_f
#pragma weak pmpix_comm_agree__ = ompix_comm_agree_f
#pragma weak PMPIX_Comm_agree_f = ompix_comm_agree_f
#pragma weak PMPIX_Comm_agree_f08 = ompix_comm_agree_f

#pragma weak MPIX_COMM_AGREE = ompix_comm_agree_f
#pragma weak mpix_comm_agree = ompix_comm_agree_f
#pragma weak mpix_comm_agree_ = ompix_comm_agree_f
#pragma weak mpix_comm_agree__ = ompix_comm_agree_f
#pragma weak MPIX_Comm_agree_f = ompix_comm_agree_f
#pragma weak MPIX_Comm_agree_f08 = ompix_comm_agree_f

#else /* No weak symbols */
OMPI_GENERATE_F77_BINDINGS(PMPIX_COMM_AGREE,
                        pmpix_comm_agree,
                        pmpix_comm_agree_,
                        pmpix_comm_agree__,
                        ompix_comm_agree_f,
                        (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                        (comm, flag, ierr))

OMPI_GENERATE_F77_BINDINGS(MPIX_COMM_AGREE,
                        mpix_comm_agree,
                        mpix_comm_agree_,
                        mpix_comm_agree__,
                        ompix_comm_agree_f,
                        (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                        (comm, flag, ierr))
#endif

void ompix_comm_agree_f(MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    OMPI_LOGICAL_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(PMPIX_Comm_agree(c_comm,
                                             OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag)));
}
