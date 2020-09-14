/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
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
#pragma weak PMPI_PARRIVED = ompi_parrived_f
#pragma weak pmpi_parrived = ompi_parrived_f
#pragma weak pmpi_parrived_ = ompi_parrived_f
#pragma weak pmpi_parrived__ = ompi_parrived_f

#pragma weak PMPI_Parrived_f = ompi_parrived_f
#pragma weak PMPI_Parrived_f08 = ompi_parrived_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PARRIVED,
                           pmpi_parrived,
                           pmpi_parrived_,
                           pmpi_parrived__,
                           pompi_parrived_f,
                           (MPI_Fint *request, MPI_Fint *partition, MPI_Fint *flag, MPI_Fint *ierr),
                           (request, partition, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PARRIVED = ompi_parrived_f
#pragma weak mpi_parrived = ompi_parrived_f
#pragma weak mpi_parrived_ = ompi_parrived_f
#pragma weak mpi_parrived__ = ompi_parrived_f

#pragma weak MPI_Parrived_f = ompi_parrived_f
#pragma weak MPI_Parrived_f08 = ompi_parrived_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PARRIVED,
                           mpi_parrived,
                           mpi_parrived_,
                           mpi_parrived__,
                           ompi_parrived_f,
                           (MPI_Fint *request, MPI_Fint *partition, MPI_Fint *flag, MPI_Fint *ierr),
                           (request, partition, flag, ierr) )
#else
#define ompi_parrived_f pompi_parrived_f
#endif
#endif


void ompi_parrived_f(MPI_Fint *request, MPI_Fint *partition, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = PMPI_Request_f2c(*request);
    OMPI_LOGICAL_NAME_DECL(flag);

    c_ierr = PMPI_Parrived(c_req, OMPI_FINT_2_INT(*partition), OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));

    OMPI_SINGLE_INT_2_LOGICAL(flag);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr && *flag) {
        *request = OMPI_INT_2_FINT(c_req->req_f_to_c_index);
    }
}
