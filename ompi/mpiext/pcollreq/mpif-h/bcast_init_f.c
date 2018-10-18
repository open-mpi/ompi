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
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpiext/pcollreq/mpif-h/mpiext_pcollreq_prototypes.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_BCAST_INIT = ompix_bcast_init_f
#pragma weak pmpix_bcast_init = ompix_bcast_init_f
#pragma weak pmpix_bcast_init_ = ompix_bcast_init_f
#pragma weak pmpix_bcast_init__ = ompix_bcast_init_f

#pragma weak PMPIX_Bcast_init_f = ompix_bcast_init_f
#pragma weak PMPIX_Bcast_init_f08 = ompix_bcast_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPIX_BCAST_INIT,
                            pmpix_bcast_init,
                            pmpix_bcast_init_,
                            pmpix_bcast_init__,
                            pompix_bcast_init_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_BCAST_INIT = ompix_bcast_init_f
#pragma weak mpix_bcast_init = ompix_bcast_init_f
#pragma weak mpix_bcast_init_ = ompix_bcast_init_f
#pragma weak mpix_bcast_init__ = ompix_bcast_init_f

#pragma weak MPIX_Bcast_init_f = ompix_bcast_init_f
#pragma weak MPIX_Bcast_init_f08 = ompix_bcast_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPIX_BCAST_INIT,
                            mpix_bcast_init,
                            mpix_bcast_init_,
                            mpix_bcast_init__,
                            ompix_bcast_init_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, info, request, ierr) )
#else
#define ompix_bcast_init_f pompix_bcast_init_f
#endif
#endif


void ompix_bcast_init_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype,
                   MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request,
                   MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Info c_info;
    MPI_Request c_req;
    MPI_Datatype c_type;

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);
    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPIX_Bcast_init(OMPI_F2C_BOTTOM(buffer),
                              OMPI_FINT_2_INT(*count),
                              c_type,
                              OMPI_FINT_2_INT(*root),
                              c_comm,
                              c_info,
                              &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_req);
}
