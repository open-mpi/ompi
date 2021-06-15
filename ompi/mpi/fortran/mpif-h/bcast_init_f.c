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
 * Copyright (c) 2015-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#pragma weak PMPI_BCAST_INIT = ompi_bcast_init_f
#pragma weak pmpi_bcast_init = ompi_bcast_init_f
#pragma weak pmpi_bcast_init_ = ompi_bcast_init_f
#pragma weak pmpi_bcast_init__ = ompi_bcast_init_f

#pragma weak PMPI_Bcast_init_f = ompi_bcast_init_f
#pragma weak PMPI_Bcast_init_f08 = ompi_bcast_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BCAST_INIT,
                            pmpi_bcast_init,
                            pmpi_bcast_init_,
                            pmpi_bcast_init__,
                            pompi_bcast_init_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BCAST_INIT = ompi_bcast_init_f
#pragma weak mpi_bcast_init = ompi_bcast_init_f
#pragma weak mpi_bcast_init_ = ompi_bcast_init_f
#pragma weak mpi_bcast_init__ = ompi_bcast_init_f

#pragma weak MPI_Bcast_init_f = ompi_bcast_init_f
#pragma weak MPI_Bcast_init_f08 = ompi_bcast_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BCAST_INIT,
                            mpi_bcast_init,
                            mpi_bcast_init_,
                            mpi_bcast_init__,
                            ompi_bcast_init_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, info, request, ierr) )
#else
#define ompi_bcast_init_f pompi_bcast_init_f
#endif
#endif


void ompi_bcast_init_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype,
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

    c_ierr = PMPI_Bcast_init(OMPI_F2C_BOTTOM(buffer),
                             OMPI_FINT_2_INT(*count),
                             c_type,
                             OMPI_FINT_2_INT(*root),
                             c_comm,
                             c_info,
                             &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_req);
}
