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
#pragma weak PMPI_ALLREDUCE_INIT = ompi_allreduce_init_f
#pragma weak pmpi_allreduce_init = ompi_allreduce_init_f
#pragma weak pmpi_allreduce_init_ = ompi_allreduce_init_f
#pragma weak pmpi_allreduce_init__ = ompi_allreduce_init_f

#pragma weak PMPI_Allreduce_init_f = ompi_allreduce_init_f
#pragma weak PMPI_Allreduce_init_f08 = ompi_allreduce_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLREDUCE_INIT,
                            pmpi_allreduce_init,
                            pmpi_allreduce_init_,
                            pmpi_allreduce_init__,
                            pompi_allreduce_init_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLREDUCE_INIT = ompi_allreduce_init_f
#pragma weak mpi_allreduce_init = ompi_allreduce_init_f
#pragma weak mpi_allreduce_init_ = ompi_allreduce_init_f
#pragma weak mpi_allreduce_init__ = ompi_allreduce_init_f

#pragma weak MPI_Allreduce_init_f = ompi_allreduce_init_f
#pragma weak MPI_Allreduce_init_f08 = ompi_allreduce_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ALLREDUCE_INIT,
                            mpi_allreduce_init,
                            mpi_allreduce_init_,
                            mpi_allreduce_init__,
                            ompi_allreduce_init_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr) )
#else
#define ompi_allreduce_init_f pompi_allreduce_init_f
#endif
#endif


void ompi_allreduce_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
                           MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm,
                           MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Info c_info;
    MPI_Request c_request;
    MPI_Op c_op;

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);
    c_info = PMPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = PMPI_Allreduce_init(sendbuf, recvbuf,
                                 OMPI_FINT_2_INT(*count),
                                 c_type, c_op, c_comm, c_info, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_request);
}
