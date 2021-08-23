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
#pragma weak PMPI_SCATTER_INIT = ompi_scatter_init_f
#pragma weak pmpi_scatter_init = ompi_scatter_init_f
#pragma weak pmpi_scatter_init_ = ompi_scatter_init_f
#pragma weak pmpi_scatter_init__ = ompi_scatter_init_f

#pragma weak PMPI_Scatter_init_f = ompi_scatter_init_f
#pragma weak PMPI_Scatter_init_f08 = ompi_scatter_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SCATTER_INIT,
                            pmpi_scatter_init,
                            pmpi_scatter_init_,
                            pmpi_scatter_init__,
                            pompi_scatter_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCATTER_INIT = ompi_scatter_init_f
#pragma weak mpi_scatter_init = ompi_scatter_init_f
#pragma weak mpi_scatter_init_ = ompi_scatter_init_f
#pragma weak mpi_scatter_init__ = ompi_scatter_init_f

#pragma weak MPI_Scatter_init_f = ompi_scatter_init_f
#pragma weak MPI_Scatter_init_f08 = ompi_scatter_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SCATTER_INIT,
                            mpi_scatter_init,
                            mpi_scatter_init_,
                            mpi_scatter_init__,
                            ompi_scatter_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#else
#define ompi_scatter_init_f pompi_scatter_init_f
#endif
#endif


void ompi_scatter_init_f(char *sendbuf, MPI_Fint *sendcount,
                         MPI_Fint *sendtype, char *recvbuf,
                         MPI_Fint *recvcount, MPI_Fint *recvtype,
                         MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request,
                         MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_request;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);
    c_info = PMPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_IN_PLACE(recvbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Scatter_init(sendbuf,OMPI_FINT_2_INT(*sendcount),
                               c_sendtype, recvbuf,
                               OMPI_FINT_2_INT(*recvcount),
                               c_recvtype,
                               OMPI_FINT_2_INT(*root), c_comm, c_info, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);
}
