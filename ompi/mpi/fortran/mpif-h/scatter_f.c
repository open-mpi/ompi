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
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
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
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_SCATTER = ompi_scatter_f
#pragma weak pmpi_scatter = ompi_scatter_f
#pragma weak pmpi_scatter_ = ompi_scatter_f
#pragma weak pmpi_scatter__ = ompi_scatter_f

#pragma weak PMPI_Scatter_f = ompi_scatter_f
#pragma weak PMPI_Scatter_f08 = ompi_scatter_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SCATTER,
                           pmpi_scatter,
                           pmpi_scatter_,
                           pmpi_scatter__,
                           pompi_scatter_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCATTER = ompi_scatter_f
#pragma weak mpi_scatter = ompi_scatter_f
#pragma weak mpi_scatter_ = ompi_scatter_f
#pragma weak mpi_scatter__ = ompi_scatter_f

#pragma weak MPI_Scatter_f = ompi_scatter_f
#pragma weak MPI_Scatter_f08 = ompi_scatter_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SCATTER,
                           mpi_scatter,
                           mpi_scatter_,
                           mpi_scatter__,
                           ompi_scatter_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#else
#define ompi_scatter_f pompi_scatter_f
#endif
#endif


void ompi_scatter_f(char *sendbuf, MPI_Fint *sendcount,
		   MPI_Fint *sendtype, char *recvbuf,
		   MPI_Fint *recvcount, MPI_Fint *recvtype,
		   MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    int c_root = OMPI_FINT_2_INT(*root);
    MPI_Datatype c_sendtype = NULL, c_recvtype = NULL;
    int c_sendcount = 0, c_recvcount = 0;

    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        } else if (MPI_PROC_NULL != c_root) {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
        }
    } else {
        if (OMPI_IS_FORTRAN_IN_PLACE(recvbuf)) {
            recvbuf = MPI_IN_PLACE;
        } else {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
        }
        if (ompi_comm_rank(c_comm) == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Scatter(sendbuf,c_sendcount, c_sendtype,
                          recvbuf, c_recvcount, c_recvtype,
                          c_root, c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
