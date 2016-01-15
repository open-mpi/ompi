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
#pragma weak PMPI_SCAN = ompi_scan_f
#pragma weak pmpi_scan = ompi_scan_f
#pragma weak pmpi_scan_ = ompi_scan_f
#pragma weak pmpi_scan__ = ompi_scan_f

#pragma weak PMPI_Scan_f = ompi_scan_f
#pragma weak PMPI_Scan_f08 = ompi_scan_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SCAN,
                           pmpi_scan,
                           pmpi_scan_,
                           pmpi_scan__,
                           pompi_scan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCAN = ompi_scan_f
#pragma weak mpi_scan = ompi_scan_f
#pragma weak mpi_scan_ = ompi_scan_f
#pragma weak mpi_scan__ = ompi_scan_f

#pragma weak MPI_Scan_f = ompi_scan_f
#pragma weak MPI_Scan_f08 = ompi_scan_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SCAN,
                           mpi_scan,
                           mpi_scan_,
                           mpi_scan__,
                           ompi_scan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#else
#define ompi_scan_f pompi_scan_f
#endif
#endif


void ompi_scan_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
		MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm,
		MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;

    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);
    c_comm = PMPI_Comm_f2c(*comm);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Scan(sendbuf, recvbuf,
                      OMPI_FINT_2_INT(*count),
                      c_type, c_op,
                      c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
