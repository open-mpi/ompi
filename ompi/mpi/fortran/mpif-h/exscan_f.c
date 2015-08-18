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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_EXSCAN = ompi_exscan_f
#pragma weak pmpi_exscan = ompi_exscan_f
#pragma weak pmpi_exscan_ = ompi_exscan_f
#pragma weak pmpi_exscan__ = ompi_exscan_f

#pragma weak PMPI_Exscan_f = ompi_exscan_f
#pragma weak PMPI_Exscan_f08 = ompi_exscan_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_EXSCAN,
                           pmpi_exscan,
                           pmpi_exscan_,
                           pmpi_exscan__,
                           pompi_exscan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_EXSCAN = ompi_exscan_f
#pragma weak mpi_exscan = ompi_exscan_f
#pragma weak mpi_exscan_ = ompi_exscan_f
#pragma weak mpi_exscan__ = ompi_exscan_f

#pragma weak MPI_Exscan_f = ompi_exscan_f
#pragma weak MPI_Exscan_f08 = ompi_exscan_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_EXSCAN,
                           mpi_exscan,
                           mpi_exscan_,
                           mpi_exscan__,
                           ompi_exscan_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_exscan_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
		  MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm,
		  MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);

    /* MPI_IN_PLACE is not supported */
    sendbuf = (char *) OMPI_F2C_BOTTOM (sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM (recvbuf);

    c_ierr = MPI_Exscan(sendbuf, recvbuf,
                        OMPI_FINT_2_INT(*count),
                        c_type, c_op, c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
