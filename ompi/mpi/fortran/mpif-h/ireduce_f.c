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
#pragma weak PMPI_IREDUCE = ompi_ireduce_f
#pragma weak pmpi_ireduce = ompi_ireduce_f
#pragma weak pmpi_ireduce_ = ompi_ireduce_f
#pragma weak pmpi_ireduce__ = ompi_ireduce_f

#pragma weak PMPI_Ireduce_f = ompi_ireduce_f
#pragma weak PMPI_Ireduce_f08 = ompi_ireduce_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IREDUCE,
                            pmpi_ireduce,
                            pmpi_ireduce_,
                            pmpi_ireduce__,
                            pompi_ireduce_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, root, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IREDUCE = ompi_ireduce_f
#pragma weak mpi_ireduce = ompi_ireduce_f
#pragma weak mpi_ireduce_ = ompi_ireduce_f
#pragma weak mpi_ireduce__ = ompi_ireduce_f

#pragma weak MPI_Ireduce_f = ompi_ireduce_f
#pragma weak MPI_Ireduce_f08 = ompi_ireduce_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IREDUCE,
                            mpi_ireduce,
                            mpi_ireduce_,
                            mpi_ireduce__,
                            ompi_ireduce_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, root, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_ireduce_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
                    MPI_Fint *datatype, MPI_Fint *op, 
                    MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, 
                    MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;
    MPI_Request c_request;
    MPI_Op c_op;
    MPI_Comm c_comm;

    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_comm = MPI_Comm_f2c(*comm);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPI_Ireduce(sendbuf, recvbuf,
                         OMPI_FINT_2_INT(*count),
                         c_type, c_op, 
                         OMPI_FINT_2_INT(*root),
                         c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = MPI_Request_c2f(c_request);
}
