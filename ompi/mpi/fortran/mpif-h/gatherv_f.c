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
#pragma weak PMPI_GATHERV = ompi_gatherv_f
#pragma weak pmpi_gatherv = ompi_gatherv_f
#pragma weak pmpi_gatherv_ = ompi_gatherv_f
#pragma weak pmpi_gatherv__ = ompi_gatherv_f

#pragma weak PMPI_Gatherv_f = ompi_gatherv_f
#pragma weak PMPI_Gatherv_f08 = ompi_gatherv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GATHERV,
                           pmpi_gatherv,
                           pmpi_gatherv_,
                           pmpi_gatherv__,
                           pompi_gatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GATHERV = ompi_gatherv_f
#pragma weak mpi_gatherv = ompi_gatherv_f
#pragma weak mpi_gatherv_ = ompi_gatherv_f
#pragma weak mpi_gatherv__ = ompi_gatherv_f

#pragma weak MPI_Gatherv_f = ompi_gatherv_f
#pragma weak MPI_Gatherv_f08 = ompi_gatherv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GATHERV,
                           mpi_gatherv,
                           mpi_gatherv_,
                           mpi_gatherv__,
                           ompi_gatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_gatherv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		   char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
		   MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm,
		   MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPI_Gatherv(sendbuf, OMPI_FINT_2_INT(*sendcount),
                         c_sendtype, recvbuf,
                         OMPI_ARRAY_NAME_CONVERT(recvcounts),
                         OMPI_ARRAY_NAME_CONVERT(displs),
                         c_recvtype,
                         OMPI_FINT_2_INT(*root),
                         c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
