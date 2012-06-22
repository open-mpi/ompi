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
#pragma weak PMPI_ISCATTERV = ompi_iscatterv_f
#pragma weak pmpi_iscatterv = ompi_iscatterv_f
#pragma weak pmpi_iscatterv_ = ompi_iscatterv_f
#pragma weak pmpi_iscatterv__ = ompi_iscatterv_f

#pragma weak PMPI_Iscatterv_f = ompi_iscatterv_f
#pragma weak PMPI_Iscatterv_f08 = ompi_iscatterv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ISCATTERV,
                            pmpi_iscatterv,
                            pmpi_iscatterv_,
                            pmpi_iscatterv__,
                            pompi_iscatterv_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ISCATTERV = ompi_iscatterv_f
#pragma weak mpi_iscatterv = ompi_iscatterv_f
#pragma weak mpi_iscatterv_ = ompi_iscatterv_f
#pragma weak mpi_iscatterv__ = ompi_iscatterv_f

#pragma weak MPI_Iscatterv_f = ompi_iscatterv_f
#pragma weak MPI_Iscatterv_f08 = ompi_iscatterv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ISCATTERV,
                            mpi_iscatterv,
                            mpi_iscatterv_,
                            mpi_iscatterv__,
                            ompi_iscatterv_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_iscatterv_f(char *sendbuf, MPI_Fint *sendcounts,
                      MPI_Fint *displs, MPI_Fint *sendtype,
                      char *recvbuf, MPI_Fint *recvcount, 
                      MPI_Fint *recvtype, MPI_Fint *root,
                      MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Request c_request;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);
    
    c_ierr = MPI_Iscatterv(sendbuf, 
                           OMPI_ARRAY_NAME_CONVERT(sendcounts),
                           OMPI_ARRAY_NAME_CONVERT(displs),
                           c_sendtype, recvbuf,
                           OMPI_FINT_2_INT(*recvcount),
                           c_recvtype, 
                           OMPI_FINT_2_INT(*root), c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = MPI_Request_c2f(c_request);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
