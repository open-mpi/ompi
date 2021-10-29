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
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
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
#pragma weak PMPI_ISENDRECV = ompi_isendrecv_f
#pragma weak pmpi_isendrecv = ompi_isendrecv_f
#pragma weak pmpi_isendrecv_ = ompi_isendrecv_f
#pragma weak pmpi_isendrecv__ = ompi_isendrecv_f

#pragma weak PMPI_Isendrecv_f = ompi_isendrecv_f
#pragma weak PMPI_Isendrecv_f08 = ompi_isendrecv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ISENDRECV,
                           pmpi_isendrecv,
                           pmpi_isendrecv_,
                           pmpi_isendrecv__,
                           pompi_isendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ISENDRECV = ompi_isendrecv_f
#pragma weak mpi_isendrecv = ompi_isendrecv_f
#pragma weak mpi_isendrecv_ = ompi_isendrecv_f
#pragma weak mpi_isendrecv__ = ompi_isendrecv_f

#pragma weak MPI_Isendrecv_f = ompi_isendrecv_f
#pragma weak MPI_Isendrecv_f08 = ompi_isendrecv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ISENDRECV,
                           mpi_isendrecv,
                           mpi_isendrecv_,
                           mpi_isendrecv__,
                           ompi_isendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, request, ierr) )
#else
#define ompi_isendrecv_f pompi_isendrecv_f
#endif
#endif


void ompi_isendrecv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		    MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf,
		    MPI_Fint *recvcount, MPI_Fint *recvtype,
		    MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm,
		    MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Comm c_comm;
   MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
   MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
   MPI_Request c_req;

   c_comm = PMPI_Comm_f2c (*comm);

   c_ierr = PMPI_Isendrecv(OMPI_F2C_BOTTOM(sendbuf), OMPI_FINT_2_INT(*sendcount),
                         c_sendtype,
                         OMPI_FINT_2_INT(*dest),
                         OMPI_FINT_2_INT(*sendtag),
                         OMPI_F2C_BOTTOM(recvbuf), OMPI_FINT_2_INT(*recvcount),
                         c_recvtype, OMPI_FINT_2_INT(*source),
                         OMPI_FINT_2_INT(*recvtag),
                         c_comm, &c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = PMPI_Request_c2f(c_req);
   }
}
