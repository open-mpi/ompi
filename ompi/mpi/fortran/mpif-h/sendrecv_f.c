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
#pragma weak PMPI_SENDRECV = ompi_sendrecv_f
#pragma weak pmpi_sendrecv = ompi_sendrecv_f
#pragma weak pmpi_sendrecv_ = ompi_sendrecv_f
#pragma weak pmpi_sendrecv__ = ompi_sendrecv_f

#pragma weak PMPI_Sendrecv_f = ompi_sendrecv_f
#pragma weak PMPI_Sendrecv_f08 = ompi_sendrecv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SENDRECV,
                           pmpi_sendrecv,
                           pmpi_sendrecv_,
                           pmpi_sendrecv__,
                           pompi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV = ompi_sendrecv_f
#pragma weak mpi_sendrecv = ompi_sendrecv_f
#pragma weak mpi_sendrecv_ = ompi_sendrecv_f
#pragma weak mpi_sendrecv__ = ompi_sendrecv_f

#pragma weak MPI_Sendrecv_f = ompi_sendrecv_f
#pragma weak MPI_Sendrecv_f08 = ompi_sendrecv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SENDRECV,
                           mpi_sendrecv,
                           mpi_sendrecv_,
                           mpi_sendrecv__,
                           ompi_sendrecv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierr) )
#else
#define ompi_sendrecv_f pompi_sendrecv_f
#endif
#endif


void ompi_sendrecv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		    MPI_Fint *dest, MPI_Fint *sendtag, char *recvbuf,
		    MPI_Fint *recvcount, MPI_Fint *recvtype,
		    MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm,
		    MPI_Fint *status, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Comm c_comm;
   MPI_Datatype c_sendtype = PMPI_Type_f2c(*sendtype);
   MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
   MPI_Status c_status;

   c_comm = PMPI_Comm_f2c (*comm);

   c_ierr = PMPI_Sendrecv(OMPI_F2C_BOTTOM(sendbuf), OMPI_FINT_2_INT(*sendcount),
                         c_sendtype,
                         OMPI_FINT_2_INT(*dest),
                         OMPI_FINT_2_INT(*sendtag),
                         OMPI_F2C_BOTTOM(recvbuf), *recvcount,
                         c_recvtype, OMPI_FINT_2_INT(*source),
                         OMPI_FINT_2_INT(*recvtag),
                         c_comm, &c_status);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr &&
       !OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
       PMPI_Status_c2f(&c_status, status);
   }
}
