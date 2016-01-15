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
#pragma weak PMPI_SENDRECV_REPLACE = ompi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace = ompi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace_ = ompi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace__ = ompi_sendrecv_replace_f

#pragma weak PMPI_Sendrecv_replace_f = ompi_sendrecv_replace_f
#pragma weak PMPI_Sendrecv_replace_f08 = ompi_sendrecv_replace_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SENDRECV_REPLACE,
                           pmpi_sendrecv_replace,
                           pmpi_sendrecv_replace_,
                           pmpi_sendrecv_replace__,
                           pompi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV_REPLACE = ompi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace = ompi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace_ = ompi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace__ = ompi_sendrecv_replace_f

#pragma weak MPI_Sendrecv_replace_f = ompi_sendrecv_replace_f
#pragma weak MPI_Sendrecv_replace_f08 = ompi_sendrecv_replace_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SENDRECV_REPLACE,
                           mpi_sendrecv_replace,
                           mpi_sendrecv_replace_,
                           mpi_sendrecv_replace__,
                           ompi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#else
#define ompi_sendrecv_replace_f pompi_sendrecv_replace_f
#endif
#endif


void ompi_sendrecv_replace_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
			    MPI_Fint *dest, MPI_Fint *sendtag,
			    MPI_Fint *source, MPI_Fint *recvtag,
			    MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
   MPI_Comm c_comm;
   MPI_Status c_status;

   c_comm = PMPI_Comm_f2c (*comm);

   c_ierr = PMPI_Sendrecv_replace(OMPI_F2C_BOTTOM(buf),
                                 OMPI_FINT_2_INT(*count),
                                 c_type,
                                 OMPI_FINT_2_INT(*dest),
                                 OMPI_FINT_2_INT(*sendtag),
                                 OMPI_FINT_2_INT(*source),
                                 OMPI_FINT_2_INT(*recvtag),
                                 c_comm, &c_status);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr &&
        !OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
      PMPI_Status_c2f(&c_status, status);
   }
}
