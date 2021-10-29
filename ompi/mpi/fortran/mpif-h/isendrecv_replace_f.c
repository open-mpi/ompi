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
#pragma weak PMPI_ISENDRECV_REPLACE = ompi_isendrecv_replace_f
#pragma weak pmpi_isendrecv_replace = ompi_isendrecv_replace_f
#pragma weak pmpi_isendrecv_replace_ = ompi_isendrecv_replace_f
#pragma weak pmpi_isendrecv_replace__ = ompi_isendrecv_replace_f

#pragma weak PMPI_Isendrecv_replace_f = ompi_isendrecv_replace_f
#pragma weak PMPI_Isendrecv_replace_f08 = ompi_isendrecv_replace_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ISENDRECV_REPLACE,
                           pmpi_isendrecv_replace,
                           pmpi_isendrecv_replace_,
                           pmpi_isendrecv_replace__,
                           pompi_isendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ISENDRECV_REPLACE = ompi_isendrecv_replace_f
#pragma weak mpi_isendrecv_replace = ompi_isendrecv_replace_f
#pragma weak mpi_isendrecv_replace_ = ompi_isendrecv_replace_f
#pragma weak mpi_isendrecv_replace__ = ompi_isendrecv_replace_f

#pragma weak MPI_Isendrecv_replace_f = ompi_isendrecv_replace_f
#pragma weak MPI_Isendrecv_replace_f08 = ompi_isendrecv_replace_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ISENDRECV_REPLACE,
                           mpi_isendrecv_replace,
                           mpi_isendrecv_replace_,
                           mpi_isendrecv_replace__,
                           ompi_isendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#else
#define ompi_isendrecv_replace_f pompi_isendrecv_replace_f
#endif
#endif


void ompi_isendrecv_replace_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
			    MPI_Fint *dest, MPI_Fint *sendtag,
			    MPI_Fint *source, MPI_Fint *recvtag,
			    MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
   MPI_Comm c_comm;
   MPI_Request c_req;

   c_comm = PMPI_Comm_f2c (*comm);

   c_ierr = PMPI_Isendrecv_replace(OMPI_F2C_BOTTOM(buf),
                                 OMPI_FINT_2_INT(*count),
                                 c_type,
                                 OMPI_FINT_2_INT(*dest),
                                 OMPI_FINT_2_INT(*sendtag),
                                 OMPI_FINT_2_INT(*source),
                                 OMPI_FINT_2_INT(*recvtag),
                                 c_comm, &c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = PMPI_Request_c2f(c_req);
   }

}
