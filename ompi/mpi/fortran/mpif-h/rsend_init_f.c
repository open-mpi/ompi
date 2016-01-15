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
#pragma weak PMPI_RSEND_INIT = ompi_rsend_init_f
#pragma weak pmpi_rsend_init = ompi_rsend_init_f
#pragma weak pmpi_rsend_init_ = ompi_rsend_init_f
#pragma weak pmpi_rsend_init__ = ompi_rsend_init_f

#pragma weak PMPI_Rsend_init_f = ompi_rsend_init_f
#pragma weak PMPI_Rsend_init_f08 = ompi_rsend_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_RSEND_INIT,
                           pmpi_rsend_init,
                           pmpi_rsend_init_,
                           pmpi_rsend_init__,
                           pompi_rsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RSEND_INIT = ompi_rsend_init_f
#pragma weak mpi_rsend_init = ompi_rsend_init_f
#pragma weak mpi_rsend_init_ = ompi_rsend_init_f
#pragma weak mpi_rsend_init__ = ompi_rsend_init_f

#pragma weak MPI_Rsend_init_f = ompi_rsend_init_f
#pragma weak MPI_Rsend_init_f08 = ompi_rsend_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_RSEND_INIT,
                           mpi_rsend_init,
                           mpi_rsend_init_,
                           mpi_rsend_init__,
                           ompi_rsend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#else
#define ompi_rsend_init_f pompi_rsend_init_f
#endif
#endif


void ompi_rsend_init_f(char *buf, MPI_Fint *count,
		      MPI_Fint *datatype, MPI_Fint *dest,
		      MPI_Fint *tag, MPI_Fint *comm,
		      MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
   MPI_Request c_req;
   MPI_Comm c_comm;

   c_comm = PMPI_Comm_f2c (*comm);

   c_ierr = PMPI_Rsend_init(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                           c_type, OMPI_FINT_2_INT(*dest),
                           OMPI_FINT_2_INT(*tag),
                           c_comm, &c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = PMPI_Request_c2f(c_req);
   }
}
