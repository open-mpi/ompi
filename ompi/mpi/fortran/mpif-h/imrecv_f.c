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
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      FUJITSU LIMITED.  All rights reserved.
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
#pragma weak PMPI_IMRECV = ompi_imrecv_f
#pragma weak pmpi_imrecv = ompi_imrecv_f
#pragma weak pmpi_imrecv_ = ompi_imrecv_f
#pragma weak pmpi_imrecv__ = ompi_imrecv_f

#pragma weak PMPI_Imrecv_f = ompi_imrecv_f
#pragma weak PMPI_Imrecv_f08 = ompi_imrecv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_IMRECV,
                            pmpi_imrecv,
                            pmpi_imrecv_,
                            pmpi_imrecv__,
                            pompi_imrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *message,
                             MPI_Fint *request, MPI_Fint *ierr),
                            (buf, count, datatype, message, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IMRECV = ompi_imrecv_f
#pragma weak mpi_imrecv = ompi_imrecv_f
#pragma weak mpi_imrecv_ = ompi_imrecv_f
#pragma weak mpi_imrecv__ = ompi_imrecv_f

#pragma weak MPI_Imrecv_f = ompi_imrecv_f
#pragma weak MPI_Imrecv_f08 = ompi_imrecv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_IMRECV,
                            mpi_imrecv,
                            mpi_imrecv_,
                            mpi_imrecv__,
                            ompi_imrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *message,
                             MPI_Fint *request, MPI_Fint *ierr),
                            (buf, count, datatype, message, request, ierr) )
#else
#define ompi_imrecv_f pompi_imrecv_f
#endif
#endif


void ompi_imrecv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
                   MPI_Fint *message, MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
   MPI_Request c_req;
   MPI_Message c_message;

   c_message = PMPI_Message_f2c(*message);

   c_ierr = PMPI_Imrecv(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                        c_type, &c_message, &c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = PMPI_Request_c2f(c_req);
      /* message is an INOUT, and may be updated by the recv */
      *message = PMPI_Message_c2f(c_message);
   }
}
