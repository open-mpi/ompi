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
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/mpif-h/status-conversion.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_MRECV = ompi_mrecv_f
#pragma weak pmpi_mrecv = ompi_mrecv_f
#pragma weak pmpi_mrecv_ = ompi_mrecv_f
#pragma weak pmpi_mrecv__ = ompi_mrecv_f

#pragma weak PMPI_Mrecv_f = ompi_mrecv_f
#pragma weak PMPI_Mrecv_f08 = ompi_mrecv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_MRECV,
                            pmpi_mrecv,
                            pmpi_mrecv_,
                            pmpi_mrecv__,
                            pompi_mrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *message,
                             MPI_Fint *status, MPI_Fint *ierr),
                            (buf, count, datatype, message, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_MRECV = ompi_mrecv_f
#pragma weak mpi_mrecv = ompi_mrecv_f
#pragma weak mpi_mrecv_ = ompi_mrecv_f
#pragma weak mpi_mrecv__ = ompi_mrecv_f

#pragma weak MPI_Mrecv_f = ompi_mrecv_f
#pragma weak MPI_Mrecv_f08 = ompi_mrecv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_MRECV,
                            mpi_mrecv,
                            mpi_mrecv_,
                            mpi_mrecv__,
                            ompi_mrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype,
                             MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr),
                            (buf, count, datatype, message, status, ierr) )
#else
#define ompi_mrecv_f pompi_mrecv_f
#endif
#endif


void ompi_mrecv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
                  MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr)
{
   int c_ierr;
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)
   MPI_Message c_message = PMPI_Message_f2c(*message);
   MPI_Datatype c_type = PMPI_Type_f2c(*datatype);

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)

   /* Call the C function */
   c_ierr = OMPI_INT_2_FINT(PMPI_Mrecv(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                                       c_type, &c_message,
                                       c_status));
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
      /* message is an INOUT, and may be updated by the recv */
      *message = PMPI_Message_c2f(c_message);
   }
}
