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
#pragma weak PMPI_IMRECV = ompi_imrecv_f
#pragma weak pmpi_imrecv = ompi_imrecv_f
#pragma weak pmpi_imrecv_ = ompi_imrecv_f
#pragma weak pmpi_imrecv__ = ompi_imrecv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IMRECV,
                            pmpi_imrecv,
                            pmpi_imrecv_,
                            pmpi_imrecv__,
                            pompi_imrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *message,
                             MPI_Fint *request, MPI_Fint *ierr),
                            (buf, count, datatype, message, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IMRECV = ompi_imrecv_f
#pragma weak mpi_imrecv = ompi_imrecv_f
#pragma weak mpi_imrecv_ = ompi_imrecv_f
#pragma weak mpi_imrecv__ = ompi_imrecv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IMRECV,
                            mpi_imrecv,
                            mpi_imrecv_,
                            mpi_imrecv__,
                            ompi_imrecv_f,
                            (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *message,
                             MPI_Fint *request, MPI_Fint *ierr),
                            (buf, count, datatype, message, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_imrecv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
                   MPI_Fint *message, MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = MPI_Type_f2c(*datatype);
   MPI_Request c_req;
   MPI_Message c_message;

   c_message = MPI_Message_f2c(*message);

   c_ierr = OMPI_INT_2_FINT(MPI_Imrecv(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                                       c_type, &c_message, &c_req));
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = MPI_Request_c2f(c_req);
      /* message is an INOUT, and may be updated by the recv */
      *message = MPI_Message_c2f(c_message);
   }
}
