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
#pragma weak PMPI_SSEND_INIT = ompi_ssend_init_f
#pragma weak pmpi_ssend_init = ompi_ssend_init_f
#pragma weak pmpi_ssend_init_ = ompi_ssend_init_f
#pragma weak pmpi_ssend_init__ = ompi_ssend_init_f

#pragma weak PMPI_Ssend_init_f = ompi_ssend_init_f
#pragma weak PMPI_Ssend_init_f08 = ompi_ssend_init_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SSEND_INIT,
                           pmpi_ssend_init,
                           pmpi_ssend_init_,
                           pmpi_ssend_init__,
                           pompi_ssend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SSEND_INIT = ompi_ssend_init_f
#pragma weak mpi_ssend_init = ompi_ssend_init_f
#pragma weak mpi_ssend_init_ = ompi_ssend_init_f
#pragma weak mpi_ssend_init__ = ompi_ssend_init_f

#pragma weak MPI_Ssend_init_f = ompi_ssend_init_f
#pragma weak MPI_Ssend_init_f08 = ompi_ssend_init_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SSEND_INIT,
                           mpi_ssend_init,
                           mpi_ssend_init_,
                           mpi_ssend_init__,
                           ompi_ssend_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_ssend_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *dest, MPI_Fint *tag, 
		      MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_type = MPI_Type_f2c(*datatype);
   MPI_Request c_req;
   MPI_Comm c_comm;

   c_comm = MPI_Comm_f2c (*comm);

   c_ierr = MPI_Ssend_init(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                           c_type, OMPI_FINT_2_INT(*dest),
                           OMPI_FINT_2_INT(*tag),
                           c_comm, &c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
      *request = MPI_Request_c2f(c_req);
   }
}
