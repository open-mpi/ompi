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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_CALL_ERRHANDLER = ompi_comm_call_errhandler_f
#pragma weak pmpi_comm_call_errhandler = ompi_comm_call_errhandler_f
#pragma weak pmpi_comm_call_errhandler_ = ompi_comm_call_errhandler_f
#pragma weak pmpi_comm_call_errhandler__ = ompi_comm_call_errhandler_f

#pragma weak PMPI_Comm_call_errhandler_f = ompi_comm_call_errhandler_f
#pragma weak PMPI_Comm_call_errhandler_f08 = ompi_comm_call_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CALL_ERRHANDLER,
                            pmpi_comm_call_errhandler,
                            pmpi_comm_call_errhandler_,
                            pmpi_comm_call_errhandler__,
                            pompi_comm_call_errhandler_f,
                            (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                            (comm, errorcode, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CALL_ERRHANDLER = ompi_comm_call_errhandler_f
#pragma weak mpi_comm_call_errhandler = ompi_comm_call_errhandler_f
#pragma weak mpi_comm_call_errhandler_ = ompi_comm_call_errhandler_f
#pragma weak mpi_comm_call_errhandler__ = ompi_comm_call_errhandler_f

#pragma weak MPI_Comm_call_errhandler_f = ompi_comm_call_errhandler_f
#pragma weak MPI_Comm_call_errhandler_f08 = ompi_comm_call_errhandler_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
   OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CALL_ERRHANDLER,
                               mpi_comm_call_errhandler,
                               mpi_comm_call_errhandler_,
                               mpi_comm_call_errhandler__,
                               ompi_comm_call_errhandler_f,
                               (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                               (comm, errorcode, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_call_errhandler_f(MPI_Fint *comm, MPI_Fint *errorcode,
                                 MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Comm c_comm;

   c_comm = MPI_Comm_f2c(*comm);

   c_ierr = MPI_Comm_call_errhandler(c_comm, OMPI_FINT_2_INT(*errorcode));
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
