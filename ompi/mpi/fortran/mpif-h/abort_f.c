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
#pragma weak PMPI_ABORT = ompi_abort_f
#pragma weak pmpi_abort = ompi_abort_f
#pragma weak pmpi_abort_ = ompi_abort_f
#pragma weak pmpi_abort__ = ompi_abort_f

#pragma weak PMPI_Abort_f = ompi_abort_f
#pragma weak PMPI_Abort_f08 = ompi_abort_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS(PMPI_ABORT,
                           pmpi_abort,
                           pmpi_abort_,
                           pmpi_abort__,
                           pompi_abort_f,
                           (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (comm, errorcode, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ABORT = ompi_abort_f
#pragma weak mpi_abort = ompi_abort_f
#pragma weak mpi_abort_ = ompi_abort_f
#pragma weak mpi_abort__ = ompi_abort_f

#pragma weak MPI_Abort_f = ompi_abort_f
#pragma weak MPI_Abort_f08 = ompi_abort_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS(MPI_ABORT,
                           mpi_abort,
                           mpi_abort_,
                           mpi_abort__,
                           ompi_abort_f,
                           (MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (comm, errorcode, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif


void ompi_abort_f(MPI_Fint *comm, MPI_Fint *errorcode, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    
    ierr_c = MPI_Abort(c_comm, OMPI_FINT_2_INT(*errorcode));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
}
