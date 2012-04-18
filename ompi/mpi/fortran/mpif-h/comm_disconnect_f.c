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
#pragma weak PMPI_COMM_DISCONNECT = ompi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect = ompi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect_ = ompi_comm_disconnect_f
#pragma weak pmpi_comm_disconnect__ = ompi_comm_disconnect_f

#pragma weak PMPI_Comm_disconnect_f = ompi_comm_disconnect_f
#pragma weak PMPI_Comm_disconnect_f08 = ompi_comm_disconnect_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_DISCONNECT,
                           pmpi_comm_disconnect,
                           pmpi_comm_disconnect_,
                           pmpi_comm_disconnect__,
                           pompi_comm_disconnect_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_DISCONNECT = ompi_comm_disconnect_f
#pragma weak mpi_comm_disconnect = ompi_comm_disconnect_f
#pragma weak mpi_comm_disconnect_ = ompi_comm_disconnect_f
#pragma weak mpi_comm_disconnect__ = ompi_comm_disconnect_f

#pragma weak MPI_Comm_disconnect_f = ompi_comm_disconnect_f
#pragma weak MPI_Comm_disconnect_f08 = ompi_comm_disconnect_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_DISCONNECT,
                           mpi_comm_disconnect,
                           mpi_comm_disconnect_,
                           mpi_comm_disconnect__,
                           ompi_comm_disconnect_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_disconnect_f(MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    
    c_ierr = MPI_Comm_disconnect(&c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *comm = MPI_Comm_c2f(c_comm);
    }
}
