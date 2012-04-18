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
#pragma weak PMPI_COMM_JOIN = ompi_comm_join_f
#pragma weak pmpi_comm_join = ompi_comm_join_f
#pragma weak pmpi_comm_join_ = ompi_comm_join_f
#pragma weak pmpi_comm_join__ = ompi_comm_join_f

#pragma weak PMPI_Comm_join_f = ompi_comm_join_f
#pragma weak PMPI_Comm_join_f08 = ompi_comm_join_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_JOIN,
                           pmpi_comm_join,
                           pmpi_comm_join_,
                           pmpi_comm_join__,
                           pompi_comm_join_f,
                           (MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr),
                           (fd, intercomm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_JOIN = ompi_comm_join_f
#pragma weak mpi_comm_join = ompi_comm_join_f
#pragma weak mpi_comm_join_ = ompi_comm_join_f
#pragma weak mpi_comm_join__ = ompi_comm_join_f

#pragma weak MPI_Comm_join_f = ompi_comm_join_f
#pragma weak MPI_Comm_join_f08 = ompi_comm_join_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_JOIN,
                           mpi_comm_join,
                           mpi_comm_join_,
                           mpi_comm_join__,
                           ompi_comm_join_f,
                           (MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr),
                           (fd, intercomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_join_f(MPI_Fint *fd, MPI_Fint *intercomm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_intercomm;

    c_ierr = MPI_Comm_join(OMPI_FINT_2_INT(*fd), &c_intercomm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *intercomm = MPI_Comm_c2f(c_intercomm);
    }
}
