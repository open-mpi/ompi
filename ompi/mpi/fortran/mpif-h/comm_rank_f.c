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
#pragma weak PMPI_COMM_RANK = ompi_comm_rank_f
#pragma weak pmpi_comm_rank = ompi_comm_rank_f
#pragma weak pmpi_comm_rank_ = ompi_comm_rank_f
#pragma weak pmpi_comm_rank__ = ompi_comm_rank_f

#pragma weak PMPI_Comm_rank_f = ompi_comm_rank_f
#pragma weak PMPI_Comm_rank_f08 = ompi_comm_rank_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_RANK,
                           pmpi_comm_rank,
                           pmpi_comm_rank_,
                           pmpi_comm_rank__,
                           pompi_comm_rank_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, rank, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_RANK = ompi_comm_rank_f
#pragma weak mpi_comm_rank = ompi_comm_rank_f
#pragma weak mpi_comm_rank_ = ompi_comm_rank_f
#pragma weak mpi_comm_rank__ = ompi_comm_rank_f

#pragma weak MPI_Comm_rank_f = ompi_comm_rank_f
#pragma weak MPI_Comm_rank_f08 = ompi_comm_rank_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_RANK,
                           mpi_comm_rank,
                           mpi_comm_rank_,
                           mpi_comm_rank__,
                           ompi_comm_rank_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, rank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_rank_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = MPI_Comm_f2c( *comm );
    OMPI_SINGLE_NAME_DECL(rank);

    c_ierr = MPI_Comm_rank( c_comm, OMPI_SINGLE_NAME_CONVERT(rank));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(rank);
    }
}
