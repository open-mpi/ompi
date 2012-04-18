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
#pragma weak PMPI_CART_RANK = ompi_cart_rank_f
#pragma weak pmpi_cart_rank = ompi_cart_rank_f
#pragma weak pmpi_cart_rank_ = ompi_cart_rank_f
#pragma weak pmpi_cart_rank__ = ompi_cart_rank_f

#pragma weak PMPI_Cart_rank_f = ompi_cart_rank_f
#pragma weak PMPI_Cart_rank_f08 = ompi_cart_rank_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_RANK,
                           pmpi_cart_rank,
                           pmpi_cart_rank_,
                           pmpi_cart_rank__,
                           pompi_cart_rank_f,
                           (MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, coords, rank, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_RANK = ompi_cart_rank_f
#pragma weak mpi_cart_rank = ompi_cart_rank_f
#pragma weak mpi_cart_rank_ = ompi_cart_rank_f
#pragma weak mpi_cart_rank__ = ompi_cart_rank_f

#pragma weak MPI_Cart_rank_f = ompi_cart_rank_f
#pragma weak MPI_Cart_rank_f08 = ompi_cart_rank_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_RANK,
			    mpi_cart_rank,
			    mpi_cart_rank_,
			    mpi_cart_rank__,
			    ompi_cart_rank_f,
			    (MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank, MPI_Fint *ierr),
			    (comm, coords, rank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_cart_rank_f(MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank,
		     MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    int ndims, c_ierr;
    OMPI_ARRAY_NAME_DECL(coords);
    OMPI_SINGLE_NAME_DECL(rank);

    c_comm = MPI_Comm_f2c(*comm);

    MPI_Cartdim_get(c_comm, &ndims);
    OMPI_ARRAY_FINT_2_INT(coords, ndims);

    c_ierr = MPI_Cart_rank(c_comm, 
                           OMPI_ARRAY_NAME_CONVERT(coords),
                           OMPI_SINGLE_NAME_CONVERT(rank));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(rank);
    }
}
