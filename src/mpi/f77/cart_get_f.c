/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CART_GET = mpi_cart_get_f
#pragma weak pmpi_cart_get = mpi_cart_get_f
#pragma weak pmpi_cart_get_ = mpi_cart_get_f
#pragma weak pmpi_cart_get__ = mpi_cart_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_GET,
                           pmpi_cart_get,
                           pmpi_cart_get_,
                           pmpi_cart_get__,
                           pmpi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_GET = mpi_cart_get_f
#pragma weak mpi_cart_get = mpi_cart_get_f
#pragma weak mpi_cart_get_ = mpi_cart_get_f
#pragma weak mpi_cart_get__ = mpi_cart_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_GET,
                           mpi_cart_get,
                           mpi_cart_get_,
                           mpi_cart_get__,
                           mpi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_cart_get_f(MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims,
		    MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    int size;
    OMPI_ARRAY_NAME_DECL(dims);
    OMPI_ARRAY_NAME_DECL(periods);
    OMPI_ARRAY_NAME_DECL(coords);
    
    c_comm = MPI_Comm_f2c(*comm);

    size = OMPI_FINT_2_INT(*maxdims);
    OMPI_ARRAY_FINT_2_INT_ALLOC(dims, size);
    OMPI_ARRAY_FINT_2_INT_ALLOC(periods, size);
    OMPI_ARRAY_FINT_2_INT_ALLOC(coords, size);
    
    *ierr = OMPI_INT_2_FINT(MPI_Cart_get(c_comm,
					 OMPI_FINT_2_INT(*maxdims), 
					 OMPI_ARRAY_NAME_CONVERT(dims),
					 OMPI_ARRAY_NAME_CONVERT(periods),
					 OMPI_ARRAY_NAME_CONVERT(coords)));

    OMPI_ARRAY_INT_2_FINT(dims, size);
    OMPI_ARRAY_INT_2_FINT(periods, size);
    OMPI_ARRAY_INT_2_FINT(coords, size);
}
