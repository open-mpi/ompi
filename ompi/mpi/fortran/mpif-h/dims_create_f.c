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
#pragma weak PMPI_DIMS_CREATE = ompi_dims_create_f
#pragma weak pmpi_dims_create = ompi_dims_create_f
#pragma weak pmpi_dims_create_ = ompi_dims_create_f
#pragma weak pmpi_dims_create__ = ompi_dims_create_f

#pragma weak PMPI_Dims_create_f = ompi_dims_create_f
#pragma weak PMPI_Dims_create_f08 = ompi_dims_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_DIMS_CREATE,
                           pmpi_dims_create,
                           pmpi_dims_create_,
                           pmpi_dims_create__,
                           pompi_dims_create_f,
                           (MPI_Fint *nnodes, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *ierr),
                           (nnodes, ndims, dims, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIMS_CREATE = ompi_dims_create_f
#pragma weak mpi_dims_create = ompi_dims_create_f
#pragma weak mpi_dims_create_ = ompi_dims_create_f
#pragma weak mpi_dims_create__ = ompi_dims_create_f

#pragma weak MPI_Dims_create_f = ompi_dims_create_f
#pragma weak MPI_Dims_create_f08 = ompi_dims_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_DIMS_CREATE,
                           mpi_dims_create,
                           mpi_dims_create_,
                           mpi_dims_create__,
                           ompi_dims_create_f,
                           (MPI_Fint *nnodes, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *ierr),
                           (nnodes, ndims, dims, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_dims_create_f(MPI_Fint *nnodes, MPI_Fint *ndims,
		       MPI_Fint *dims, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_ARRAY_NAME_DECL(dims);

    OMPI_ARRAY_FINT_2_INT(dims, *ndims);

    c_ierr = MPI_Dims_create(OMPI_FINT_2_INT(*nnodes),
                             OMPI_FINT_2_INT(*ndims),
                             OMPI_ARRAY_NAME_CONVERT(dims));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_ARRAY_INT_2_FINT(dims, *ndims);
    } else {
        OMPI_ARRAY_FINT_2_INT_CLEANUP(dims);
    }
}
