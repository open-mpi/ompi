/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_TRUE_EXTENT = mpi_type_get_true_extent_f
#pragma weak pmpi_type_get_true_extent = mpi_type_get_true_extent_f
#pragma weak pmpi_type_get_true_extent_ = mpi_type_get_true_extent_f
#pragma weak pmpi_type_get_true_extent__ = mpi_type_get_true_extent_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_TRUE_EXTENT,
                           pmpi_type_get_true_extent,
                           pmpi_type_get_true_extent_,
                           pmpi_type_get_true_extent__,
                           pmpi_type_get_true_extent_f,
                           (MPI_Fint *datatype, MPI_Fint *true_lb, MPI_Fint *true_extent, MPI_Fint *ierr),
                           (datatype, true_lb, true_extent, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_TRUE_EXTENT = mpi_type_get_true_extent_f
#pragma weak mpi_type_get_true_extent = mpi_type_get_true_extent_f
#pragma weak mpi_type_get_true_extent_ = mpi_type_get_true_extent_f
#pragma weak mpi_type_get_true_extent__ = mpi_type_get_true_extent_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_TRUE_EXTENT,
                           mpi_type_get_true_extent,
                           mpi_type_get_true_extent_,
                           mpi_type_get_true_extent__,
                           mpi_type_get_true_extent_f,
                           (MPI_Fint *datatype, MPI_Fint *true_lb, MPI_Fint *true_extent, MPI_Fint *ierr),
                           (datatype, true_lb, true_extent, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_get_true_extent_f(MPI_Fint *datatype, MPI_Fint *true_lb, MPI_Fint *true_extent, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Aint c_true_lb;
    MPI_Aint c_true_extent;

    *ierr = OMPI_INT_2_FINT(MPI_Type_get_true_extent(c_type,
						     &c_true_lb,
						     &c_true_extent));

    if (MPI_SUCCESS == *ierr) {
      *true_lb = (MPI_Fint) c_true_lb;
      *true_extent = (MPI_Fint) c_true_extent;
    }
}
