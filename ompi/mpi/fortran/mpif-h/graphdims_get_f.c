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
#pragma weak PMPI_GRAPHDIMS_GET = ompi_graphdims_get_f
#pragma weak pmpi_graphdims_get = ompi_graphdims_get_f
#pragma weak pmpi_graphdims_get_ = ompi_graphdims_get_f
#pragma weak pmpi_graphdims_get__ = ompi_graphdims_get_f

#pragma weak PMPI_Graphdims_get_f = ompi_graphdims_get_f
#pragma weak PMPI_Graphdims_get_f08 = ompi_graphdims_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPHDIMS_GET,
                           pmpi_graphdims_get,
                           pmpi_graphdims_get_,
                           pmpi_graphdims_get__,
                           pompi_graphdims_get_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *nedges, MPI_Fint *ierr),
                           (comm, nnodes, nedges, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPHDIMS_GET = ompi_graphdims_get_f
#pragma weak mpi_graphdims_get = ompi_graphdims_get_f
#pragma weak mpi_graphdims_get_ = ompi_graphdims_get_f
#pragma weak mpi_graphdims_get__ = ompi_graphdims_get_f

#pragma weak MPI_Graphdims_get_f = ompi_graphdims_get_f
#pragma weak MPI_Graphdims_get_f08 = ompi_graphdims_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPHDIMS_GET,
                           mpi_graphdims_get,
                           mpi_graphdims_get_,
                           mpi_graphdims_get__,
                           ompi_graphdims_get_f,
                           (MPI_Fint *comm, MPI_Fint *nnodes, MPI_Fint *nedges, MPI_Fint *ierr),
                           (comm, nnodes, nedges, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_graphdims_get_f(MPI_Fint *comm, MPI_Fint *nnodes,
			 MPI_Fint *nedges, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(nnodes);
    OMPI_SINGLE_NAME_DECL(nedges);

    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Graphdims_get(c_comm, 
                               OMPI_SINGLE_NAME_CONVERT(nnodes),
                               OMPI_SINGLE_NAME_CONVERT(nedges)
                               );
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(nnodes);
        OMPI_SINGLE_INT_2_FINT(nedges);
    }
}
