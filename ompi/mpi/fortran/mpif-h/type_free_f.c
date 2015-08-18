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
#pragma weak PMPI_TYPE_FREE = ompi_type_free_f
#pragma weak pmpi_type_free = ompi_type_free_f
#pragma weak pmpi_type_free_ = ompi_type_free_f
#pragma weak pmpi_type_free__ = ompi_type_free_f

#pragma weak PMPI_Type_free_f = ompi_type_free_f
#pragma weak PMPI_Type_free_f08 = ompi_type_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_FREE,
                           pmpi_type_free,
                           pmpi_type_free_,
                           pmpi_type_free__,
                           pompi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_FREE = ompi_type_free_f
#pragma weak mpi_type_free = ompi_type_free_f
#pragma weak mpi_type_free_ = ompi_type_free_f
#pragma weak mpi_type_free__ = ompi_type_free_f

#pragma weak MPI_Type_free_f = ompi_type_free_f
#pragma weak MPI_Type_free_f08 = ompi_type_free_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_FREE,
                           mpi_type_free,
                           mpi_type_free_,
                           mpi_type_free__,
                           ompi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_free_f(MPI_Fint *type, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;

    c_type = MPI_Type_f2c(*type);

    c_ierr = MPI_Type_free(&c_type);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *type = MPI_Type_c2f(c_type);
    }
}
