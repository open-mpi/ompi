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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_KEYVAL_FREE = ompi_keyval_free_f
#pragma weak pmpi_keyval_free = ompi_keyval_free_f
#pragma weak pmpi_keyval_free_ = ompi_keyval_free_f
#pragma weak pmpi_keyval_free__ = ompi_keyval_free_f

#pragma weak PMPI_Keyval_free_f = ompi_keyval_free_f
#pragma weak PMPI_Keyval_free_f08 = ompi_keyval_free_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_KEYVAL_FREE,
                           pmpi_keyval_free,
                           pmpi_keyval_free_,
                           pmpi_keyval_free__,
                           pompi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_KEYVAL_FREE = ompi_keyval_free_f
#pragma weak mpi_keyval_free = ompi_keyval_free_f
#pragma weak mpi_keyval_free_ = ompi_keyval_free_f
#pragma weak mpi_keyval_free__ = ompi_keyval_free_f

#pragma weak MPI_Keyval_free_f = ompi_keyval_free_f
#pragma weak MPI_Keyval_free_f08 = ompi_keyval_free_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_KEYVAL_FREE,
                           mpi_keyval_free,
                           mpi_keyval_free_,
                           mpi_keyval_free__,
                           ompi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#else
#define ompi_keyval_free_f pompi_keyval_free_f
#endif
#endif


void ompi_keyval_free_f(MPI_Fint *keyval, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_SINGLE_NAME_DECL(keyval);

    OMPI_SINGLE_FINT_2_INT(keyval);

    c_ierr = PMPI_Keyval_free(OMPI_SINGLE_NAME_CONVERT(keyval));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(keyval);
    }
}
