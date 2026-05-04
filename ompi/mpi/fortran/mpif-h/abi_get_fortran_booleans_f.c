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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ABI_GET_FOTRAN_BOOLEANS = ompi_abi_get_fortran_booleans_f
#pragma weak pmpi_abi_get_fortran_booleans = ompi_abi_get_fortran_booleans_f
#pragma weak pmpi_abi_get_fortran_booleans_ = ompi_abi_get_fortran_booleans_f
#pragma weak pmpi_abi_get_fortran_booleans__ = ompi_abi_get_fortran_booleans_f

#pragma weak PMPI_Abi_get_fortran_booleans_f = ompi_abi_get_fortran_booleans_f
#pragma weak PMPI_Abi_get_fortran_booleans_f08 = ompi_abi_get_fortran_booleans_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ABI_GET_FOTRAN_BOOLEANS,
                           pmpi_abi_get_fortran_booleans,
                           pmpi_abi_get_fortran_booleans_,
                           pmpi_abi_get_fortran_booleans__,
                           pompi_abi_get_fortran_booleans_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ABI_GET_FOTRAN_BOOLEANS = ompi_abi_get_fortran_booleans_f
#pragma weak mpi_abi_get_fortran_booleans = ompi_abi_get_fortran_booleans_f
#pragma weak mpi_abi_get_fortran_booleans_ = ompi_abi_get_fortran_booleans_f
#pragma weak mpi_abi_get_fortran_booleans__ = ompi_abi_get_fortran_booleans_f

#pragma weak MPI_Abi_get_fortran_booleans_f = ompi_abi_get_fortran_booleans_f
#pragma weak MPI_Abi_get_fortran_booleans_f08 = ompi_abi_get_fortran_booleans_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ABI_GET_FOTRAN_BOOLEANS,
                           mpi_abi_get_fortran_booleans,
                           mpi_abi_get_fortran_booleans_,
                           mpi_abi_get_fortran_booleans__,
                           ompi_abi_get_fortran_booleans_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#else
#define ompi_abi_get_fortran_booleans_f pompi_abi_get_fortran_booleans_f
#endif
#endif

void ompi_abi_get_fortran_booleans_f(MPI_Fint *logical_size, 
                                     ompi_fortran_logical_t *logical_true, 
                                     ompi_fortran_logical_t *logical_false, 
                                     ompi_fortran_logical_t *is_set,
                                     MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_LOGICAL_NAME_DECL(logical_true);
    OMPI_LOGICAL_NAME_DECL(logical_false);
    OMPI_LOGICAL_NAME_DECL(is_set);

    c_ierr = PMPI_Abi_get_fortran_booleans(OMPI_FINT_2_INT(*logical_size), 
                                           OMPI_LOGICAL_SINGLE_NAME_CONVERT(logical_true),
                                           OMPI_LOGICAL_SINGLE_NAME_CONVERT(logical_false),
                                           OMPI_LOGICAL_SINGLE_NAME_CONVERT(is_set));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_LOGICAL(logical_true);
    OMPI_SINGLE_INT_2_LOGICAL(logical_false);
    OMPI_SINGLE_INT_2_LOGICAL(is_set);
}
