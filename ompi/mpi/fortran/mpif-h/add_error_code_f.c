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
#pragma weak PMPI_ADD_ERROR_CODE = ompi_add_error_code_f
#pragma weak pmpi_add_error_code = ompi_add_error_code_f
#pragma weak pmpi_add_error_code_ = ompi_add_error_code_f
#pragma weak pmpi_add_error_code__ = ompi_add_error_code_f

#pragma weak PMPI_Add_error_code_f = ompi_add_error_code_f
#pragma weak PMPI_Add_error_code_f08 = ompi_add_error_code_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_CODE,
                           pmpi_add_error_code,
                           pmpi_add_error_code_,
                           pmpi_add_error_code__,
                           pompi_add_error_code_f,
                           (MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (errorclass, errorcode, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_CODE = ompi_add_error_code_f
#pragma weak mpi_add_error_code = ompi_add_error_code_f
#pragma weak mpi_add_error_code_ = ompi_add_error_code_f
#pragma weak mpi_add_error_code__ = ompi_add_error_code_f

#pragma weak MPI_Add_error_code_f = ompi_add_error_code_f
#pragma weak MPI_Add_error_code_f08 = ompi_add_error_code_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_CODE,
                           mpi_add_error_code,
                           mpi_add_error_code_,
                           mpi_add_error_code__,
                           ompi_add_error_code_f,
                           (MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (errorclass, errorcode, ierr) )
#else
#define ompi_add_error_code_f pompi_add_error_code_f
#endif
#endif


void ompi_add_error_code_f(MPI_Fint *errorclass, MPI_Fint *errorcode, MPI_Fint *ierr)
{
    int ierr_c;
    OMPI_SINGLE_NAME_DECL(errorcode);

    ierr_c = PMPI_Add_error_code(OMPI_FINT_2_INT(*errorclass),
                                 OMPI_SINGLE_NAME_CONVERT(errorcode)
                                 );

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
    if (MPI_SUCCESS == ierr_c) {
        OMPI_SINGLE_INT_2_FINT(errorcode);
    }
}
