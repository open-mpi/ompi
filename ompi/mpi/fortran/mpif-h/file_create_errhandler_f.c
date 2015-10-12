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
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/errhandler/errhandler.h"
#include "ompi/file/file.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_FILE_CREATE_ERRHANDLER = ompi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler = ompi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler_ = ompi_file_create_errhandler_f
#pragma weak pmpi_file_create_errhandler__ = ompi_file_create_errhandler_f

#pragma weak PMPI_File_create_errhandler_f = ompi_file_create_errhandler_f
#pragma weak PMPI_File_create_errhandler_f08 = ompi_file_create_errhandler_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_CREATE_ERRHANDLER,
                           pmpi_file_create_errhandler,
                           pmpi_file_create_errhandler_,
                           pmpi_file_create_errhandler__,
                           pompi_file_create_errhandler_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CREATE_ERRHANDLER = ompi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler = ompi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler_ = ompi_file_create_errhandler_f
#pragma weak mpi_file_create_errhandler__ = ompi_file_create_errhandler_f

#pragma weak MPI_File_create_errhandler_f = ompi_file_create_errhandler_f
#pragma weak MPI_File_create_errhandler_f08 = ompi_file_create_errhandler_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_CREATE_ERRHANDLER,
                           mpi_file_create_errhandler,
                           mpi_file_create_errhandler_,
                           mpi_file_create_errhandler__,
                           ompi_file_create_errhandler_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#else
#define ompi_file_create_errhandler_f pompi_file_create_errhandler_f
#endif
#endif

static const char FUNC_NAME[] = "MPI_FILE_CREATE_ERRHANDLER";


void ompi_file_create_errhandler_f(ompi_errhandler_fortran_handler_fn_t* function,
				  MPI_Fint *errhandler, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Errhandler c_errhandler =
        ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_FILE,
                               (ompi_errhandler_generic_handler_fn_t*) function,
                               OMPI_ERRHANDLER_LANG_FORTRAN);
    if (MPI_ERRHANDLER_NULL != c_errhandler) {
        *errhandler = PMPI_Errhandler_c2f(c_errhandler);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = MPI_ERR_INTERN;
        OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, MPI_ERR_INTERN, FUNC_NAME);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
