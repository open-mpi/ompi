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
#pragma weak PMPI_FILE_CALL_ERRHANDLER = ompi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler = ompi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler_ = ompi_file_call_errhandler_f
#pragma weak pmpi_file_call_errhandler__ = ompi_file_call_errhandler_f

#pragma weak PMPI_File_call_errhandler_f = ompi_file_call_errhandler_f
#pragma weak PMPI_File_call_errhandler_f08 = ompi_file_call_errhandler_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_CALL_ERRHANDLER,
                           pmpi_file_call_errhandler,
                           pmpi_file_call_errhandler_,
                           pmpi_file_call_errhandler__,
                           pompi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_CALL_ERRHANDLER = ompi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler = ompi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler_ = ompi_file_call_errhandler_f
#pragma weak mpi_file_call_errhandler__ = ompi_file_call_errhandler_f

#pragma weak MPI_File_call_errhandler_f = ompi_file_call_errhandler_f
#pragma weak MPI_File_call_errhandler_f08 = ompi_file_call_errhandler_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_CALL_ERRHANDLER,
                           mpi_file_call_errhandler,
                           mpi_file_call_errhandler_,
                           mpi_file_call_errhandler__,
                           ompi_file_call_errhandler_f,
                           (MPI_Fint *fh, MPI_Fint *errorcode, MPI_Fint *ierr),
                           (fh, errorcode, ierr) )
#else
#define ompi_file_call_errhandler_f pompi_file_call_errhandler_f
#endif
#endif


void ompi_file_call_errhandler_f(MPI_Fint *fh, MPI_Fint *errorcode,
				MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh;

    c_fh = PMPI_File_f2c(*fh);

    c_ierr = PMPI_File_call_errhandler(c_fh, OMPI_FINT_2_INT(*errorcode));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
