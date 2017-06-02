/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mpi/fortran/base/fortran_base_strings.h"
#include "ompi/file/file.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_FILE_DELETE = ompi_file_delete_f
#pragma weak pmpi_file_delete = ompi_file_delete_f
#pragma weak pmpi_file_delete_ = ompi_file_delete_f
#pragma weak pmpi_file_delete__ = ompi_file_delete_f

#pragma weak PMPI_File_delete_f = ompi_file_delete_f
#pragma weak PMPI_File_delete_f08 = ompi_file_delete_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_DELETE,
                           pmpi_file_delete,
                           pmpi_file_delete_,
                           pmpi_file_delete__,
                           pompi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len),
                           (filename, info, ierr, filename_len)  )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_DELETE = ompi_file_delete_f
#pragma weak mpi_file_delete = ompi_file_delete_f
#pragma weak mpi_file_delete_ = ompi_file_delete_f
#pragma weak mpi_file_delete__ = ompi_file_delete_f

#pragma weak MPI_File_delete_f = ompi_file_delete_f
#pragma weak MPI_File_delete_f08 = ompi_file_delete_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_DELETE,
                           mpi_file_delete,
                           mpi_file_delete_,
                           mpi_file_delete__,
                           ompi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len),
                           (filename, info, ierr, filename_len) )
#else
#define ompi_file_delete_f pompi_file_delete_f
#endif
#endif


void ompi_file_delete_f(char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len)
{
    MPI_Info c_info;
    char *c_filename;
    int c_ierr, ret;

    c_info = PMPI_Info_f2c(*info);

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(filename, filename_len,
                                                       &c_filename))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, ret, "MPI_FILE_DELETE");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_File_delete(c_filename, c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_filename);
}
