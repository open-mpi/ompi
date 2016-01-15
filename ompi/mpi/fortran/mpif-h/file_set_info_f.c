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
#pragma weak PMPI_FILE_SET_INFO = ompi_file_set_info_f
#pragma weak pmpi_file_set_info = ompi_file_set_info_f
#pragma weak pmpi_file_set_info_ = ompi_file_set_info_f
#pragma weak pmpi_file_set_info__ = ompi_file_set_info_f

#pragma weak PMPI_File_set_info_f = ompi_file_set_info_f
#pragma weak PMPI_File_set_info_f08 = ompi_file_set_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_SET_INFO,
                           pmpi_file_set_info,
                           pmpi_file_set_info_,
                           pmpi_file_set_info__,
                           pompi_file_set_info_f,
                           (MPI_Fint *fh, MPI_Fint *info, MPI_Fint *ierr),
                           (fh, info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_INFO = ompi_file_set_info_f
#pragma weak mpi_file_set_info = ompi_file_set_info_f
#pragma weak mpi_file_set_info_ = ompi_file_set_info_f
#pragma weak mpi_file_set_info__ = ompi_file_set_info_f

#pragma weak MPI_File_set_info_f = ompi_file_set_info_f
#pragma weak MPI_File_set_info_f08 = ompi_file_set_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_SET_INFO,
                           mpi_file_set_info,
                           mpi_file_set_info_,
                           mpi_file_set_info__,
                           ompi_file_set_info_f,
                           (MPI_Fint *fh, MPI_Fint *info, MPI_Fint *ierr),
                           (fh, info, ierr) )
#else
#define ompi_file_set_info_f pompi_file_set_info_f
#endif
#endif


void ompi_file_set_info_f(MPI_Fint *fh, MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh = PMPI_File_f2c(*fh);
    MPI_Info c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_File_set_info(c_fh, c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
