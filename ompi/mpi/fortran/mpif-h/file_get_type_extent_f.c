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
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
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
#pragma weak PMPI_FILE_GET_TYPE_EXTENT = ompi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent = ompi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent_ = ompi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent__ = ompi_file_get_type_extent_f

#pragma weak PMPI_File_get_type_extent_f = ompi_file_get_type_extent_f
#pragma weak PMPI_File_get_type_extent_f08 = ompi_file_get_type_extent_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_TYPE_EXTENT,
                           pmpi_file_get_type_extent,
                           pmpi_file_get_type_extent_,
                           pmpi_file_get_type_extent__,
                           pompi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Aint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_TYPE_EXTENT = ompi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent = ompi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent_ = ompi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent__ = ompi_file_get_type_extent_f

#pragma weak MPI_File_get_type_extent_f = ompi_file_get_type_extent_f
#pragma weak MPI_File_get_type_extent_f08 = ompi_file_get_type_extent_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_TYPE_EXTENT,
                           mpi_file_get_type_extent,
                           mpi_file_get_type_extent_,
                           mpi_file_get_type_extent__,
                           ompi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Aint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#else
#define ompi_file_get_type_extent_f pompi_file_get_type_extent_f
#endif
#endif


void ompi_file_get_type_extent_f(MPI_Fint *fh, MPI_Fint *datatype,
				MPI_Aint *extent, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh = PMPI_File_f2c(*fh);
    MPI_Datatype c_type;

    c_type = PMPI_Type_f2c(*datatype);

    c_ierr = PMPI_File_get_type_extent(c_fh, c_type, extent);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
