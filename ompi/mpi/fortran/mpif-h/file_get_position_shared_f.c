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
#pragma weak PMPI_FILE_GET_POSITION_SHARED = ompi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared = ompi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared_ = ompi_file_get_position_shared_f
#pragma weak pmpi_file_get_position_shared__ = ompi_file_get_position_shared_f

#pragma weak PMPI_File_get_position_shared_f = ompi_file_get_position_shared_f
#pragma weak PMPI_File_get_position_shared_f08 = ompi_file_get_position_shared_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_POSITION_SHARED,
                           pmpi_file_get_position_shared,
                           pmpi_file_get_position_shared_,
                           pmpi_file_get_position_shared__,
                           pompi_file_get_position_shared_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_POSITION_SHARED = ompi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared = ompi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared_ = ompi_file_get_position_shared_f
#pragma weak mpi_file_get_position_shared__ = ompi_file_get_position_shared_f

#pragma weak MPI_File_get_position_shared_f = ompi_file_get_position_shared_f
#pragma weak MPI_File_get_position_shared_f08 = ompi_file_get_position_shared_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_POSITION_SHARED,
                           mpi_file_get_position_shared,
                           mpi_file_get_position_shared_,
                           mpi_file_get_position_shared__,
                           ompi_file_get_position_shared_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#else
#define ompi_file_get_position_shared_f pompi_file_get_position_shared_f
#endif
#endif


void ompi_file_get_position_shared_f(MPI_Fint *fh, MPI_Offset *offset,
				    MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh = PMPI_File_f2c(*fh);
    MPI_Offset c_offset;

    c_ierr = PMPI_File_get_position_shared(c_fh, &c_offset);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *offset = (MPI_Offset) c_offset;
    }
}
