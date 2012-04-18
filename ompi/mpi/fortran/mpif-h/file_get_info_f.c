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
#pragma weak PMPI_FILE_GET_INFO = ompi_file_get_info_f
#pragma weak pmpi_file_get_info = ompi_file_get_info_f
#pragma weak pmpi_file_get_info_ = ompi_file_get_info_f
#pragma weak pmpi_file_get_info__ = ompi_file_get_info_f

#pragma weak PMPI_File_get_info_f = ompi_file_get_info_f
#pragma weak PMPI_File_get_info_f08 = ompi_file_get_info_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_INFO,
                           pmpi_file_get_info,
                           pmpi_file_get_info_,
                           pmpi_file_get_info__,
                           pompi_file_get_info_f,
                           (MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr),
                           (fh, info_used, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_INFO = ompi_file_get_info_f
#pragma weak mpi_file_get_info = ompi_file_get_info_f
#pragma weak mpi_file_get_info_ = ompi_file_get_info_f
#pragma weak mpi_file_get_info__ = ompi_file_get_info_f

#pragma weak MPI_File_get_info_f = ompi_file_get_info_f
#pragma weak MPI_File_get_info_f08 = ompi_file_get_info_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_INFO,
                           mpi_file_get_info,
                           mpi_file_get_info_,
                           mpi_file_get_info__,
                           ompi_file_get_info_f,
                           (MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr),
                           (fh, info_used, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_file_get_info_f(MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Info c_info;

    c_ierr = MPI_File_get_info(c_fh, &c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *info_used = MPI_Info_c2f(c_info);
    }
}
