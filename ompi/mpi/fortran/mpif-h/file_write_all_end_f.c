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
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/mpif-h/status-conversion.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_WRITE_ALL_END = ompi_file_write_all_end_f
#pragma weak pmpi_file_write_all_end = ompi_file_write_all_end_f
#pragma weak pmpi_file_write_all_end_ = ompi_file_write_all_end_f
#pragma weak pmpi_file_write_all_end__ = ompi_file_write_all_end_f

#pragma weak PMPI_File_write_all_end_f = ompi_file_write_all_end_f
#pragma weak PMPI_File_write_all_end_f08 = ompi_file_write_all_end_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_WRITE_ALL_END,
                           pmpi_file_write_all_end,
                           pmpi_file_write_all_end_,
                           pmpi_file_write_all_end__,
                           pompi_file_write_all_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_WRITE_ALL_END = ompi_file_write_all_end_f
#pragma weak mpi_file_write_all_end = ompi_file_write_all_end_f
#pragma weak mpi_file_write_all_end_ = ompi_file_write_all_end_f
#pragma weak mpi_file_write_all_end__ = ompi_file_write_all_end_f

#pragma weak MPI_File_write_all_end_f = ompi_file_write_all_end_f
#pragma weak MPI_File_write_all_end_f08 = ompi_file_write_all_end_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_WRITE_ALL_END,
                           mpi_file_write_all_end,
                           mpi_file_write_all_end_,
                           mpi_file_write_all_end__,
                           ompi_file_write_all_end_f,
                           (MPI_Fint *fh, char *buf, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, buf, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_file_write_all_end_f(MPI_Fint *fh, char *buf, MPI_Fint *status,
			      MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)
    MPI_File c_fh = MPI_File_f2c(*fh);

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)
    
    c_ierr = MPI_File_write_all_end(c_fh, buf, c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
}
