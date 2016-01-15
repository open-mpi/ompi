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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
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
#include "ompi/mpi/fortran/mpif-h/status-conversion.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_FILE_READ_AT = ompi_file_read_at_f
#pragma weak pmpi_file_read_at = ompi_file_read_at_f
#pragma weak pmpi_file_read_at_ = ompi_file_read_at_f
#pragma weak pmpi_file_read_at__ = ompi_file_read_at_f

#pragma weak PMPI_File_read_at_f = ompi_file_read_at_f
#pragma weak PMPI_File_read_at_f08 = ompi_file_read_at_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_READ_AT,
                           pmpi_file_read_at,
                           pmpi_file_read_at_,
                           pmpi_file_read_at__,
                           pompi_file_read_at_f,
                           (MPI_Fint *fh, MPI_Offset *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_READ_AT = ompi_file_read_at_f
#pragma weak mpi_file_read_at = ompi_file_read_at_f
#pragma weak mpi_file_read_at_ = ompi_file_read_at_f
#pragma weak mpi_file_read_at__ = ompi_file_read_at_f

#pragma weak MPI_File_read_at_f = ompi_file_read_at_f
#pragma weak MPI_File_read_at_f08 = ompi_file_read_at_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_READ_AT,
                           mpi_file_read_at,
                           mpi_file_read_at_,
                           mpi_file_read_at__,
                           ompi_file_read_at_f,
                           (MPI_Fint *fh, MPI_Offset *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *status, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, status, ierr) )
#else
#define ompi_file_read_at_f pompi_file_read_at_f
#endif
#endif


void ompi_file_read_at_f(MPI_Fint *fh, MPI_Offset *offset, char *buf,
			MPI_Fint *count, MPI_Fint *datatype,
			MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh = PMPI_File_f2c(*fh);
    MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)

    c_ierr = PMPI_File_read_at(c_fh,
                              (MPI_Offset) *offset,
                              buf,
                              OMPI_FINT_2_INT(*count),
                              c_type, c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
}
