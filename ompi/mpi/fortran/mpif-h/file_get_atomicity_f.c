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
#pragma weak PMPI_FILE_GET_ATOMICITY = ompi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity = ompi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity_ = ompi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity__ = ompi_file_get_atomicity_f

#pragma weak PMPI_File_get_atomicity_f = ompi_file_get_atomicity_f
#pragma weak PMPI_File_get_atomicity_f08 = ompi_file_get_atomicity_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_ATOMICITY,
                           pmpi_file_get_atomicity,
                           pmpi_file_get_atomicity_,
                           pmpi_file_get_atomicity__,
                           pompi_file_get_atomicity_f,
                           (MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_ATOMICITY = ompi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity = ompi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity_ = ompi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity__ = ompi_file_get_atomicity_f

#pragma weak MPI_File_get_atomicity_f = ompi_file_get_atomicity_f
#pragma weak MPI_File_get_atomicity_f08 = ompi_file_get_atomicity_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_ATOMICITY,
                           mpi_file_get_atomicity,
                           mpi_file_get_atomicity_,
                           mpi_file_get_atomicity__,
                           ompi_file_get_atomicity_f,
                           (MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_file_get_atomicity_f(MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_File c_fh;
    OMPI_LOGICAL_NAME_DECL(flag);

    c_fh = MPI_File_f2c(*fh);
    c_ierr = MPI_File_get_atomicity(c_fh,
                                    OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
