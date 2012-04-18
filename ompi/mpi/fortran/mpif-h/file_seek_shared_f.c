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
#pragma weak PMPI_FILE_SEEK_SHARED = ompi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared = ompi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared_ = ompi_file_seek_shared_f
#pragma weak pmpi_file_seek_shared__ = ompi_file_seek_shared_f

#pragma weak PMPI_File_seek_shared_f = ompi_file_seek_shared_f
#pragma weak PMPI_File_seek_shared_f08 = ompi_file_seek_shared_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_SEEK_SHARED,
                           pmpi_file_seek_shared,
                           pmpi_file_seek_shared_,
                           pmpi_file_seek_shared__,
                           pompi_file_seek_shared_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SEEK_SHARED = ompi_file_seek_shared_f
#pragma weak mpi_file_seek_shared = ompi_file_seek_shared_f
#pragma weak mpi_file_seek_shared_ = ompi_file_seek_shared_f
#pragma weak mpi_file_seek_shared__ = ompi_file_seek_shared_f

#pragma weak MPI_File_seek_shared_f = ompi_file_seek_shared_f
#pragma weak MPI_File_seek_shared_f08 = ompi_file_seek_shared_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_SEEK_SHARED,
                           mpi_file_seek_shared,
                           mpi_file_seek_shared_,
                           mpi_file_seek_shared__,
                           ompi_file_seek_shared_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *whence, MPI_Fint *ierr),
                           (fh, offset, whence, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_file_seek_shared_f(MPI_Fint *fh, MPI_Offset *offset, 
			    MPI_Fint *whence, MPI_Fint *ierr)
{    
    int c_ierr;
    MPI_File c_fh = MPI_File_f2c(*fh);

    c_ierr = MPI_File_seek_shared(c_fh, (MPI_Offset) *offset,
                                  OMPI_FINT_2_INT(*whence));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
