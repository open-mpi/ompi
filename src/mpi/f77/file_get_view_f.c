/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_VIEW = mpi_file_get_view_f
#pragma weak pmpi_file_get_view = mpi_file_get_view_f
#pragma weak pmpi_file_get_view_ = mpi_file_get_view_f
#pragma weak pmpi_file_get_view__ = mpi_file_get_view_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_VIEW,
                           pmpi_file_get_view,
                           pmpi_file_get_view_,
                           pmpi_file_get_view__,
                           pmpi_file_get_view_f,
                           (MPI_Fint *fh, MPI_Fint *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *ierr),
                           (fh, disp, etype, filetype, datarep, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_VIEW = mpi_file_get_view_f
#pragma weak mpi_file_get_view = mpi_file_get_view_f
#pragma weak mpi_file_get_view_ = mpi_file_get_view_f
#pragma weak mpi_file_get_view__ = mpi_file_get_view_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_VIEW,
                           mpi_file_get_view,
                           mpi_file_get_view_,
                           mpi_file_get_view__,
                           mpi_file_get_view_f,
                           (MPI_Fint *fh, MPI_Fint *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *ierr),
                           (fh, disp, etype, filetype, datarep, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_get_view_f(MPI_Fint *fh, MPI_Fint *disp, 
			 MPI_Fint *etype, MPI_Fint *filetype, 
			 char *datarep, MPI_Fint *ierr)
{
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Datatype c_etype, c_filetype;
    MPI_Offset c_disp;

    *ierr = OMPI_INT_2_FINT(MPI_File_get_view(c_fh, &c_disp, &c_etype, 
					      &c_filetype, datarep));

    *disp = (MPI_Fint) c_disp;
    *etype = MPI_Type_c2f(c_etype);
    *filetype = MPI_Type_c2f(c_filetype);
}
