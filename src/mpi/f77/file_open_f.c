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
#pragma weak PMPI_FILE_OPEN = mpi_file_open_f
#pragma weak pmpi_file_open = mpi_file_open_f
#pragma weak pmpi_file_open_ = mpi_file_open_f
#pragma weak pmpi_file_open__ = mpi_file_open_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_OPEN,
                           pmpi_file_open,
                           pmpi_file_open_,
                           pmpi_file_open__,
                           pmpi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr),
                           (comm, filename, amode, info, fh, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_OPEN = mpi_file_open_f
#pragma weak mpi_file_open = mpi_file_open_f
#pragma weak mpi_file_open_ = mpi_file_open_f
#pragma weak mpi_file_open__ = mpi_file_open_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_OPEN,
                           mpi_file_open,
                           mpi_file_open_,
                           mpi_file_open__,
                           mpi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr),
                           (comm, filename, amode, info, fh, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_file_open_f(MPI_Fint *comm, char *filename, MPI_Fint *amode,
		     MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr)
{
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Info c_info = MPI_Info_f2c(*info);
    MPI_File c_fh;

    *ierr = OMPI_INT_2_FINT(MPI_File_open(c_comm, filename, 
					  OMPI_FINT_2_INT(*amode),
					  c_info, &c_fh));
    
    *fh = MPI_File_c2f(c_fh);
}
