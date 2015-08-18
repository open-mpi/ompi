/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "ompi/mpi/fortran/base/strings.h"
#include "ompi/file/file.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_OPEN = ompi_file_open_f
#pragma weak pmpi_file_open = ompi_file_open_f
#pragma weak pmpi_file_open_ = ompi_file_open_f
#pragma weak pmpi_file_open__ = ompi_file_open_f

#pragma weak PMPI_File_open_f = ompi_file_open_f
#pragma weak PMPI_File_open_f08 = ompi_file_open_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_OPEN,
                           pmpi_file_open,
                           pmpi_file_open_,
                           pmpi_file_open__,
                           pompi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr, int name_len),
                           (comm, filename, amode, info, fh, ierr, name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_OPEN = ompi_file_open_f
#pragma weak mpi_file_open = ompi_file_open_f
#pragma weak mpi_file_open_ = ompi_file_open_f
#pragma weak mpi_file_open__ = ompi_file_open_f

#pragma weak MPI_File_open_f = ompi_file_open_f
#pragma weak MPI_File_open_f08 = ompi_file_open_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_OPEN,
                           mpi_file_open,
                           mpi_file_open_,
                           mpi_file_open__,
                           ompi_file_open_f,
                           (MPI_Fint *comm, char *filename, MPI_Fint *amode, MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr, int name_len),
                           (comm, filename, amode, info, fh, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_file_open_f(MPI_Fint *comm, char *filename, MPI_Fint *amode,
		     MPI_Fint *info, MPI_Fint *fh, MPI_Fint *ierr, int name_len)
{
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Info c_info = MPI_Info_f2c(*info);
    MPI_File c_fh;
    char *c_filename;
    int c_ierr, ret;

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(filename, name_len,
                                                       &c_filename))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, ret, "MPI_FILE_OPEN");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = MPI_File_open(c_comm, c_filename,
                           OMPI_FINT_2_INT(*amode),
                           c_info, &c_fh);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *fh = MPI_File_c2f(c_fh);
    }

    free(c_filename);
}
