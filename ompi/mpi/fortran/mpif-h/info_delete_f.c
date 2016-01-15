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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mpi/fortran/base/strings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_INFO_DELETE = ompi_info_delete_f
#pragma weak pmpi_info_delete = ompi_info_delete_f
#pragma weak pmpi_info_delete_ = ompi_info_delete_f
#pragma weak pmpi_info_delete__ = ompi_info_delete_f

#pragma weak PMPI_Info_delete_f = ompi_info_delete_f
#pragma weak PMPI_Info_delete_f08 = ompi_info_delete_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_DELETE,
                            pmpi_info_delete,
                            pmpi_info_delete_,
                            pmpi_info_delete__,
                            pompi_info_delete_f,
                            (MPI_Fint *info, char *key, MPI_Fint *ierr, int key_len),
                            (info, key, ierr, key_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_DELETE = ompi_info_delete_f
#pragma weak mpi_info_delete = ompi_info_delete_f
#pragma weak mpi_info_delete_ = ompi_info_delete_f
#pragma weak mpi_info_delete__ = ompi_info_delete_f

#pragma weak MPI_Info_delete_f = ompi_info_delete_f
#pragma weak MPI_Info_delete_f08 = ompi_info_delete_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_DELETE,
                            mpi_info_delete,
                            mpi_info_delete_,
                            mpi_info_delete__,
                            ompi_info_delete_f,
                            (MPI_Fint *info, char *key, MPI_Fint *ierr, int key_len),
                            (info, key, ierr, key_len) )
#else
#define ompi_info_delete_f pompi_info_delete_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_INFO_DELETE";

/* Note that the key_len parameter is silently added by the Fortran
   compiler, and will be filled in with the actual length of the
   character array from the caller.  Hence, it's the max length of the
   string that we can use. */

void ompi_info_delete_f(MPI_Fint *info, char *key, MPI_Fint *ierr, int key_len)
{
    int c_ierr, ret;
    MPI_Info c_info;
    char *c_key;

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(key, key_len, &c_key))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Info_delete(c_info, c_key);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_key);
}
