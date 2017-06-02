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
 * Copyright (c) 2015-2017 Research Organization for Information Science
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
#include "ompi/mpi/fortran/base/fortran_base_strings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_INFO_SET = ompi_info_set_f
#pragma weak pmpi_info_set = ompi_info_set_f
#pragma weak pmpi_info_set_ = ompi_info_set_f
#pragma weak pmpi_info_set__ = ompi_info_set_f

#pragma weak PMPI_Info_set_f = ompi_info_set_f
#pragma weak PMPI_Info_set_f08 = ompi_info_set_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_SET,
                            pmpi_info_set,
                            pmpi_info_set_,
                            pmpi_info_set__,
                            pompi_info_set_f,
                            (MPI_Fint *info, char *key, char *value, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, value, ierr, key_len, value_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_SET = ompi_info_set_f
#pragma weak mpi_info_set = ompi_info_set_f
#pragma weak mpi_info_set_ = ompi_info_set_f
#pragma weak mpi_info_set__ = ompi_info_set_f

#pragma weak MPI_Info_set_f = ompi_info_set_f
#pragma weak MPI_Info_set_f08 = ompi_info_set_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_SET,
                            mpi_info_set,
                            mpi_info_set_,
                            mpi_info_set__,
                            ompi_info_set_f,
                            (MPI_Fint *info, char *key, char *value, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, value, ierr, key_len, value_len) )
#else
#define ompi_info_set_f pompi_info_set_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_INFO_SET";

/* Note that the key_len and value_len parameters are silently added
   by the Fortran compiler, and will be filled in with the actual
   length of the character array from the caller.  Hence, it's the max
   length of the string that we can use. */

void ompi_info_set_f(MPI_Fint *info, char *key, char *value, MPI_Fint *ierr,
                    int key_len, int value_len)
{
    int ret, c_ierr;
    MPI_Info c_info;
    char *c_key = NULL, *c_value = NULL;

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(key, key_len, &c_key)) ||
        OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(value, value_len,
                                                       &c_value))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        if (NULL != c_key) {
            free(c_key);
        }
        return;
    }
    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Info_set(c_info, c_key, c_value);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_key);
    free(c_value);
}
