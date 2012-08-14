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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INFO_GET = ompi_info_get_f
#pragma weak pmpi_info_get = ompi_info_get_f
#pragma weak pmpi_info_get_ = ompi_info_get_f
#pragma weak pmpi_info_get__ = ompi_info_get_f

#pragma weak PMPI_Info_get_f = ompi_info_get_f
#pragma weak PMPI_Info_get_f08 = ompi_info_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_GET,
                            pmpi_info_get,
                            pmpi_info_get_,
                            pmpi_info_get__,
                            pompi_info_get_f,
                            (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, valuelen, value, flag, ierr, key_len, value_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET = ompi_info_get_f
#pragma weak mpi_info_get = ompi_info_get_f
#pragma weak mpi_info_get_ = ompi_info_get_f
#pragma weak mpi_info_get__ = ompi_info_get_f

#pragma weak MPI_Info_get_f = ompi_info_get_f
#pragma weak MPI_Info_get_f08 = ompi_info_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_GET,
                            mpi_info_get,
                            mpi_info_get_,
                            mpi_info_get__,
                            ompi_info_get_f,
                            (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, valuelen, value, flag, ierr, key_len, value_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_INFO_GET";

/* Note that the key_len and value_len parameters are silently added
   by the Fortran compiler, and will be filled in with the actual
   length of the character array from the caller.  Hence, it's the max
   length of the string that we can use. */

void ompi_info_get_f(MPI_Fint *info, char *key, MPI_Fint *valuelen,
                    char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr,
                    int key_len, int value_len)
{
    int c_ierr, ret;
    MPI_Info c_info;
    char *c_key = NULL, c_value[MPI_MAX_INFO_VAL + 1];
    OMPI_LOGICAL_NAME_DECL(flag);

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(key, key_len, &c_key))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_info = MPI_Info_f2c(*info);

    c_ierr = MPI_Info_get(c_info, c_key,
                          OMPI_FINT_2_INT(*valuelen),
                          c_value,
                          OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);

        /* If we found the info key, copy the value back to the
           Fortran string (note: all Fortran compilers have FALSE ==
           0, so just check for any nonzero value, because not all
           Fortran compilers have TRUE == 1).  Note: use the full
           length of the Fortran string, not *valuelen.  See comment
           in ompi/mpi/fortran/base/strings.c. */
        if (*flag && OMPI_SUCCESS != 
            (ret = ompi_fortran_string_c2f(c_value, value, value_len))) {
            c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            free(c_key);
            return;
        }
    }

    free(c_key);
}
