/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
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
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_INFO_GET_STRING = ompi_info_get_string_f
#pragma weak pmpi_info_get_string = ompi_info_get_string_f
#pragma weak pmpi_info_get_string_ = ompi_info_get_string_f
#pragma weak pmpi_info_get_string__ = ompi_info_get_string_f

#pragma weak PMPI_Info_get_string_f = ompi_info_get_string_f
#pragma weak PMPI_Info_get_string_f08 = ompi_info_get_string_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_GET_STRING,
                            pmpi_info_get_string,
                            pmpi_info_get_string_,
                            pmpi_info_get_string__,
                            pompi_info_get_string_f,
                            (MPI_Fint *info, char *key, MPI_Fint *buflen, char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, buflen, value, flag, ierr, key_len, value_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_STRING = ompi_info_get_string_f
#pragma weak mpi_info_get_string = ompi_info_get_string_f
#pragma weak mpi_info_get_string_ = ompi_info_get_string_f
#pragma weak mpi_info_get_string__ = ompi_info_get_string_f

#pragma weak MPI_Info_get_string_f = ompi_info_get_string_f
#pragma weak MPI_Info_get_string_f08 = ompi_info_get_string_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_GET_STRING,
                            mpi_info_get_string,
                            mpi_info_get_string_,
                            mpi_info_get_string__,
                            ompi_info_get_string_f,
                            (MPI_Fint *info, char *key, MPI_Fint *buflen, char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len, int value_len),
                            (info, key, buflen, value, flag, ierr, key_len, value_len) )
#else
#define ompi_info_get_string_f pompi_info_get_string_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_INFO_GET_STRING";

/* Note that the key_len and value_len parameters are silently added
   by the Fortran compiler, and will be filled in with the actual
   length of the character array from the caller.  Hence, it's the max
   length of the string that we can use. */

void ompi_info_get_string_f(MPI_Fint *info, char *key, MPI_Fint *buflen,
                            char *value, ompi_fortran_logical_t *flag, MPI_Fint *ierr,
                            int key_len, int value_len)
{
    int c_ierr, ret;
    MPI_Info c_info;
    char *c_key = NULL;
    OMPI_SINGLE_NAME_DECL(buflen);
    OMPI_LOGICAL_NAME_DECL(flag);
    opal_cstring_t *info_str;

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(key, key_len, &c_key))) {
        c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_info = PMPI_Info_f2c(*info);

    if (0 == *buflen) {
        c_ierr = ompi_info_get_valuelen(c_info, c_key,
                                       OMPI_SINGLE_NAME_CONVERT(buflen),
                                       OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

        if (MPI_SUCCESS == c_ierr) {
            OMPI_SINGLE_INT_2_FINT(buflen);
            OMPI_SINGLE_INT_2_LOGICAL(flag);
        }
    } else { 
        c_ierr = ompi_info_get(c_info, c_key, &info_str,
                               OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
        if (NULL != ierr) {
            *ierr = OMPI_INT_2_FINT(c_ierr);
        }
        

        if (MPI_SUCCESS == c_ierr) {
            OMPI_SINGLE_INT_2_LOGICAL(flag);

            /* If we found the info key, copy the value back to the
               Fortran string (note: all Fortran compilers have FALSE ==
               0, so just check for any nonzero value, because not all
               Fortran compilers have TRUE == 1).  Note: use the full
               length of the Fortran string, which means adding one to the 3rd arg 
               to ompi_fortran_string_c2f */
            if (*flag) {
                if (OMPI_SUCCESS !=
                  (ret = ompi_fortran_string_c2f(info_str->string, value, value_len + 1))) {
                    c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(ret, FUNC_NAME);
                    if (NULL != ierr) {
                        *ierr = OMPI_INT_2_FINT(c_ierr);
                    }
                }
                *buflen = info_str->length;
                OBJ_RELEASE(info_str);
            }
        }
    }

    if (NULL != c_key) {
        free(c_key);
    }
}
