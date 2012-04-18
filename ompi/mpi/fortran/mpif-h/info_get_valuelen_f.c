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
#pragma weak PMPI_INFO_GET_VALUELEN = ompi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen = ompi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen_ = ompi_info_get_valuelen_f
#pragma weak pmpi_info_get_valuelen__ = ompi_info_get_valuelen_f

#pragma weak PMPI_Info_get_valuelen_f = ompi_info_get_valuelen_f
#pragma weak PMPI_Info_get_valuelen_f08 = ompi_info_get_valuelen_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_GET_VALUELEN,
                            pmpi_info_get_valuelen,
                            pmpi_info_get_valuelen_,
                            pmpi_info_get_valuelen__,
                            pompi_info_get_valuelen_f,
                            (MPI_Fint *info, char *key, MPI_Fint *valuelen, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len),
                            (info, key, valuelen, flag, ierr, key_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET_VALUELEN = ompi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen = ompi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen_ = ompi_info_get_valuelen_f
#pragma weak mpi_info_get_valuelen__ = ompi_info_get_valuelen_f

#pragma weak MPI_Info_get_valuelen_f = ompi_info_get_valuelen_f
#pragma weak MPI_Info_get_valuelen_f08 = ompi_info_get_valuelen_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_GET_VALUELEN,
                            mpi_info_get_valuelen,
                            mpi_info_get_valuelen_,
                            mpi_info_get_valuelen__,
                            ompi_info_get_valuelen_f,
                            (MPI_Fint *info, char *key, MPI_Fint *valuelen, ompi_fortran_logical_t *flag, MPI_Fint *ierr, int key_len),
                            (info, key, valuelen, flag, ierr, key_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_INFO_GET_VALUELEN";

/* Note that the key_len parameter is silently added by the Fortran
   compiler, and will be filled in with the actual length of the
   character array from the caller.  Hence, it's the max length of the
   string that we can use. */

void ompi_info_get_valuelen_f(MPI_Fint *info, char *key,
                             MPI_Fint *valuelen, ompi_fortran_logical_t *flag,
                             MPI_Fint *ierr, int key_len)
{
    int c_ierr, ret;
    MPI_Info c_info;
    char *c_key;
    OMPI_SINGLE_NAME_DECL(valuelen);
    OMPI_LOGICAL_NAME_DECL(flag);

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(key, key_len, &c_key))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_info = MPI_Info_f2c(*info);
    c_ierr = MPI_Info_get_valuelen(c_info, c_key,
                                   OMPI_SINGLE_NAME_CONVERT(valuelen),
                                   OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(valuelen);
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }

    free(c_key);
}
