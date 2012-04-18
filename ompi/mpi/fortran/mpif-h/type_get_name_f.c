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
#include "ompi/mpi/fortran/base/strings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_NAME = ompi_type_get_name_f
#pragma weak pmpi_type_get_name = ompi_type_get_name_f
#pragma weak pmpi_type_get_name_ = ompi_type_get_name_f
#pragma weak pmpi_type_get_name__ = ompi_type_get_name_f

#pragma weak PMPI_Type_get_name_f = ompi_type_get_name_f
#pragma weak PMPI_Type_get_name_f08 = ompi_type_get_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_NAME,
                            pmpi_type_get_name,
                            pmpi_type_get_name_,
                            pmpi_type_get_name__,
                            pompi_type_get_name_f,
                            (MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (type, type_name, resultlen, ierr, name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_NAME = ompi_type_get_name_f
#pragma weak mpi_type_get_name = ompi_type_get_name_f
#pragma weak mpi_type_get_name_ = ompi_type_get_name_f
#pragma weak mpi_type_get_name__ = ompi_type_get_name_f

#pragma weak MPI_Type_get_name_f = ompi_type_get_name_f
#pragma weak MPI_Type_get_name_f08 = ompi_type_get_name_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_NAME,
                            mpi_type_get_name,
                            mpi_type_get_name_,
                            mpi_type_get_name__,
                            ompi_type_get_name_f,
                            (MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (type, type_name, resultlen, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_get_name_f(MPI_Fint *type, char *type_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len)
{
    int c_ierr, c_len;
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    char c_name[MPI_MAX_OBJECT_NAME];

    c_ierr = MPI_Type_get_name(c_type, c_name, &c_len);
    if (MPI_SUCCESS == c_ierr) {
        ompi_fortran_string_c2f(c_name, type_name, name_len);
        *resultlen = OMPI_INT_2_FINT(c_len);
        c_ierr = MPI_SUCCESS;
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
