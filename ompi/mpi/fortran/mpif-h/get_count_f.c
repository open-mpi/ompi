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
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET_COUNT = ompi_get_count_f
#pragma weak pmpi_get_count = ompi_get_count_f
#pragma weak pmpi_get_count_ = ompi_get_count_f
#pragma weak pmpi_get_count__ = ompi_get_count_f

#pragma weak PMPI_Get_count_f = ompi_get_count_f
#pragma weak PMPI_Get_count_f08 = ompi_get_count_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_COUNT,
                           pmpi_get_count,
                           pmpi_get_count_,
                           pmpi_get_count__,
                           pompi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_COUNT = ompi_get_count_f
#pragma weak mpi_get_count = ompi_get_count_f
#pragma weak mpi_get_count_ = ompi_get_count_f
#pragma weak mpi_get_count__ = ompi_get_count_f

#pragma weak MPI_Get_count_f = ompi_get_count_f
#pragma weak MPI_Get_count_f08 = ompi_get_count_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_COUNT,
                           mpi_get_count,
                           mpi_get_count_,
                           mpi_get_count__,
                           ompi_get_count_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_get_count_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Status   c_status;
    OMPI_SINGLE_NAME_DECL(count);

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *count = OMPI_INT_2_FINT(0);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = MPI_Status_f2c(status, &c_status);

        if (MPI_SUCCESS == c_ierr) {
            c_ierr = MPI_Get_count(&c_status, c_type, 
                                   OMPI_SINGLE_NAME_CONVERT(count));
            OMPI_SINGLE_INT_2_FINT(count);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
