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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GET_ELEMENTS = ompi_get_elements_f
#pragma weak pmpi_get_elements = ompi_get_elements_f
#pragma weak pmpi_get_elements_ = ompi_get_elements_f
#pragma weak pmpi_get_elements__ = ompi_get_elements_f

#pragma weak PMPI_Get_elements_f = ompi_get_elements_f
#pragma weak PMPI_Get_elements_f08 = ompi_get_elements_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_ELEMENTS,
                           pmpi_get_elements,
                           pmpi_get_elements_,
                           pmpi_get_elements__,
                           pompi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ELEMENTS = ompi_get_elements_f
#pragma weak mpi_get_elements = ompi_get_elements_f
#pragma weak mpi_get_elements_ = ompi_get_elements_f
#pragma weak mpi_get_elements__ = ompi_get_elements_f

#pragma weak MPI_Get_elements_f = ompi_get_elements_f
#pragma weak MPI_Get_elements_f08 = ompi_get_elements_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GET_ELEMENTS,
                           mpi_get_elements,
                           mpi_get_elements_,
                           mpi_get_elements__,
                           ompi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#else
#define ompi_get_elements_f pompi_get_elements_f
#endif
#endif


void ompi_get_elements_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*datatype);
    MPI_Status   c_status;
    OMPI_SINGLE_NAME_DECL(count);

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *count = OMPI_INT_2_FINT(0);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = PMPI_Status_f2c(status, &c_status);

        if (MPI_SUCCESS == c_ierr) {
            c_ierr = PMPI_Get_elements(&c_status, c_type,
                                      OMPI_SINGLE_NAME_CONVERT(count));
            OMPI_SINGLE_INT_2_FINT(count);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
