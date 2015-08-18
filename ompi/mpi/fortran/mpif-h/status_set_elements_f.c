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
#pragma weak PMPI_STATUS_SET_ELEMENTS = ompi_status_set_elements_f
#pragma weak pmpi_status_set_elements = ompi_status_set_elements_f
#pragma weak pmpi_status_set_elements_ = ompi_status_set_elements_f
#pragma weak pmpi_status_set_elements__ = ompi_status_set_elements_f

#pragma weak PMPI_Status_set_elements_f = ompi_status_set_elements_f
#pragma weak PMPI_Status_set_elements_f08 = ompi_status_set_elements_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_STATUS_SET_ELEMENTS,
                           pmpi_status_set_elements,
                           pmpi_status_set_elements_,
                           pmpi_status_set_elements__,
                           pompi_status_set_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_SET_ELEMENTS = ompi_status_set_elements_f
#pragma weak mpi_status_set_elements = ompi_status_set_elements_f
#pragma weak mpi_status_set_elements_ = ompi_status_set_elements_f
#pragma weak mpi_status_set_elements__ = ompi_status_set_elements_f

#pragma weak MPI_Status_set_elements_f = ompi_status_set_elements_f
#pragma weak MPI_Status_set_elements_f08 = ompi_status_set_elements_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_STATUS_SET_ELEMENTS,
                           mpi_status_set_elements,
                           mpi_status_set_elements_,
                           mpi_status_set_elements__,
                           ompi_status_set_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_status_set_elements_f(MPI_Fint *status, MPI_Fint *datatype,
			       MPI_Fint *count, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Status c_status;

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        c_ierr = MPI_SUCCESS;
    } else {
        MPI_Status_f2c( status, &c_status );

        c_ierr = MPI_Status_set_elements(&c_status, c_type,
                                         OMPI_FINT_2_INT(*count));

        /* If datatype is really being set, then that needs to be
           converted.... */
        if (MPI_SUCCESS == c_ierr) {
            MPI_Status_c2f(&c_status, status);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
