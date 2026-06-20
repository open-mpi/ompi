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
 * Copyright (c) 2026      Triad National Security, LLC.  All rights reserved.
 *
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
#pragma weak PMPI_STATUS_GET_TAG = ompi_status_get_tag_f
#pragma weak pmpi_status_get_tag = ompi_status_get_tag_f
#pragma weak pmpi_status_get_tag_ = ompi_status_get_tag_f
#pragma weak pmpi_status_get_tag__ = ompi_status_get_tag_f

#pragma weak PMPI_Status_get_tag_f = ompi_status_get_tag_f
#pragma weak PMPI_Status_get_tag_f08 = ompi_status_get_tag_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_STATUS_GET_TAG,
                           pmpi_status_get_tag,
                           pmpi_status_get_tag_,
                           pmpi_status_get_tag__,
                           pompi_status_get_tag_f,
                           (MPI_Fint *status, MPI_Fint *tag, MPI_Fint *ierr),
                           (status, tag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_GET_TAG = ompi_status_get_tag_f
#pragma weak mpi_status_get_tag = ompi_status_get_tag_f
#pragma weak mpi_status_get_tag_ = ompi_status_get_tag_f
#pragma weak mpi_status_get_tag__ = ompi_status_get_tag_f

#pragma weak MPI_Status_get_tag_f = ompi_status_get_tag_f
#pragma weak MPI_Status_get_tag_f08 = ompi_status_get_tag_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_STATUS_GET_TAG,
                           mpi_status_get_tag,
                           mpi_status_get_tag_,
                           mpi_status_get_tag__,
                           ompi_status_get_tag_f,
                           (MPI_Fint *status, MPI_Fint *tag, MPI_Fint *ierr),
                           (status, tag, ierr) )
#else
#define ompi_status_get_tag_f pompi_status_get_tag_f
#endif
#endif


void ompi_status_get_tag_f(MPI_Fint *status, MPI_Fint *tag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;
    OMPI_SINGLE_NAME_DECL(tag);

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *tag = OMPI_INT_2_FINT(0);
        c_ierr = MPI_SUCCESS;
    } else {
        PMPI_Status_f2c( status, &c_status );

        c_ierr = PMPI_Status_get_tag(&c_status, OMPI_SINGLE_NAME_CONVERT(tag));

        if (MPI_SUCCESS == c_ierr) {
            OMPI_SINGLE_INT_2_FINT(tag);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
