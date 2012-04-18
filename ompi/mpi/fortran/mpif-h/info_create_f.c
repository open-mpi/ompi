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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INFO_CREATE = ompi_info_create_f
#pragma weak pmpi_info_create = ompi_info_create_f
#pragma weak pmpi_info_create_ = ompi_info_create_f
#pragma weak pmpi_info_create__ = ompi_info_create_f

#pragma weak PMPI_Info_create_f = ompi_info_create_f
#pragma weak PMPI_Info_create_f08 = ompi_info_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_CREATE,
                           pmpi_info_create,
                           pmpi_info_create_,
                           pmpi_info_create__,
                           pompi_info_create_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_CREATE = ompi_info_create_f
#pragma weak mpi_info_create = ompi_info_create_f
#pragma weak mpi_info_create_ = ompi_info_create_f
#pragma weak mpi_info_create__ = ompi_info_create_f

#pragma weak MPI_Info_create_f = ompi_info_create_f
#pragma weak MPI_Info_create_f08 = ompi_info_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_CREATE,
                           mpi_info_create,
                           mpi_info_create_,
                           mpi_info_create__,
                           ompi_info_create_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_info_create_f(MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info;

    c_ierr = MPI_Info_create(&c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *info = MPI_Info_c2f(c_info);
    }
}
