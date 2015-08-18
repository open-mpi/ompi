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
#pragma weak PMPI_TYPE_UB = ompi_type_ub_f
#pragma weak pmpi_type_ub = ompi_type_ub_f
#pragma weak pmpi_type_ub_ = ompi_type_ub_f
#pragma weak pmpi_type_ub__ = ompi_type_ub_f

#pragma weak PMPI_Type_ub_f = ompi_type_ub_f
#pragma weak PMPI_Type_ub_f08 = ompi_type_ub_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_UB,
                           pmpi_type_ub,
                           pmpi_type_ub_,
                           pmpi_type_ub__,
                           pompi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_UB = ompi_type_ub_f
#pragma weak mpi_type_ub = ompi_type_ub_f
#pragma weak mpi_type_ub_ = ompi_type_ub_f
#pragma weak mpi_type_ub__ = ompi_type_ub_f

#pragma weak MPI_Type_ub_f = ompi_type_ub_f
#pragma weak MPI_Type_ub_f08 = ompi_type_ub_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_UB,
                           mpi_type_ub,
                           mpi_type_ub_,
                           mpi_type_ub__,
                           ompi_type_ub_f,
                           (MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr),
                           (mtype, ub, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_ub_f(MPI_Fint *mtype, MPI_Fint *ub, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_mtype = MPI_Type_f2c(*mtype);
    MPI_Aint c_ub;

    c_ierr = MPI_Type_ub(c_mtype, &c_ub);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *ub = OMPI_INT_2_FINT(c_ub);
    }
}
