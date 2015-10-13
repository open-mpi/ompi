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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_TYPE_LB = ompi_type_lb_f
#pragma weak pmpi_type_lb = ompi_type_lb_f
#pragma weak pmpi_type_lb_ = ompi_type_lb_f
#pragma weak pmpi_type_lb__ = ompi_type_lb_f

#pragma weak PMPI_Type_lb_f = ompi_type_lb_f
#pragma weak PMPI_Type_lb_f08 = ompi_type_lb_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_LB,
                           pmpi_type_lb,
                           pmpi_type_lb_,
                           pmpi_type_lb__,
                           pompi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_LB = ompi_type_lb_f
#pragma weak mpi_type_lb = ompi_type_lb_f
#pragma weak mpi_type_lb_ = ompi_type_lb_f
#pragma weak mpi_type_lb__ = ompi_type_lb_f

#pragma weak MPI_Type_lb_f = ompi_type_lb_f
#pragma weak MPI_Type_lb_f08 = ompi_type_lb_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_LB,
                           mpi_type_lb,
                           mpi_type_lb_,
                           mpi_type_lb__,
                           ompi_type_lb_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr),
                           (type, lb, ierr) )
#else
#define ompi_type_lb_f pompi_type_lb_f
#endif
#endif


void ompi_type_lb_f(MPI_Fint *type, MPI_Fint *lb, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*type);
    MPI_Aint c_lb;

    c_ierr = PMPI_Type_lb(c_type, &c_lb);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *lb = OMPI_INT_2_FINT(c_lb);
    }
}
