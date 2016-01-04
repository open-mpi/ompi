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
#pragma weak PMPI_PCONTROL = ompi_pcontrol_f
#pragma weak pmpi_pcontrol = ompi_pcontrol_f
#pragma weak pmpi_pcontrol_ = ompi_pcontrol_f
#pragma weak pmpi_pcontrol__ = ompi_pcontrol_f

#pragma weak PMPI_Pcontrol_f = ompi_pcontrol_f
#pragma weak PMPI_Pcontrol_f08 = ompi_pcontrol_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PCONTROL,
                           pmpi_pcontrol,
                           pmpi_pcontrol_,
                           pmpi_pcontrol__,
                           pompi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PCONTROL = ompi_pcontrol_f
#pragma weak mpi_pcontrol = ompi_pcontrol_f
#pragma weak mpi_pcontrol_ = ompi_pcontrol_f
#pragma weak mpi_pcontrol__ = ompi_pcontrol_f

#pragma weak MPI_Pcontrol_f = ompi_pcontrol_f
#pragma weak MPI_Pcontrol_f08 = ompi_pcontrol_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PCONTROL,
                           mpi_pcontrol,
                           mpi_pcontrol_,
                           mpi_pcontrol__,
                           ompi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#else
#define ompi_pcontrol_f pompi_pcontrol_f
#endif
#endif


void ompi_pcontrol_f(MPI_Fint *level)
{
    PMPI_Pcontrol(OMPI_FINT_2_INT(*level));
}
