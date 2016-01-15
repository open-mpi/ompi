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
 * Copyright (c) 2012      University of Oregon.  All rights reserved.
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
#pragma weak PMPI_F_SYNC_REG = ompi_f_sync_reg_f
#pragma weak pmpi_f_sync_reg = ompi_f_sync_reg_f
#pragma weak pmpi_f_sync_reg_ = ompi_f_sync_reg_f
#pragma weak pmpi_f_sync_reg__ = ompi_f_sync_reg_f

#pragma weak PMPI_F_sync_reg_f = ompi_f_sync_reg_f
#pragma weak PMPI_F_sync_reg_f08 = ompi_f_sync_reg_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_F_SYNC_REG,
                           pmpi_f_sync_reg,
                           pmpi_f_sync_reg_,
                           pmpi_f_sync_reg__,
                           pompi_f_sync_reg_f,
                           (char *buf),
                           (buf) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_F_SYNC_REG = ompi_f_sync_reg_f
#pragma weak mpi_f_sync_reg = ompi_f_sync_reg_f
#pragma weak mpi_f_sync_reg_ = ompi_f_sync_reg_f
#pragma weak mpi_f_sync_reg__ = ompi_f_sync_reg_f

#pragma weak MPI_F_sync_reg_f = ompi_f_sync_reg_f
#pragma weak MPI_F_sync_reg_f08 = ompi_f_sync_reg_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_F_SYNC_REG,
                           mpi_f_sync_reg,
                           mpi_f_sync_reg_,
                           mpi_f_sync_reg__,
                           ompi_f_sync_reg_f,
                           (char *buf),
                           (buf) )
#else
#define ompi_f_sync_reg_f pompi_f_sync_reg_f
#endif
#endif

void ompi_f_sync_reg_f(char *buf)
{
    /* This is a noop in C to disable potential Fortran optimizations. */
    return;
}
