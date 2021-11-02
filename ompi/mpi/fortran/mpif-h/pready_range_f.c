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
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
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
#pragma weak PMPI_PREADY_RANGE = ompi_pready_range_f
#pragma weak pmpi_pready_range = ompi_pready_range_f
#pragma weak pmpi_pready_range_ = ompi_pready_range_f
#pragma weak pmpi_pready_range__ = ompi_pready_range_f

#pragma weak PMPI_Pready_range_f = ompi_pready_range_f
#pragma weak PMPI_Pready_range_f08 = ompi_pready_range_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PREADY_RANGE,
                           pmpi_pready_range,
                           pmpi_pready_range_,
                           pmpi_pready_range__,
                           pompi_pready_range_f,
                           (MPI_Fint *partition_low, MPI_Fint *partition_high, MPI_Fint *request, MPI_Fint *ierr),
                           (partition_low, partition_high, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PREADY_RANGE = ompi_pready_range_f
#pragma weak mpi_pready_range = ompi_pready_range_f
#pragma weak mpi_pready_range_ = ompi_pready_range_f
#pragma weak mpi_pready_range__ = ompi_pready_range_f

#pragma weak MPI_Pready_range_f = ompi_pready_range_f
#pragma weak MPI_Pready_range_f08 = ompi_pready_range_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PREADY_RANGE,
                           mpi_pready_range,
                           mpi_pready_range_,
                           mpi_pready_range__,
                           ompi_pready_range_f,
                           (MPI_Fint *partition_low, MPI_Fint *partition_high, MPI_Fint *request, MPI_Fint *ierr),
                           (partition_low, partition_high, request, ierr) )
#else
#define ompi_pready_range_f pompi_pready_range_f
#endif
#endif


void ompi_pready_range_f(MPI_Fint *partition_low, MPI_Fint *partition_high, MPI_Fint *request, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Request c_req = PMPI_Request_f2c(*request);

   c_ierr = PMPI_Pready_range(OMPI_FINT_2_INT(*partition_low), OMPI_FINT_2_INT(*partition_high), c_req);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
   if (MPI_SUCCESS == c_ierr) {
      *request = PMPI_Request_c2f(c_req);
   }
}
