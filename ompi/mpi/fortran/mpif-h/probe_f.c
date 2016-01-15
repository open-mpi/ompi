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
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
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
#include "ompi/mpi/fortran/mpif-h/status-conversion.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_PROBE = ompi_probe_f
#pragma weak pmpi_probe = ompi_probe_f
#pragma weak pmpi_probe_ = ompi_probe_f
#pragma weak pmpi_probe__ = ompi_probe_f

#pragma weak PMPI_Probe_f = ompi_probe_f
#pragma weak PMPI_Probe_f08 = ompi_probe_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PROBE,
                           pmpi_probe,
                           pmpi_probe_,
                           pmpi_probe__,
                           pompi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PROBE = ompi_probe_f
#pragma weak mpi_probe = ompi_probe_f
#pragma weak mpi_probe_ = ompi_probe_f
#pragma weak mpi_probe__ = ompi_probe_f

#pragma weak MPI_Probe_f = ompi_probe_f
#pragma weak MPI_Probe_f08 = ompi_probe_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PROBE,
                           mpi_probe,
                           mpi_probe_,
                           mpi_probe__,
                           ompi_probe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, status, ierr) )
#else
#define ompi_probe_f pompi_probe_f
#endif
#endif


void ompi_probe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)

    c_comm = PMPI_Comm_f2c (*comm);

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)

    c_ierr = PMPI_Probe(OMPI_FINT_2_INT(*source),
                       OMPI_FINT_2_INT(*tag),
                       c_comm, c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
}
