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
#pragma weak PMPI_BSEND = ompi_bsend_f
#pragma weak pmpi_bsend = ompi_bsend_f
#pragma weak pmpi_bsend_ = ompi_bsend_f
#pragma weak pmpi_bsend__ = ompi_bsend_f

#pragma weak PMPI_Bsend_f = ompi_bsend_f
#pragma weak PMPI_Bsend_f08 = ompi_bsend_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BSEND,
                           pmpi_bsend,
                           pmpi_bsend_,
                           pmpi_bsend__,
                           pompi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BSEND = ompi_bsend_f
#pragma weak mpi_bsend = ompi_bsend_f
#pragma weak mpi_bsend_ = ompi_bsend_f
#pragma weak mpi_bsend__ = ompi_bsend_f

#pragma weak MPI_Bsend_f = ompi_bsend_f
#pragma weak MPI_Bsend_f08 = ompi_bsend_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BSEND,
                           mpi_bsend,
                           mpi_bsend_,
                           mpi_bsend__,
                           ompi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#else
#define ompi_bsend_f pompi_bsend_f
#endif
#endif


void ompi_bsend_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type = PMPI_Type_f2c(*datatype);

    c_comm = PMPI_Comm_f2c (*comm);

    c_ierr = PMPI_Bsend(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count),
                       c_type, OMPI_FINT_2_INT(*dest),
                       OMPI_FINT_2_INT(*tag), c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
