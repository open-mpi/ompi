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
#pragma weak PMPI_PACK_SIZE = ompi_pack_size_f
#pragma weak pmpi_pack_size = ompi_pack_size_f
#pragma weak pmpi_pack_size_ = ompi_pack_size_f
#pragma weak pmpi_pack_size__ = ompi_pack_size_f

#pragma weak PMPI_Pack_size_f = ompi_pack_size_f
#pragma weak PMPI_Pack_size_f08 = ompi_pack_size_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK_SIZE,
                           pmpi_pack_size,
                           pmpi_pack_size_,
                           pmpi_pack_size__,
                           pompi_pack_size_f,
                           (MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (incount, datatype, comm, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_SIZE = ompi_pack_size_f
#pragma weak mpi_pack_size = ompi_pack_size_f
#pragma weak mpi_pack_size_ = ompi_pack_size_f
#pragma weak mpi_pack_size__ = ompi_pack_size_f

#pragma weak MPI_Pack_size_f = ompi_pack_size_f
#pragma weak MPI_Pack_size_f08 = ompi_pack_size_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PACK_SIZE,
                           mpi_pack_size,
                           mpi_pack_size_,
                           mpi_pack_size__,
                           ompi_pack_size_f,
                           (MPI_Fint *incount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (incount, datatype, comm, size, ierr) )
#else
#define ompi_pack_size_f pompi_pack_size_f
#endif
#endif


void ompi_pack_size_f(MPI_Fint *incount, MPI_Fint *datatype,
		     MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    OMPI_SINGLE_NAME_DECL(size);

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);

    c_ierr = PMPI_Pack_size(OMPI_FINT_2_INT(*incount),
                           c_type, c_comm,
                           OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
