/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/fortran_base_strings.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_GET_NAME = ompi_comm_get_name_f
#pragma weak pmpi_comm_get_name = ompi_comm_get_name_f
#pragma weak pmpi_comm_get_name_ = ompi_comm_get_name_f
#pragma weak pmpi_comm_get_name__ = ompi_comm_get_name_f

#pragma weak PMPI_Comm_get_name_f = ompi_comm_get_name_f
#pragma weak PMPI_Comm_get_name_f08 = ompi_comm_get_name_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_NAME,
                            pmpi_comm_get_name,
                            pmpi_comm_get_name_,
                            pmpi_comm_get_name__,
                            pompi_comm_get_name_f,
                            (MPI_Fint *comm, char *comm_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (comm, comm_name, resultlen, ierr, name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_NAME = ompi_comm_get_name_f
#pragma weak mpi_comm_get_name = ompi_comm_get_name_f
#pragma weak mpi_comm_get_name_ = ompi_comm_get_name_f
#pragma weak mpi_comm_get_name__ = ompi_comm_get_name_f

#pragma weak MPI_Comm_get_name_f = ompi_comm_get_name_f
#pragma weak MPI_Comm_get_name_f08 = ompi_comm_get_name_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_NAME,
                            mpi_comm_get_name,
                            mpi_comm_get_name_,
                            mpi_comm_get_name__,
                            ompi_comm_get_name_f,
                            (MPI_Fint *comm, char *comm_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (comm, comm_name, resultlen, ierr, name_len) )
#else
#define ompi_comm_get_name_f pompi_comm_get_name_f
#endif
#endif


void ompi_comm_get_name_f(MPI_Fint *comm, char *comm_name,
                          MPI_Fint *resultlen, MPI_Fint *ierr,
                          int name_len)
{
    int c_ierr, c_len;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    char c_name[MPI_MAX_OBJECT_NAME];

    c_ierr = PMPI_Comm_get_name(c_comm, c_name, &c_len);
    if (MPI_SUCCESS == c_ierr) {
        ompi_fortran_string_c2f(c_name, comm_name, name_len);
        *resultlen = OMPI_INT_2_FINT(c_len);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    }
}
