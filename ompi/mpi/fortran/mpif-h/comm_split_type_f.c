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
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#pragma weak PMPI_COMM_SPLIT_TYPE = ompi_comm_split_type_f
#pragma weak pmpi_comm_split_type = ompi_comm_split_type_f
#pragma weak pmpi_comm_split_type_ = ompi_comm_split_type_f
#pragma weak pmpi_comm_split_type__ = ompi_comm_split_type_f

#pragma weak PMPI_Comm_split_type_f = ompi_comm_split_type_f
#pragma weak PMPI_Comm_split_type_f08 = ompi_comm_split_type_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPLIT_TYPE,
                            pmpi_comm_split_type,
                            pmpi_comm_split_type_,
                            pmpi_comm_split_type__,
                            pompi_comm_split_type_f,
                            (MPI_Fint *comm, MPI_Fint *split_type, MPI_Fint *key, MPI_Fint *info, MPI_Fint *newcomm, MPI_Fint *ierr),
                            (comm, split_type, key, info, newcomm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPLIT_TYPE = ompi_comm_split_type_f
#pragma weak mpi_comm_split_type = ompi_comm_split_type_f
#pragma weak mpi_comm_split_type_ = ompi_comm_split_type_f
#pragma weak mpi_comm_split_type__ = ompi_comm_split_type_f

#pragma weak MPI_Comm_split_type_f = ompi_comm_split_type_f
#pragma weak MPI_Comm_split_type_f08 = ompi_comm_split_type_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPLIT_TYPE,
                            mpi_comm_split_type,
                            mpi_comm_split_type_,
                            mpi_comm_split_type__,
                            ompi_comm_split_type_f,
                            (MPI_Fint *comm, MPI_Fint *split_type, MPI_Fint *key, MPI_Fint *info, MPI_Fint *newcomm, MPI_Fint *ierr),
                            (comm, split_type, key, info, newcomm, ierr) )
#else
#define ompi_comm_split_type_f pompi_comm_split_type_f
#endif
#endif


void ompi_comm_split_type_f(MPI_Fint *comm, MPI_Fint *split_type, MPI_Fint *key,
                            MPI_Fint *info, MPI_Fint *newcomm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = PMPI_Comm_f2c ( *comm );
    MPI_Info c_info;

    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Comm_split_type(c_comm,
                                  OMPI_FINT_2_INT(*split_type),
                                  OMPI_FINT_2_INT(*key),
                                  c_info,
                                  &c_newcomm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newcomm = PMPI_Comm_c2f (c_newcomm);
    }
}
