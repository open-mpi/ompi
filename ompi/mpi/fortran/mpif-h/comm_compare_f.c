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
#pragma weak PMPI_COMM_COMPARE = ompi_comm_compare_f
#pragma weak pmpi_comm_compare = ompi_comm_compare_f
#pragma weak pmpi_comm_compare_ = ompi_comm_compare_f
#pragma weak pmpi_comm_compare__ = ompi_comm_compare_f

#pragma weak PMPI_Comm_compare_f = ompi_comm_compare_f
#pragma weak PMPI_Comm_compare_f08 = ompi_comm_compare_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_COMPARE,
                           pmpi_comm_compare,
                           pmpi_comm_compare_,
                           pmpi_comm_compare__,
                           pompi_comm_compare_f,
                           (MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr),
                           (comm1, comm2, result, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_COMPARE = ompi_comm_compare_f
#pragma weak mpi_comm_compare = ompi_comm_compare_f
#pragma weak mpi_comm_compare_ = ompi_comm_compare_f
#pragma weak mpi_comm_compare__ = ompi_comm_compare_f

#pragma weak MPI_Comm_compare_f = ompi_comm_compare_f
#pragma weak MPI_Comm_compare_f08 = ompi_comm_compare_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_COMPARE,
                           mpi_comm_compare,
                           mpi_comm_compare_,
                           mpi_comm_compare__,
                           ompi_comm_compare_f,
                           (MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr),
                           (comm1, comm2, result, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_compare_f(MPI_Fint *comm1, MPI_Fint *comm2, MPI_Fint *result, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm1 = MPI_Comm_f2c(*comm1);
    MPI_Comm c_comm2 = MPI_Comm_f2c(*comm2);
    OMPI_SINGLE_NAME_DECL(result);

    c_ierr = MPI_Comm_compare(c_comm1, c_comm2, 
                              OMPI_SINGLE_NAME_CONVERT(result));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(result);
    }
}
