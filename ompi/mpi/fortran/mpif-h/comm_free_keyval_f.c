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
#pragma weak PMPI_COMM_FREE_KEYVAL = ompi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval = ompi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval_ = ompi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval__ = ompi_comm_free_keyval_f

#pragma weak PMPI_Comm_free_keyval_f = ompi_comm_free_keyval_f
#pragma weak PMPI_Comm_free_keyval_f08 = ompi_comm_free_keyval_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_FREE_KEYVAL,
                           pmpi_comm_free_keyval,
                           pmpi_comm_free_keyval_,
                           pmpi_comm_free_keyval__,
                           pompi_comm_free_keyval_f,
                           (MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm_keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_FREE_KEYVAL = ompi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval = ompi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval_ = ompi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval__ = ompi_comm_free_keyval_f

#pragma weak MPI_Comm_free_keyval_f = ompi_comm_free_keyval_f
#pragma weak MPI_Comm_free_keyval_f08 = ompi_comm_free_keyval_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_FREE_KEYVAL,
                           mpi_comm_free_keyval,
                           mpi_comm_free_keyval_,
                           mpi_comm_free_keyval__,
                           ompi_comm_free_keyval_f,
                           (MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_free_keyval_f(MPI_Fint *comm_keyval, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_SINGLE_NAME_DECL(comm_keyval);

    OMPI_SINGLE_FINT_2_INT(comm_keyval);

    c_ierr = MPI_Comm_free_keyval(OMPI_SINGLE_NAME_CONVERT(comm_keyval));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(comm_keyval);
    }
}
