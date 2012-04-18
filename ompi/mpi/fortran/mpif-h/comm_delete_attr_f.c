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
#pragma weak PMPI_COMM_DELETE_ATTR = ompi_comm_delete_attr_f
#pragma weak pmpi_comm_delete_attr = ompi_comm_delete_attr_f
#pragma weak pmpi_comm_delete_attr_ = ompi_comm_delete_attr_f
#pragma weak pmpi_comm_delete_attr__ = ompi_comm_delete_attr_f

#pragma weak PMPI_Comm_delete_attr_f = ompi_comm_delete_attr_f
#pragma weak PMPI_Comm_delete_attr_f08 = ompi_comm_delete_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_DELETE_ATTR,
                           pmpi_comm_delete_attr,
                           pmpi_comm_delete_attr_,
                           pmpi_comm_delete_attr__,
                           pompi_comm_delete_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm, comm_keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_DELETE_ATTR = ompi_comm_delete_attr_f
#pragma weak mpi_comm_delete_attr = ompi_comm_delete_attr_f
#pragma weak mpi_comm_delete_attr_ = ompi_comm_delete_attr_f
#pragma weak mpi_comm_delete_attr__ = ompi_comm_delete_attr_f

#pragma weak MPI_Comm_delete_attr_f = ompi_comm_delete_attr_f
#pragma weak MPI_Comm_delete_attr_f08 = ompi_comm_delete_attr_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_DELETE_ATTR,
                           mpi_comm_delete_attr,
                           mpi_comm_delete_attr_,
                           mpi_comm_delete_attr__,
                           ompi_comm_delete_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm, comm_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_delete_attr_f(MPI_Fint *comm, MPI_Fint *comm_keyval,
			    MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    
    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Comm_delete_attr(c_comm, OMPI_FINT_2_INT(*comm_keyval));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
