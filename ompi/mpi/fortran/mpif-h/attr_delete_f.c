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
#pragma weak PMPI_ATTR_DELETE = ompi_attr_delete_f
#pragma weak pmpi_attr_delete = ompi_attr_delete_f
#pragma weak pmpi_attr_delete_ = ompi_attr_delete_f
#pragma weak pmpi_attr_delete__ = ompi_attr_delete_f

#pragma weak PMPI_Attr_delete_f = ompi_attr_delete_f
#pragma weak PMPI_Attr_delete_f08 = ompi_attr_delete_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_DELETE,
                           pmpi_attr_delete,
                           pmpi_attr_delete_,
                           pmpi_attr_delete__,
                           pompi_attr_delete_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr),
                           (comm, keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_DELETE = ompi_attr_delete_f
#pragma weak mpi_attr_delete = ompi_attr_delete_f
#pragma weak mpi_attr_delete_ = ompi_attr_delete_f
#pragma weak mpi_attr_delete__ = ompi_attr_delete_f

#pragma weak MPI_Attr_delete_f = ompi_attr_delete_f
#pragma weak MPI_Attr_delete_f08 = ompi_attr_delete_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_DELETE,
                           mpi_attr_delete,
                           mpi_attr_delete_,
                           mpi_attr_delete__,
                           ompi_attr_delete_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr),
                           (comm, keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_attr_delete_f(MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Attr_delete(c_comm, OMPI_FINT_2_INT(*keyval));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
