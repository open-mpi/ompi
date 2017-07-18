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
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ATTR_PUT = ompi_attr_put_f
#pragma weak pmpi_attr_put = ompi_attr_put_f
#pragma weak pmpi_attr_put_ = ompi_attr_put_f
#pragma weak pmpi_attr_put__ = ompi_attr_put_f

#pragma weak PMPI_Attr_put_f = ompi_attr_put_f
#pragma weak PMPI_Attr_put_f08 = ompi_attr_put_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_PUT,
                           pmpi_attr_put,
                           pmpi_attr_put_,
                           pmpi_attr_put__,
                           pompi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_PUT = ompi_attr_put_f
#pragma weak mpi_attr_put = ompi_attr_put_f
#pragma weak mpi_attr_put_ = ompi_attr_put_f
#pragma weak mpi_attr_put__ = ompi_attr_put_f

#pragma weak MPI_Attr_put_f = ompi_attr_put_f
#pragma weak MPI_Attr_put_f08 = ompi_attr_put_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_PUT,
                           mpi_attr_put,
                           mpi_attr_put_,
                           mpi_attr_put__,
                           ompi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#else
#define ompi_attr_put_f pompi_attr_put_f
#endif
#endif

void ompi_attr_put_f(MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val,
		    MPI_Fint *ierr)
{
    int c_err;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_err = ompi_attr_set_fint(COMM_ATTR,
                               c_comm,
                               &c_comm->c_keyhash,
                               OMPI_FINT_2_INT(*keyval),
                               *attribute_val,
                               false);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_err);
}
