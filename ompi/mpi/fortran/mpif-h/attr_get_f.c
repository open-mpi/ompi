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
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ATTR_GET = ompi_attr_get_f
#pragma weak pmpi_attr_get = ompi_attr_get_f
#pragma weak pmpi_attr_get_ = ompi_attr_get_f
#pragma weak pmpi_attr_get__ = ompi_attr_get_f

#pragma weak PMPI_Attr_get_f = ompi_attr_get_f
#pragma weak PMPI_Attr_get_f08 = ompi_attr_get_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_GET,
                           pmpi_attr_get,
                           pmpi_attr_get_,
                           pmpi_attr_get__,
                           pompi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_GET = ompi_attr_get_f
#pragma weak mpi_attr_get = ompi_attr_get_f
#pragma weak mpi_attr_get_ = ompi_attr_get_f
#pragma weak mpi_attr_get__ = ompi_attr_get_f

#pragma weak MPI_Attr_get_f = ompi_attr_get_f
#pragma weak MPI_Attr_get_f08 = ompi_attr_get_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_GET,
                           mpi_attr_get,
                           mpi_attr_get_,
                           mpi_attr_get__,
                           ompi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#else
#define ompi_attr_get_f pompi_attr_get_f
#endif
#endif

void ompi_attr_get_f(MPI_Fint *comm, MPI_Fint *keyval,
                    MPI_Fint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_ierr = ompi_attr_get_fortran_mpi1(c_comm->c_keyhash,
                                        OMPI_FINT_2_INT(*keyval),
                                        attribute_val,
                                        OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_LOGICAL(flag);
}
