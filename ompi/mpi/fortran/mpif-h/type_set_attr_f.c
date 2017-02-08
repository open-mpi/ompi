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
#include "ompi/datatype/ompi_datatype.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_TYPE_SET_ATTR = ompi_type_set_attr_f
#pragma weak pmpi_type_set_attr = ompi_type_set_attr_f
#pragma weak pmpi_type_set_attr_ = ompi_type_set_attr_f
#pragma weak pmpi_type_set_attr__ = ompi_type_set_attr_f

#pragma weak PMPI_Type_set_attr_f = ompi_type_set_attr_f
#pragma weak PMPI_Type_set_attr_f08 = ompi_type_set_attr_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SET_ATTR,
                           pmpi_type_set_attr,
                           pmpi_type_set_attr_,
                           pmpi_type_set_attr__,
                           pompi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SET_ATTR = ompi_type_set_attr_f
#pragma weak mpi_type_set_attr = ompi_type_set_attr_f
#pragma weak mpi_type_set_attr_ = ompi_type_set_attr_f
#pragma weak mpi_type_set_attr__ = ompi_type_set_attr_f

#pragma weak MPI_Type_set_attr_f = ompi_type_set_attr_f
#pragma weak MPI_Type_set_attr_f08 = ompi_type_set_attr_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SET_ATTR,
                           mpi_type_set_attr,
                           mpi_type_set_attr_,
                           mpi_type_set_attr__,
                           ompi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#else
#define ompi_type_set_attr_f pompi_type_set_attr_f
#endif
#endif

void ompi_type_set_attr_f(MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*type);

    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_ierr = ompi_attr_set_aint(TYPE_ATTR,
                                c_type,
                                &c_type->d_keyhash,
                                OMPI_FINT_2_INT(*type_keyval),
                                *attribute_val,
                                false);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
