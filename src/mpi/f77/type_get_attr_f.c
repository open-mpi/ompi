/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr__ = mpi_type_get_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_ATTR,
                           pmpi_type_get_attr,
                           pmpi_type_get_attr_,
                           pmpi_type_get_attr__,
                           pmpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr__ = mpi_type_get_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_ATTR,
                           mpi_type_get_attr,
                           mpi_type_get_attr_,
                           mpi_type_get_attr__,
                           mpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_get_attr_f(MPI_Fint *type, MPI_Fint *type_keyval,
			 MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    int c_err, c_flag;
    MPI_Datatype c_type;
    int *c_value;

    c_type = MPI_Type_f2c(*type);

    /* This stuff is very confusing.  Be sure to see MPI-2 4.12.7. */

    /* Didn't use all the FINT macros that could have prevented a few
       extra variables in this function, but I figured that the
       clarity of code, and the fact that this is not expected to be a
       high-performance function, was worth it */

    /* Note that there is no conversion on attribute_val -- MPI-2 says
       that it is supposed to be the right size already */

    c_err = MPI_Type_get_attr(c_type, OMPI_FINT_2_INT(*type_keyval),
                              &c_value, &c_flag);
    *ierr = OMPI_INT_2_FINT(c_err);
    *flag = OMPI_INT_2_FINT(c_flag);

    /* Note that MPI-2 4.12.7 specifically says that Fortran's
       xxx_GET_ATTR functions will take the address returned from C
       and "convert it to an integer" (which assumedly means
       dereference) */

    if (MPI_SUCCESS == c_err && 1 == c_flag) {
        *attribute_val = (MPI_Aint) *c_value;
    }
}
