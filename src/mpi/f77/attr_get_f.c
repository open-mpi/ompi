/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ATTR_GET = mpi_attr_get_f
#pragma weak pmpi_attr_get = mpi_attr_get_f
#pragma weak pmpi_attr_get_ = mpi_attr_get_f
#pragma weak pmpi_attr_get__ = mpi_attr_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_GET,
                           pmpi_attr_get,
                           pmpi_attr_get_,
                           pmpi_attr_get__,
                           pmpi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_GET = mpi_attr_get_f
#pragma weak mpi_attr_get = mpi_attr_get_f
#pragma weak mpi_attr_get_ = mpi_attr_get_f
#pragma weak mpi_attr_get__ = mpi_attr_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_GET,
                           mpi_attr_get,
                           mpi_attr_get_,
                           mpi_attr_get__,
                           mpi_attr_get_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_attr_get_f(MPI_Fint *comm, MPI_Fint *keyval,
		    MPI_Fint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    int c_err, c_flag;
    MPI_Comm c_comm;
    int *c_value;

    c_comm = MPI_Comm_f2c(*comm);
    
    /* This stuff is very confusing.  Be sure to see MPI-2 4.12.7. */

    /* Didn't use all the FINT macros that could have prevented a few
       extra variables in this function, but I figured that the
       clarity of code, and the fact that this is not expected to be a
       high-performance function, was worth it */

    /* Note that this function deals with attribute values that are
       the size of Fortran INTEGERS; MPI_ATTR_GET deals with attribute
       values that are the size of address integers.  Hence, it is
       possible that you'll lose some precision upon the cast.  Per
       MPI-2 4.12.7, use MPI_xxx_get/put_attr when you need lossless
       conversion. */

    c_err = MPI_Attr_get(c_comm, OMPI_FINT_2_INT(*keyval), &c_value, &c_flag);
    *ierr = OMPI_INT_2_FINT(c_err);
    *flag = OMPI_INT_2_FINT(c_flag);

    /* Note that MPI-2 4.12.7 specifically says that Fortran's
       ATTR_GET function will take the address returned from C and
       "convert it to an integer" (which assumedly means
       dereference) */

    if (MPI_SUCCESS == c_err && 1 == c_flag) {
        *attribute_val = OMPI_INT_2_FINT(*c_value);
    }
}
