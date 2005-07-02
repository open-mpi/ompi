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
#include "attribute/attribute.h"
#include "communicator/communicator.h"

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
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    
    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_err = ompi_attr_get_fortran_mpi1(c_comm->c_keyhash, 
                                       OMPI_FINT_2_INT(*keyval),
                                       attribute_val, 
                                       &c_flag);
    *ierr = OMPI_INT_2_FINT(c_err);
    *flag = OMPI_INT_2_FINT(c_flag);
}
