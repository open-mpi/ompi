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

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SET_ATTR = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr_ = mpi_comm_set_attr_f
#pragma weak pmpi_comm_set_attr__ = mpi_comm_set_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SET_ATTR,
                           pmpi_comm_set_attr,
                           pmpi_comm_set_attr_,
                           pmpi_comm_set_attr__,
                           pmpi_comm_set_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_ATTR = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr_ = mpi_comm_set_attr_f
#pragma weak mpi_comm_set_attr__ = mpi_comm_set_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SET_ATTR,
                           mpi_comm_set_attr,
                           mpi_comm_set_attr_,
                           mpi_comm_set_attr__,
                           mpi_comm_set_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_set_attr_f(MPI_Fint *comm, MPI_Fint *comm_keyval,
			 MPI_Aint *attribute_val, MPI_Fint *ierr)
{
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    
    *ierr = OMPI_INT_2_FINT(MPI_Comm_set_attr(c_comm,
					      OMPI_FINT_2_INT(*comm_keyval),
					      attribute_val));
}
