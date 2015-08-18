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
#pragma weak PMPI_CART_SHIFT = ompi_cart_shift_f
#pragma weak pmpi_cart_shift = ompi_cart_shift_f
#pragma weak pmpi_cart_shift_ = ompi_cart_shift_f
#pragma weak pmpi_cart_shift__ = ompi_cart_shift_f

#pragma weak PMPI_Cart_shift_f = ompi_cart_shift_f
#pragma weak PMPI_Cart_shift_f08 = ompi_cart_shift_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_SHIFT,
                           pmpi_cart_shift,
                           pmpi_cart_shift_,
                           pmpi_cart_shift__,
                           pompi_cart_shift_f,
                           (MPI_Fint *comm, MPI_Fint *direction, MPI_Fint *disp, MPI_Fint *rank_source, MPI_Fint *rank_dest, MPI_Fint *ierr),
                           (comm, direction, disp, rank_source, rank_dest, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_SHIFT = ompi_cart_shift_f
#pragma weak mpi_cart_shift = ompi_cart_shift_f
#pragma weak mpi_cart_shift_ = ompi_cart_shift_f
#pragma weak mpi_cart_shift__ = ompi_cart_shift_f

#pragma weak MPI_Cart_shift_f = ompi_cart_shift_f
#pragma weak MPI_Cart_shift_f08 = ompi_cart_shift_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_SHIFT,
                           mpi_cart_shift,
                           mpi_cart_shift_,
                           mpi_cart_shift__,
                           ompi_cart_shift_f,
                           (MPI_Fint *comm, MPI_Fint *direction, MPI_Fint *disp, MPI_Fint *rank_source, MPI_Fint *rank_dest, MPI_Fint *ierr),
                           (comm, direction, disp, rank_source, rank_dest, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_cart_shift_f(MPI_Fint *comm, MPI_Fint *direction, MPI_Fint *disp,
		      MPI_Fint *rank_source, MPI_Fint *rank_dest,
		      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(rank_source);
    OMPI_SINGLE_NAME_DECL(rank_dest);

    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Cart_shift(c_comm,
                            OMPI_FINT_2_INT(*direction),
                            OMPI_FINT_2_INT(*disp),
                            OMPI_SINGLE_NAME_CONVERT(rank_source),
                            OMPI_SINGLE_NAME_CONVERT(rank_dest));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(rank_source);
        OMPI_SINGLE_INT_2_FINT(rank_dest);
    }
}
