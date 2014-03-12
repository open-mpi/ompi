/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"


#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMPARE_AND_SWAP = ompi_compare_and_swap_f
#pragma weak pmpi_compare_and_swap = ompi_compare_and_swap_f
#pragma weak pmpi_compare_and_swap_ = ompi_compare_and_swap_f
#pragma weak pmpi_compare_and_swap__ = ompi_compare_and_swap_f

#pragma weak PMPI_Compare_and_swap_f = ompi_compare_and_swap_f
#pragma weak PMPI_Compare_and_swap_f08 = ompi_compare_and_swap_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMPARE_AND_SWAP,
			    pmpi_compare_and_swap,
			    pmpi_compare_and_swap_,
			    pmpi_compare_and_swap__,
			    pompi_compare_and_swap_f,
                            (char *origin_addr, char *compare_addr, char *result_addr, MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *win, MPI_Fint *ierr),
			    (origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMPARE_AND_SWAP = ompi_compare_and_swap_f
#pragma weak mpi_compare_and_swap = ompi_compare_and_swap_f
#pragma weak mpi_compare_and_swap_ = ompi_compare_and_swap_f
#pragma weak mpi_compare_and_swap__ = ompi_compare_and_swap_f

#pragma weak MPI_Compare_and_swap_f = ompi_compare_and_swap_f
#pragma weak MPI_Compare_and_swap_f08 = ompi_compare_and_swap_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMPARE_AND_SWAP,
			    mpi_compare_and_swap,
			    mpi_compare_and_swap_,
			    mpi_compare_and_swap__,
			    ompi_compare_and_swap_f,
                            (char *origin_addr, char *compare_addr, char *result_addr, MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *win, MPI_Fint *ierr),
			    (origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_compare_and_swap_f(char *origin_addr, char *compare_addr, char *result_addr,
                             MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp,
                             MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_datatype = MPI_Type_f2c(*datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);

    c_ierr = MPI_Compare_and_swap(OMPI_F2C_BOTTOM(origin_addr),
                                  OMPI_F2C_BOTTOM(compare_addr),
                                  OMPI_F2C_BOTTOM(result_addr),
                                  c_datatype,
                                  OMPI_FINT_2_INT(*target_rank),
                                  *target_disp, c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
