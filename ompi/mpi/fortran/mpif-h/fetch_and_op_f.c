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
#pragma weak PMPI_FETCH_AND_OP = ompi_fetch_and_op_f
#pragma weak pmpi_fetch_and_op = ompi_fetch_and_op_f
#pragma weak pmpi_fetch_and_op_ = ompi_fetch_and_op_f
#pragma weak pmpi_fetch_and_op__ = ompi_fetch_and_op_f

#pragma weak PMPI_Fetch_and_op_f = ompi_fetch_and_op_f
#pragma weak PMPI_Fetch_and_op_f08 = ompi_fetch_and_op_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FETCH_AND_OP,
			    pmpi_fetch_and_op,
			    pmpi_fetch_and_op_,
			    pmpi_fetch_and_op__,
			    pompi_fetch_and_op_f,
                            (char *origin_addr, char *result_addr, MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr),
			    (origin_addr, result_addr, datatype, target_rank, target_disp, op, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FETCH_AND_OP = ompi_fetch_and_op_f
#pragma weak mpi_fetch_and_op = ompi_fetch_and_op_f
#pragma weak mpi_fetch_and_op_ = ompi_fetch_and_op_f
#pragma weak mpi_fetch_and_op__ = ompi_fetch_and_op_f

#pragma weak MPI_Fetch_and_op_f = ompi_fetch_and_op_f
#pragma weak MPI_Fetch_and_op_f08 = ompi_fetch_and_op_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FETCH_AND_OP,
			    mpi_fetch_and_op,
			    mpi_fetch_and_op_,
			    mpi_fetch_and_op__,
			    ompi_fetch_and_op_f,
                            (char *origin_addr, char *result_addr, MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr),
			    (origin_addr, result_addr, datatype, target_rank, target_disp, op, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_fetch_and_op_f(char *origin_addr, char *result_addr, MPI_Fint *datatype,
                         MPI_Fint *target_rank, MPI_Aint *target_disp,
                         MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_datatype = MPI_Type_f2c(*datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);
    MPI_Op c_op = MPI_Op_f2c(*op);

    c_ierr = MPI_Fetch_and_op(OMPI_F2C_BOTTOM(origin_addr),
                              OMPI_F2C_BOTTOM(result_addr),
                              c_datatype,
                              OMPI_FINT_2_INT(*target_rank),
                              *target_disp, c_op, c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
