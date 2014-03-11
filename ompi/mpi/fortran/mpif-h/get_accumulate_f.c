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
#pragma weak PMPI_GET_ACCUMULATE = ompi_get_accumulate_f
#pragma weak pmpi_get_accumulate = ompi_get_accumulate_f
#pragma weak pmpi_get_accumulate_ = ompi_get_accumulate_f
#pragma weak pmpi_get_accumulate__ = ompi_get_accumulate_f

#pragma weak PMPI_Get_accumulate_f = ompi_get_accumulate_f
#pragma weak PMPI_Get_accumulate_f08 = ompi_get_accumulate_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_ACCUMULATE,
                            pmpi_get_accumulate,
                            pmpi_get_accumulate_,
                            pmpi_get_accumulate__,
                            pompi_get_accumulate_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, char *result_addr, MPI_Fint *result_count, MPI_Fint *result_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ACCUMULATE = ompi_get_accumulate_f
#pragma weak mpi_get_accumulate = ompi_get_accumulate_f
#pragma weak mpi_get_accumulate_ = ompi_get_accumulate_f
#pragma weak mpi_get_accumulate__ = ompi_get_accumulate_f

#pragma weak MPI_Get_accumulate_f = ompi_get_accumulate_f
#pragma weak MPI_Get_accumulate_f08 = ompi_get_accumulate_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_ACCUMULATE,
                            mpi_get_accumulate,
                            mpi_get_accumulate_,
                            mpi_get_accumulate__,
                            ompi_get_accumulate_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, char *result_addr, MPI_Fint *result_count, MPI_Fint *result_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, target_rank, target_disp, target_count, target_datatype, op, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_get_accumulate_f(char *origin_addr, MPI_Fint *origin_count,
                           MPI_Fint *origin_datatype, char *result_addr,
                           MPI_Fint *result_count, MPI_Fint *result_datatype,
                           MPI_Fint *target_rank, MPI_Aint *target_disp,
                           MPI_Fint *target_count, MPI_Fint *target_datatype,
                           MPI_Fint *op, MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_origin_datatype = MPI_Type_f2c(*origin_datatype);
    MPI_Datatype c_result_datatype = MPI_Type_f2c(*result_datatype);
    MPI_Datatype c_target_datatype = MPI_Type_f2c(*target_datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);
    MPI_Op c_op = MPI_Op_f2c(*op);

    c_ierr = MPI_Get_accumulate(OMPI_F2C_BOTTOM(origin_addr),
                                OMPI_FINT_2_INT(*origin_count),
                                c_origin_datatype,
                                OMPI_F2C_BOTTOM(result_addr),
                                OMPI_FINT_2_INT(*result_count),
                                c_result_datatype,
                                OMPI_FINT_2_INT(*target_rank),
                                *target_disp,
                                OMPI_FINT_2_INT(*target_count),
                                c_target_datatype, c_op, c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
