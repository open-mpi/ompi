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
#pragma weak PMPI_RACCUMULATE = ompi_raccumulate_f
#pragma weak pmpi_raccumulate = ompi_raccumulate_f
#pragma weak pmpi_raccumulate_ = ompi_raccumulate_f
#pragma weak pmpi_raccumulate__ = ompi_raccumulate_f

#pragma weak PMPI_Raccumulate_f = ompi_raccumulate_f
#pragma weak PMPI_Raccumulate_f08 = ompi_raccumulate_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_RACCUMULATE,
                            pmpi_raccumulate,
                            pmpi_raccumulate_,
                            pmpi_raccumulate__,
                            pompi_raccumulate_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *op, MPI_Fint *win, MPI_Fint *request, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RACCUMULATE = ompi_raccumulate_f
#pragma weak mpi_raccumulate = ompi_raccumulate_f
#pragma weak mpi_raccumulate_ = ompi_raccumulate_f
#pragma weak mpi_raccumulate__ = ompi_raccumulate_f

#pragma weak MPI_Raccumulate_f = ompi_raccumulate_f
#pragma weak MPI_Raccumulate_f08 = ompi_raccumulate_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_RACCUMULATE,
                            mpi_raccumulate,
                            mpi_raccumulate_,
                            mpi_raccumulate__,
                            ompi_raccumulate_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *op, MPI_Fint *win, MPI_Fint *request, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, op, win, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_raccumulate_f(char *origin_addr, MPI_Fint *origin_count,
                        MPI_Fint *origin_datatype, MPI_Fint *target_rank,
                        MPI_Aint *target_disp, MPI_Fint *target_count,
                        MPI_Fint *target_datatype, MPI_Fint *op, MPI_Fint *win,
                        MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;

    MPI_Datatype c_origin_datatype = MPI_Type_f2c(*origin_datatype);
    MPI_Datatype c_target_datatype = MPI_Type_f2c(*target_datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);
    MPI_Op c_op = MPI_Op_f2c(*op);
    MPI_Request c_req;

    ierr_c = MPI_Raccumulate(OMPI_F2C_BOTTOM(origin_addr),
                             OMPI_FINT_2_INT(*origin_count),
                             c_origin_datatype,
                             OMPI_FINT_2_INT(*target_rank),
                             *target_disp,
                             OMPI_FINT_2_INT(*target_count),
                             c_target_datatype, c_op, c_win,
                             &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS != ierr_c) {
        *request = MPI_Request_c2f(c_req);
    }
}
