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
#pragma weak PMPI_RPUT = ompi_rput_f
#pragma weak pmpi_rput = ompi_rput_f
#pragma weak pmpi_rput_ = ompi_rput_f
#pragma weak pmpi_rput__ = ompi_rput_f

#pragma weak PMPI_Rput_f = ompi_rput_f
#pragma weak PMPI_Rput_f08 = ompi_rput_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_RPUT,
                            pmpi_rput,
                            pmpi_rput_,
                            pmpi_rput__,
                            pompi_rput_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *request, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RPUT = ompi_rput_f
#pragma weak mpi_rput = ompi_rput_f
#pragma weak mpi_rput_ = ompi_rput_f
#pragma weak mpi_rput__ = ompi_rput_f

#pragma weak MPI_Rput_f = ompi_rput_f
#pragma weak MPI_Rput_f08 = ompi_rput_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_RPUT,
                            mpi_rput,
                            mpi_rput_,
                            mpi_rput__,
                            ompi_rput_f,
                            (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *request, MPI_Fint *ierr),
                            (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_rput_f(char *origin_addr, MPI_Fint *origin_count,
                 MPI_Fint *origin_datatype, MPI_Fint *target_rank,
                 MPI_Aint *target_disp, MPI_Fint *target_count,
                 MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *request,
                 MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_origin_datatype = MPI_Type_f2c(*origin_datatype);
    MPI_Datatype c_target_datatype = MPI_Type_f2c(*target_datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);
    MPI_Request c_req;

    c_ierr = MPI_Rput(OMPI_F2C_BOTTOM(origin_addr),
                      OMPI_FINT_2_INT(*origin_count),
                      c_origin_datatype,
                      OMPI_FINT_2_INT(*target_rank),
                      *target_disp,
                      OMPI_FINT_2_INT(*target_count),
                      c_target_datatype, c_win, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS != c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}
