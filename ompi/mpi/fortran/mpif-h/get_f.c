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
#include "ompi/mpi/fortran/base/constants.h"


#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET = ompi_get_f
#pragma weak pmpi_get = ompi_get_f
#pragma weak pmpi_get_ = ompi_get_f
#pragma weak pmpi_get__ = ompi_get_f

#pragma weak PMPI_Get_f = ompi_get_f
#pragma weak PMPI_Get_f08 = ompi_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET,
                           pmpi_get,
                           pmpi_get_,
                           pmpi_get__,
                           pompi_get_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET = ompi_get_f
#pragma weak mpi_get = ompi_get_f
#pragma weak mpi_get_ = ompi_get_f
#pragma weak mpi_get__ = ompi_get_f

#pragma weak MPI_Get_f = ompi_get_f
#pragma weak MPI_Get_f08 = ompi_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET,
                           mpi_get,
                           mpi_get_,
                           mpi_get__,
                           ompi_get_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_get_f(char *origin_addr, MPI_Fint *origin_count,
	       MPI_Fint *origin_datatype, MPI_Fint *target_rank,
	       MPI_Aint *target_disp, MPI_Fint *target_count,
	       MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_origin_datatype = MPI_Type_f2c(*origin_datatype);
    MPI_Datatype c_target_datatype = MPI_Type_f2c(*target_datatype);
    MPI_Win c_win = MPI_Win_f2c(*win);

    c_ierr = MPI_Get(OMPI_F2C_BOTTOM(origin_addr),
                     OMPI_FINT_2_INT(*origin_count),
                     c_origin_datatype,
                     OMPI_FINT_2_INT(*target_rank),
                     *target_disp,
                     OMPI_FINT_2_INT(*target_count),
                     c_target_datatype, c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
