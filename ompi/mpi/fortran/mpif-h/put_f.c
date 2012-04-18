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
#pragma weak PMPI_PUT = ompi_put_f
#pragma weak pmpi_put = ompi_put_f
#pragma weak pmpi_put_ = ompi_put_f
#pragma weak pmpi_put__ = ompi_put_f

#pragma weak PMPI_Put_f = ompi_put_f
#pragma weak PMPI_Put_f08 = ompi_put_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PUT,
                           pmpi_put,
                           pmpi_put_,
                           pmpi_put__,
                           pompi_put_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PUT = ompi_put_f
#pragma weak mpi_put = ompi_put_f
#pragma weak mpi_put_ = ompi_put_f
#pragma weak mpi_put__ = ompi_put_f

#pragma weak MPI_Put_f = ompi_put_f
#pragma weak MPI_Put_f08 = ompi_put_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PUT,
                           mpi_put,
                           mpi_put_,
                           mpi_put__,
                           ompi_put_f,
                           (char *origin_addr, MPI_Fint *origin_count, MPI_Fint *origin_datatype, MPI_Fint *target_rank, MPI_Aint *target_disp, MPI_Fint *target_count, MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr),
                           (origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_put_f(char *origin_addr, MPI_Fint *origin_count,
	       MPI_Fint *origin_datatype, MPI_Fint *target_rank,
	       MPI_Aint *target_disp, MPI_Fint *target_count,
	       MPI_Fint *target_datatype, MPI_Fint *win, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Datatype c_origin_datatype = MPI_Type_f2c(*origin_datatype);
   MPI_Datatype c_target_datatype = MPI_Type_f2c(*target_datatype);
   MPI_Win c_win = MPI_Win_f2c(*win);

   c_ierr = MPI_Put(OMPI_F2C_BOTTOM(origin_addr),
                    OMPI_FINT_2_INT(*origin_count),
                    c_origin_datatype,
                    OMPI_FINT_2_INT(*target_rank),
                    *target_disp, 
                    OMPI_FINT_2_INT(*target_count),
                    c_target_datatype, c_win);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
