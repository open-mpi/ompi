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
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_ALLOCATE_SHARED = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared_ = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared__ = ompi_win_allocate_shared_f

#pragma weak PMPI_Win_allocate_shared_f = ompi_win_allocate_shared_f
#pragma weak PMPI_Win_allocate_shared_f08 = ompi_win_allocate_shared_f

#pragma weak PMPI_WIN_ALLOCATE_SHARED_CPTR = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared_cptr = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared_cptr_ = ompi_win_allocate_shared_f
#pragma weak pmpi_win_allocate_shared_cptr__ = ompi_win_allocate_shared_f

#pragma weak PMPI_Win_allocate_shared_cptr_f = ompi_win_allocate_shared_f
#pragma weak PMPI_Win_allocate_shared_cptr_f08 = ompi_win_allocate_shared_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_ALLOCATE_SHARED,
                            pmpi_win_allocate_shared,
                            pmpi_win_allocate_shared_,
                            pmpi_win_allocate_shared__,
                            pompi_win_allocate_shared_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )

OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_ALLOCATE_SHARED_CPTR,
                            pmpi_win_allocate_shared_cptr,
                            pmpi_win_allocate_shared_cptr_,
                            pmpi_win_allocate_shared_cptr__,
                            pompi_win_allocate_shared_cptr_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_ALLOCATE_SHARED = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared_ = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared__ = ompi_win_allocate_shared_f

#pragma weak MPI_Win_allocate_shared_f = ompi_win_allocate_shared_f
#pragma weak MPI_Win_allocate_shared_f08 = ompi_win_allocate_shared_f

#pragma weak MPI_WIN_ALLOCATE_SHARED_CPTR = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared_cptr = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared_cptr_ = ompi_win_allocate_shared_f
#pragma weak mpi_win_allocate_shared_cptr__ = ompi_win_allocate_shared_f

#pragma weak MPI_Win_allocate_shared_cptr_f = ompi_win_allocate_shared_f
#pragma weak MPI_Win_allocate_shared_cptr_f08 = ompi_win_allocate_shared_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_ALLOCATE_SHARED,
                            mpi_win_allocate_shared,
                            mpi_win_allocate_shared_,
                            mpi_win_allocate_shared__,
                            ompi_win_allocate_shared_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )

OMPI_GENERATE_F77_BINDINGS (MPI_WIN_ALLOCATE_SHARED_CPTR,
                            mpi_win_allocate_shared_cptr,
                            mpi_win_allocate_shared_cptr_,
                            mpi_win_allocate_shared_cptr__,
                            ompi_win_allocate_shared_cptr_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_allocate_shared_f(MPI_Aint *size, MPI_Fint *disp_unit,
                                MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                                MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info;
    MPI_Comm c_comm;
    MPI_Win c_win;

    c_info = MPI_Info_f2c(*info);
    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Win_allocate_shared(*size, OMPI_FINT_2_INT(*disp_unit),
                                     c_info, c_comm,
                                     baseptr, &c_win);
    *win = MPI_Win_c2f(c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

/*
 * Note that MPI-3 mandates a second form of the
 * MPI_Win_allocate_shared interface -- one that has a "_cptr" suffix.
 */
void ompi_win_allocate_shared_cptr_f(MPI_Aint *size, MPI_Fint *disp_unit,
                                     MPI_Fint *info, MPI_Fint *comm,
                                     char *baseptr,
                                     MPI_Fint *win, MPI_Fint *ierr)
{
    ompi_win_allocate_shared_f(size, disp_unit, info, comm, baseptr,
                               win, ierr);
}
