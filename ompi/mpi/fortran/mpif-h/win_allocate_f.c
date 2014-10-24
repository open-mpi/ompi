/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2014 High Performance Computing Center Stuttgart,
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
#pragma weak PMPI_WIN_ALLOCATE = ompi_win_allocate_f
#pragma weak pmpi_win_allocate = ompi_win_allocate_f
#pragma weak pmpi_win_allocate_ = ompi_win_allocate_f
#pragma weak pmpi_win_allocate__ = ompi_win_allocate_f

#pragma weak PMPI_Win_allocate_f = ompi_win_allocate_f
#pragma weak PMPI_Win_allocate_f08 = ompi_win_allocate_f

#pragma weak PMPI_WIN_ALLOCATE_CPTR = ompi_win_allocate_f
#pragma weak pmpi_win_allocate_cptr = ompi_win_allocate_f
#pragma weak pmpi_win_allocate_cptr_ = ompi_win_allocate_f
#pragma weak pmpi_win_allocate_cptr__ = ompi_win_allocate_f

#pragma weak PMPI_Win_allocate_cptr_f = ompi_win_allocate_f
#pragma weak PMPI_Win_allocate_cptr_f08 = ompi_win_allocate_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_ALLOCATE,
                            pmpi_win_allocate,
                            pmpi_win_allocate_,
                            pmpi_win_allocate__,
                            pompi_win_allocate_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )

OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_ALLOCATE_CPTR,
                            pmpi_win_allocate_cptr,
                            pmpi_win_allocate_cptr_,
                            pmpi_win_allocate_cptr__,
                            pompi_win_allocate_cptr_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_ALLOCATE = ompi_win_allocate_f
#pragma weak mpi_win_allocate = ompi_win_allocate_f
#pragma weak mpi_win_allocate_ = ompi_win_allocate_f
#pragma weak mpi_win_allocate__ = ompi_win_allocate_f

#pragma weak MPI_Win_allocate_f = ompi_win_allocate_f
#pragma weak MPI_Win_allocate_f08 = ompi_win_allocate_f

#pragma weak MPI_WIN_ALLOCATE_CPTR = ompi_win_allocate_f
#pragma weak mpi_win_allocate_cptr = ompi_win_allocate_f
#pragma weak mpi_win_allocate_cptr_ = ompi_win_allocate_f
#pragma weak mpi_win_allocate_cptr__ = ompi_win_allocate_f

#pragma weak MPI_Win_allocate_cptr_f = ompi_win_allocate_f
#pragma weak MPI_Win_allocate_cptr_f08 = ompi_win_allocate_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_ALLOCATE,
                            mpi_win_allocate,
                            mpi_win_allocate_,
                            mpi_win_allocate__,
                            ompi_win_allocate_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )

OMPI_GENERATE_F77_BINDINGS (MPI_WIN_ALLOCATE_CPTR,
                            mpi_win_allocate_cptr,
                            mpi_win_allocate_cptr_,
                            mpi_win_allocate_cptr__,
                            ompi_win_allocate_cptr_f,
                            (MPI_Aint *size, MPI_Fint *disp_unit,
                             MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                             MPI_Fint *win, MPI_Fint *ierr),
                            (size, disp_unit, info, comm, baseptr, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_allocate_f(MPI_Aint *size, MPI_Fint *disp_unit,
                                MPI_Fint *info, MPI_Fint *comm, char *baseptr,
                                MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info;
    MPI_Comm c_comm;
    MPI_Win c_win;

    c_info = MPI_Info_f2c(*info);
    c_comm = MPI_Comm_f2c(*comm);

    c_ierr = MPI_Win_allocate(*size, OMPI_FINT_2_INT(*disp_unit),
                                     c_info, c_comm,
                                     baseptr, &c_win);
    *win = MPI_Win_c2f(c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

/*
 * Note that MPI-3 mandates a second form of the
 * MPI_Win_allocate interface -- one that has a "_cptr" suffix.
 */
void ompi_win_allocate_cptr_f(MPI_Aint *size, MPI_Fint *disp_unit,
                                     MPI_Fint *info, MPI_Fint *comm,
                                     char *baseptr,
                                     MPI_Fint *win, MPI_Fint *ierr)
{
    ompi_win_allocate_f(size, disp_unit, info, comm, baseptr,
                               win, ierr);
}
