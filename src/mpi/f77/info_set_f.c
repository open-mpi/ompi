/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INFO_SET = mpi_info_set_f
#pragma weak pmpi_info_set = mpi_info_set_f
#pragma weak pmpi_info_set_ = mpi_info_set_f
#pragma weak pmpi_info_set__ = mpi_info_set_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_SET,
                           pmpi_info_set,
                           pmpi_info_set_,
                           pmpi_info_set__,
                           pmpi_info_set_f,
                           (MPI_Fint *info, char *key, char *value, MPI_Fint *ierr),
                           (info, key, value, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_SET = mpi_info_set_f
#pragma weak mpi_info_set = mpi_info_set_f
#pragma weak mpi_info_set_ = mpi_info_set_f
#pragma weak mpi_info_set__ = mpi_info_set_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_SET,
                           mpi_info_set,
                           mpi_info_set_,
                           mpi_info_set__,
                           mpi_info_set_f,
                           (MPI_Fint *info, char *key, char *value, MPI_Fint *ierr),
                           (info, key, value, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_info_set_f(MPI_Fint *info, char *key, char *value, MPI_Fint *ierr)
{
    MPI_Info c_info;

    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Info_set(c_info, key, value));
}
