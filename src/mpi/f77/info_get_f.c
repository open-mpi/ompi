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
#pragma weak PMPI_INFO_GET = mpi_info_get_f
#pragma weak pmpi_info_get = mpi_info_get_f
#pragma weak pmpi_info_get_ = mpi_info_get_f
#pragma weak pmpi_info_get__ = mpi_info_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_GET,
                           pmpi_info_get,
                           pmpi_info_get_,
                           pmpi_info_get__,
                           pmpi_info_get_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, value, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_GET = mpi_info_get_f
#pragma weak mpi_info_get = mpi_info_get_f
#pragma weak mpi_info_get_ = mpi_info_get_f
#pragma weak mpi_info_get__ = mpi_info_get_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_GET,
                           mpi_info_get,
                           mpi_info_get_,
                           mpi_info_get__,
                           mpi_info_get_f,
                           (MPI_Fint *info, char *key, MPI_Fint *valuelen, char *value, MPI_Fint *flag, MPI_Fint *ierr),
                           (info, key, valuelen, value, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_info_get_f(MPI_Fint *info, char *key, MPI_Fint *valuelen,
		    char *value, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Info c_info;
    OMPI_SINGLE_NAME_DECL(flag);

    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Info_get(c_info, key, 
					 OMPI_FINT_2_INT(*valuelen),
					 value,
					 OMPI_SINGLE_NAME_CONVERT(flag)));

    OMPI_SINGLE_INT_2_FINT(flag);
}
