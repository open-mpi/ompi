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
#pragma weak PMPI_PUBLISH_NAME = mpi_publish_name_f
#pragma weak pmpi_publish_name = mpi_publish_name_f
#pragma weak pmpi_publish_name_ = mpi_publish_name_f
#pragma weak pmpi_publish_name__ = mpi_publish_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PUBLISH_NAME,
                           pmpi_publish_name,
                           pmpi_publish_name_,
                           pmpi_publish_name__,
                           pmpi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PUBLISH_NAME = mpi_publish_name_f
#pragma weak mpi_publish_name = mpi_publish_name_f
#pragma weak mpi_publish_name_ = mpi_publish_name_f
#pragma weak mpi_publish_name__ = mpi_publish_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PUBLISH_NAME,
                           mpi_publish_name,
                           mpi_publish_name_,
                           mpi_publish_name__,
                           mpi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr),
                           (service_name, info, port_name, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_publish_name_f(char *service_name, MPI_Fint *info,
			char *port_name, MPI_Fint *ierr)
{
    MPI_Info c_info;

    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Publish_name(service_name, c_info,
					     port_name));
}
