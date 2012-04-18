/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "ompi/mpi/fortran/base/strings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_PUBLISH_NAME = ompi_publish_name_f
#pragma weak pmpi_publish_name = ompi_publish_name_f
#pragma weak pmpi_publish_name_ = ompi_publish_name_f
#pragma weak pmpi_publish_name__ = ompi_publish_name_f

#pragma weak PMPI_Publish_name_f = ompi_publish_name_f
#pragma weak PMPI_Publish_name_f08 = ompi_publish_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PUBLISH_NAME,
                           pmpi_publish_name,
                           pmpi_publish_name_,
                           pmpi_publish_name__,
                           pompi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr, int service_name_len, int port_name_len),
                           (service_name, info, port_name, ierr, service_name_len, port_name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PUBLISH_NAME = ompi_publish_name_f
#pragma weak mpi_publish_name = ompi_publish_name_f
#pragma weak mpi_publish_name_ = ompi_publish_name_f
#pragma weak mpi_publish_name__ = ompi_publish_name_f

#pragma weak MPI_Publish_name_f = ompi_publish_name_f
#pragma weak MPI_Publish_name_f08 = ompi_publish_name_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PUBLISH_NAME,
                           mpi_publish_name,
                           mpi_publish_name_,
                           mpi_publish_name__,
                           ompi_publish_name_f,
                           (char *service_name, MPI_Fint *info, char *port_name, MPI_Fint *ierr, int service_name_len, int port_name_len),
                           (service_name, info, port_name, ierr, service_name_len, port_name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_publish_name_f(char *service_name, MPI_Fint *info,
			char *port_name, MPI_Fint *ierr, int service_name_len, int port_name_len)
{
    int c_ierr;
    MPI_Info c_info;
    char *c_service_name;
    char *c_port_name;

    c_info = MPI_Info_f2c(*info);
    ompi_fortran_string_f2c(service_name, service_name_len, &c_service_name);
    ompi_fortran_string_f2c(port_name, port_name_len, &c_port_name);

    c_ierr = MPI_Publish_name(c_service_name, c_info, c_port_name);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free ( c_service_name);
    free ( c_port_name);
}
