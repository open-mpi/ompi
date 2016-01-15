/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include <string.h>

#include "ompi/mpi/fortran/base/strings.h"

#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INFO_GET_VERSION,
        shmem_info_get_version_,
        shmem_info_get_version__,
        shmem_info_get_version_f,
        (MPI_Fint *major, MPI_Fint *minor),
        (major,minor) )

void shmem_info_get_version_f(MPI_Fint *major, MPI_Fint *minor)
{
    int c_major, c_minor;

    shmem_info_get_version(&c_major, &c_minor);
    *major = OMPI_INT_2_FINT(c_major);
    *minor = OMPI_INT_2_FINT(c_minor);
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INFO_GET_NAME,
        shmem_info_get_name_,
        shmem_info_get_name__,
        shmem_info_get_name_f,
        (char *name),
        (name) )

void shmem_info_get_name_f(char *name)
{
    char c_name[SHMEM_MAX_NAME_LEN];

    shmem_info_get_name(c_name);
    ompi_fortran_string_c2f(c_name, name, strlen(c_name) + 1);
}
