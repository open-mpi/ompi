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
#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"
#include "oshmem/mca/spml/spml.h"

OMPI_GENERATE_FORTRAN_BINDINGS (void,
        SHMEM_QUIET,
        shmem_quiet_,
        shmem_quiet__,
        shmem_quiet_f,
        (void), 
        () )

void shmem_quiet_f(void)
{
    MCA_SPML_CALL(fence());
}
