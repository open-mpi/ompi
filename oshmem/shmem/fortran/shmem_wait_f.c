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
#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "ompi/datatype/ompi_datatype.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_WAIT, shmem_wait)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_WAIT,
        shmem_wait_,
        shmem_wait__,
        shmem_wait_f,
        (ompi_fortran_integer_t *var, ompi_fortran_integer_t *value), 
        (var,value))

void shmem_wait_f(ompi_fortran_integer_t *var, ompi_fortran_integer_t *value)
{
    MCA_SPML_CALL(wait((void*)var, SHMEM_CMP_NE, (void*)value, SHMEM_FINT));
}

