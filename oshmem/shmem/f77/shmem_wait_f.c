/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/f77/bindings.h"
#include "oshmem/include/shmem.h"
#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "ompi/datatype/ompi_datatype.h"

OMPI_GENERATE_F77_BINDINGS (void,
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

