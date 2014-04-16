/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include "opal/runtime/opal_cr.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/runtime/runtime.h"

void shmem_finalize(void)
{
    OPAL_CR_FINALIZE_LIBRARY();

    oshmem_shmem_finalize();
}

