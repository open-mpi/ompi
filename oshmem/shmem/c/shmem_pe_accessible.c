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

#include <stdlib.h>

#include "orte/util/show_help.h"

#include "oshmem/constants.h" 
#include "oshmem/include/shmem.h"

#include "oshmem/runtime/runtime.h"

int shmem_pe_accessible(int pe)
{
    RUNTIME_CHECK_INIT();

    /* Assume that everything between 0 and num_pes() is reachable. */
    return 0 <= pe && pe < _num_pes() ? 1 : 0;
}

