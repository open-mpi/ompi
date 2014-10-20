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

#include "oshmem/include/shmem.h"
#include "oshmem/runtime/runtime.h"

#include "orte/mca/errmgr/errmgr.h"

extern int oshmem_shmem_inglobalexit;

void globalexit(int status)
{
    oshmem_shmem_inglobalexit++;

    orte_errmgr.abort(status, NULL);
    
    oshmem_shmem_aborted = true;
    exit(status);
}
