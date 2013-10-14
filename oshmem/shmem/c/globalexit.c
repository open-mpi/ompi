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

extern int inGlobalExit;

void globalexit(int status)
{
    inGlobalExit++;

    orte_errmgr.abort(status, NULL);
    
    oshmem_shmem_aborted = true;
    exit(status);
}
