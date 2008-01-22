/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_windows.h"

/*
 * Local functions
 */
static int windows_module_init(void);
static int windows_module_finalize(void);
static int windows_module_get_num_procs(int *num_procs);
static int windows_module_set(opal_paffinity_base_cpu_set_t cpumask);
static int windows_module_get(opal_paffinity_base_cpu_set_t *cpumask);

static SYSTEM_INFO sys_info;

/*
 * Linux paffinity module 
 */
static const opal_paffinity_base_module_1_1_0_t module = {

    /* Initialization function */

    windows_module_init,

    /* Module function pointers */
    windows_module_set,
    windows_module_get,
    opal_paffinity_base_map_to_processor_id,
    opal_paffinity_base_map_to_socket_core,
    opal_paffinity_base_max_processor_id,
    opal_paffinity_base_max_socket,
    opal_paffinity_base_max_core,
    windows_module_finalize
};


const opal_paffinity_base_module_1_1_0_t *
opal_paffinity_windows_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "windows", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}

static int windows_module_finalize(void)
{
    return OPAL_SUCCESS;
}

static int windows_module_init(void)
{
    GetSystemInfo( &sys_info );

    return OPAL_SUCCESS;
}


static int windows_module_get_num_procs(int *num_procs)
{
    *num_procs = sys_info.dwNumberOfProcessors;
    return OPAL_SUCCESS;
}

static int windows_module_set(opal_paffinity_base_cpu_set_t cpumask)
{
    HANDLE threadid = GetCurrentThread();
    DWORD_PTR process_mask, system_mask;

    if( !GetProcessAffinityMask( threadid, &process_mask, &system_mask ) ) {
        return OPAL_ERR_NOT_FOUND;
    }

    if( (int)(1 << cpumask.bitmask[0]) & (system_mask & 0xFFFFFFFF) ) {
        process_mask = (int)(1 << cpumask.bitmask[0]);
        if( SetThreadAffinityMask( threadid, process_mask ) )
            return OPAL_SUCCESS;
        /* otherwise something went wrong */
    }
    /* the specified processor is not enabled system wide */
    return OPAL_ERR_NOT_FOUND;
}


static int windows_module_get(opal_paffinity_base_cpu_set_t *cpumask)
{
    HANDLE threadid = GetCurrentThread();
    DWORD_PTR process_mask, system_mask;

    if( GetProcessAffinityMask( threadid, &process_mask, &system_mask ) ) {
        cpumask->bitmask[0] = (opal_paffinity_base_bitmask_t)process_mask;
        return OPAL_SUCCESS;
    }
    cpumask->bitmask[0] = 1;
    return OPAL_ERR_BAD_PARAM;
}

