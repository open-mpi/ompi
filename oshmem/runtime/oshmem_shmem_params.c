/*
* Copyright (c) 2012      Mellanox Technologies, Inc.
*                         All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/

#include "params.h"
#include "runtime.h"
#include "oshmem/constants.h"

#include "opal/mca/base/mca_base_param.h"

int oshmem_shmem_lock_recursive = 0;
int oshmem_shmem_api_verbose = 0;

int oshmem_shmem_register_params(void)
{
    mca_base_param_reg_int_name("shmem", 
        "lock_recursive",
        "Whether or not distributed locking support recursive calls (default = no)", 
        false, false, 
        0, &oshmem_shmem_lock_recursive);

    mca_base_param_reg_int_name("shmem",
        "api_verbose",
        "Verbosity level of the shmem c functions (default = 0)",
        false, false,
        0, &oshmem_shmem_api_verbose); 

    return OSHMEM_SUCCESS;
}
