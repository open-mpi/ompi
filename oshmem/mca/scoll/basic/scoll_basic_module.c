/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <stdio.h>

#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"
#include "scoll_basic.h"

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_scoll_basic_init(bool enable_progress_threads, bool enable_threads)
{
    /* Nothing to do */
    return OSHMEM_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
static int mca_scoll_basic_enable(mca_scoll_base_module_t *module,
                                  struct oshmem_group_t *comm)
{
    /*nothing to do here*/
    return OSHMEM_SUCCESS;
}

mca_scoll_base_module_t *
mca_scoll_basic_query(struct oshmem_group_t *group, int *priority)
{
    mca_scoll_basic_module_t *module;

    *priority = mca_scoll_basic_priority_param;

    module = OBJ_NEW(mca_scoll_basic_module_t);
    if (module) {
        module->super.scoll_barrier = mca_scoll_basic_barrier;
        module->super.scoll_broadcast = mca_scoll_basic_broadcast;
        module->super.scoll_collect = mca_scoll_basic_collect;
        module->super.scoll_reduce = mca_scoll_basic_reduce;
        module->super.scoll_module_enable = mca_scoll_basic_enable;
        return &(module->super);
    }

    return NULL ;
}
