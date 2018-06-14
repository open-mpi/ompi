/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/proc/proc.h"
#include "atomic_mxm.h"

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_atomic_mxm_init(bool enable_progress_threads, bool enable_threads)
{
    return OSHMEM_SUCCESS;
}

int mca_atomic_mxm_finalize(void)
{
    return OSHMEM_SUCCESS;
}

static inline
int mca_atomic_mxm_op_not_implemented(void *target,
                                      const void *value,
                                      size_t nlong,
                                      int pe,
                                      struct oshmem_op_t *op)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static inline
int mca_atomic_mxm_fop_not_implemented(void *target,
                                       void *prev,
                                       const void *value,
                                       size_t nlong,
                                       int pe,
                                       struct oshmem_op_t *op)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

mca_atomic_base_module_t *
mca_atomic_mxm_query(int *priority)
{
    mca_atomic_mxm_module_t *module;

    *priority = mca_atomic_mxm_component.priority;

    module = OBJ_NEW(mca_atomic_mxm_module_t);
    if (module) {
        module->super.atomic_add   = mca_atomic_mxm_add;
        module->super.atomic_and   = mca_atomic_mxm_op_not_implemented;
        module->super.atomic_or    = mca_atomic_mxm_op_not_implemented;
        module->super.atomic_xor   = mca_atomic_mxm_op_not_implemented;
        module->super.atomic_fadd  = mca_atomic_mxm_fadd;
        module->super.atomic_fand  = mca_atomic_mxm_fop_not_implemented;
        module->super.atomic_for   = mca_atomic_mxm_fop_not_implemented;
        module->super.atomic_fxor  = mca_atomic_mxm_fop_not_implemented;
        module->super.atomic_cswap = mca_atomic_mxm_cswap;
        module->super.atomic_cswap = mca_atomic_mxm_cswap;
        return &(module->super);
    }

    return NULL ;
}
