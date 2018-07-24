/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_ATOMIC_UCX_H
#define MCA_ATOMIC_UCX_H

#include "oshmem_config.h"

#include "opal/mca/common/ucx/common_ucx.h"
#include "opal/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

/* This component does uses SPML:UCX */
#include "oshmem/mca/spml/ucx/spml_ucx.h"


BEGIN_C_DECLS

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_ucx_component;

/* this component works with spml:ucx only */
extern mca_spml_ucx_t *mca_spml_self;

OSHMEM_DECLSPEC void atomic_ucx_lock(int pe);
OSHMEM_DECLSPEC void atomic_ucx_unlock(int pe);

/* API functions */

int mca_atomic_ucx_startup(bool enable_progress_threads, bool enable_threads);
int mca_atomic_ucx_finalize(void);
mca_atomic_base_module_t*
mca_atomic_ucx_query(int *priority);

int mca_atomic_ucx_cswap(shmem_ctx_t ctx,
                         void *target,
                         uint64_t *prev,
                         uint64_t cond,
                         uint64_t value,
                         size_t size,
                         int pe);

struct mca_atomic_ucx_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_ucx_module_t mca_atomic_ucx_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_ucx_module_t);

END_C_DECLS

#endif /* MCA_ATOMIC_UCX_H */
