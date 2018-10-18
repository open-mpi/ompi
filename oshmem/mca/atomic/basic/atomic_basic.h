/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ATOMIC_BASIC_H
#define MCA_ATOMIC_BASIC_H

#include "oshmem_config.h"

#include "oshmem/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

BEGIN_C_DECLS

#define MCA_BASIC_OP(size, op4, op8) ((size == sizeof(uint64_t)) ? (op8) : (op4))

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_basic_component;

OSHMEM_DECLSPEC void atomic_basic_lock(shmem_ctx_t ctx, int pe);
OSHMEM_DECLSPEC void atomic_basic_unlock(shmem_ctx_t ctx, int pe);

/* API functions */

int mca_atomic_basic_startup(bool enable_progress_threads, bool enable_threads);
int mca_atomic_basic_finalize(void);
mca_atomic_base_module_t*
mca_atomic_basic_query(int *priority);

int mca_atomic_basic_cswap(shmem_ctx_t ctx,
                           void *target,
                           uint64_t *prev,
                           uint64_t cond,
                           uint64_t value,
                           size_t size,
                           int pe);

struct mca_atomic_basic_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_basic_module_t mca_atomic_basic_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_basic_module_t);

END_C_DECLS

#endif /* MCA_ATOMIC_BASIC_H */
