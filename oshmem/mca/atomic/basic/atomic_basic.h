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

#include "opal/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

BEGIN_C_DECLS

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_basic_component;

OSHMEM_DECLSPEC void atomic_basic_lock(int pe);
OSHMEM_DECLSPEC void atomic_basic_unlock(int pe);

/* API functions */

int mca_atomic_basic_init(bool enable_progress_threads, bool enable_threads);
int mca_atomic_basic_finalize(void);
mca_atomic_base_module_t*
mca_atomic_basic_query(int *priority);

int mca_atomic_basic_fadd(void *target,
                          void *prev,
                          const void *value,
                          size_t nlong,
                          int pe,
                          struct oshmem_op_t *op);
int mca_atomic_basic_cswap(void *target,
                           void *prev,
                           const void *cond,
                           const void *value,
                           size_t nlong,
                           int pe);

struct mca_atomic_basic_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_basic_module_t mca_atomic_basic_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_basic_module_t);

END_C_DECLS

#endif /* MCA_ATOMIC_BASIC_H */
