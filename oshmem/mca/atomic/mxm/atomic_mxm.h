/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_ATOMIC_MXM_H
#define MCA_ATOMIC_MXM_H

#include "oshmem_config.h"

#include "opal/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

/* This component does uses SPML:IKRIT */
#include "oshmem/mca/spml/ikrit/spml_ikrit.h"


BEGIN_C_DECLS

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_mxm_component;

/* this component works with spml:ikrit only */
extern mca_spml_ikrit_t *mca_spml_self;

OSHMEM_DECLSPEC void atomic_mxm_lock(int pe);
OSHMEM_DECLSPEC void atomic_mxm_unlock(int pe);

/* API functions */

int mca_atomic_mxm_init(bool enable_progress_threads, bool enable_threads);
int mca_atomic_mxm_finalize(void);
mca_atomic_base_module_t*
mca_atomic_mxm_query(int *priority);

int mca_atomic_mxm_fadd(void *target,
                        void *prev,
                        const void *value,
                        size_t nlong,
                        int pe,
                        struct oshmem_op_t *op);
int mca_atomic_mxm_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t nlong,
                         int pe);

struct mca_atomic_mxm_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_mxm_module_t mca_atomic_mxm_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_mxm_module_t);

END_C_DECLS

#if MXM_API >= MXM_VERSION(2,0)
static inline mxm_mem_key_t *to_mxm_mkey(sshmem_mkey_t *mkey) {

    if (0 == mkey->len) {
        return &mxm_empty_mem_key;
    }
    return (mxm_mem_key_t *)mkey->u.data;
}
#endif

#endif /* MCA_ATOMIC_MXM_H */
