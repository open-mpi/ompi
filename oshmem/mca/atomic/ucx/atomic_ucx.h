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

#include "opal/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

/* This component does uses SPML:UCX */
#include "oshmem/mca/spml/ucx/spml_ucx.h"


BEGIN_C_DECLS

#define MCA_ATOMIC_UCX_GTE_VAL(val, src, size, pe)                   \
    if (8 == (size)) {                                               \
        (val) = *(uint64_t*)(src);                                   \
    } else if (4 == (size)) {                                        \
        (val) = *(uint32_t*)(src);                                   \
    } else {                                                         \
        ATOMIC_ERROR("[#%d] Type size must be 4 or 8 bytes.", (pe)); \
        return OSHMEM_ERROR;                                         \
    }

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_ucx_component;

/* this component works with spml:ucx only */
extern mca_spml_ucx_t *mca_spml_self;

OSHMEM_DECLSPEC void atomic_ucx_lock(int pe);
OSHMEM_DECLSPEC void atomic_ucx_unlock(int pe);

/* API functions */

int mca_atomic_ucx_init(bool enable_progress_threads, bool enable_threads);
int mca_atomic_ucx_finalize(void);
mca_atomic_base_module_t*
mca_atomic_ucx_query(int *priority);

int mca_atomic_ucx_add(void *target,
                       const void *value,
                       size_t size,
                       int pe,
                       struct oshmem_op_t *op);
int mca_atomic_ucx_fadd(void *target,
                        void *prev,
                        const void *value,
                        size_t nlong,
                        int pe,
                        struct oshmem_op_t *op);
int mca_atomic_ucx_swap(void *target,
                        void *prev,
                        const void *value,
                        size_t nlong,
                        int pe,
                        struct oshmem_op_t *op);
int mca_atomic_ucx_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t nlong,
                         int pe);

struct mca_atomic_ucx_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_ucx_module_t mca_atomic_ucx_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_ucx_module_t);


void mca_atomic_ucx_complete_cb(void *request, ucs_status_t status);

static inline
ucs_status_t mca_atomic_ucx_wait_request(ucs_status_ptr_t request)
{
    ucs_status_t status;
    int i;

    /* check for request completed or failed */
    if (UCS_OK == request) {
        return UCS_OK;
    } else if (UCS_PTR_IS_ERR(request)) {
        return UCS_PTR_STATUS(request);
    }

    while (1) {
        /* call UCX progress */
        for (i = 0; i < 100; i++) {
            if (UCS_INPROGRESS != (status = ucp_request_check_status(request))) {
                ucp_request_free(request);
                return status;
            }
            ucp_worker_progress(mca_spml_self->ucp_worker);
        }
        /* call OPAL progress on every 100 call to UCX progress */
        opal_progress();
    }
}
END_C_DECLS

#endif /* MCA_ATOMIC_UCX_H */
