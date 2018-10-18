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

#include "oshmem/mca/mca.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/util/oshmem_util.h"

/* This component does uses SPML:IKRIT */
#include "oshmem/mca/spml/ikrit/spml_ikrit.h"
#include "oshmem/runtime/runtime.h"


BEGIN_C_DECLS

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_atomic_base_component_1_0_0_t
mca_atomic_mxm_component;

/* this component works with spml:ikrit only */
extern mca_spml_ikrit_t *mca_atomic_mxm_spml_self;

OSHMEM_DECLSPEC void atomic_mxm_lock(int pe);
OSHMEM_DECLSPEC void atomic_mxm_unlock(int pe);

/* API functions */

int mca_atomic_mxm_startup(bool enable_progress_threads, bool enable_threads);
int mca_atomic_mxm_finalize(void);
mca_atomic_base_module_t*
mca_atomic_mxm_query(int *priority);

int mca_atomic_mxm_add(shmem_ctx_t ctx,
                       void *target,
                       uint64_t value,
                       size_t nlong,
                       int pe);
int mca_atomic_mxm_fadd(shmem_ctx_t ctx,
                        void *target,
                        void *prev,
                        uint64_t value,
                        size_t nlong,
                        int pe);
int mca_atomic_mxm_swap(shmem_ctx_t ctx,
                        void *target,
                        void *prev,
                        uint64_t value,
                        size_t nlong,
                        int pe);
int mca_atomic_mxm_cswap(shmem_ctx_t ctx,
                         void *target,
                         uint64_t *prev,
                         uint64_t cond,
                         uint64_t value,
                         size_t nlong,
                         int pe);

struct mca_atomic_mxm_module_t {
    mca_atomic_base_module_t super;
};
typedef struct mca_atomic_mxm_module_t mca_atomic_mxm_module_t;
OBJ_CLASS_DECLARATION(mca_atomic_mxm_module_t);


static inline uint8_t mca_atomic_mxm_order(size_t nlong)
{
    if (OPAL_LIKELY(8 == nlong)) {
        return 3;
    }

    if (OPAL_LIKELY(4 == nlong)) {
        return 2;
    }

    if (2 == nlong) {
        return 1;
    }

    if (1 == nlong) {
        return 0;
    }

    ATOMIC_ERROR("Type size must be 1/2/4 or 8 bytes.");
    oshmem_shmem_abort(-1);
    return OSHMEM_ERR_BAD_PARAM;
}

static inline void mca_atomic_mxm_req_init(mxm_send_req_t *sreq, int pe, void *target, size_t nlong)
{
    uint8_t nlong_order;
    void *remote_addr;
    mxm_mem_key_t *mkey;

    nlong_order = mca_atomic_mxm_order(nlong);

    mkey = mca_spml_ikrit_get_mkey(pe, target, MXM_PTL_RDMA, &remote_addr, mca_atomic_mxm_spml_self);

    /* mxm request init */
    sreq->base.state        = MXM_REQ_NEW;
    sreq->base.mq           = mca_atomic_mxm_spml_self->mxm_mq;
    sreq->base.conn         = mca_atomic_mxm_spml_self->mxm_peers[pe].mxm_hw_rdma_conn;
    sreq->base.completed_cb = NULL;
    sreq->base.data_type    = MXM_REQ_DATA_BUFFER;

    sreq->base.data.buffer.memh   = MXM_INVALID_MEM_HANDLE;
    sreq->base.data.buffer.length = nlong;

    sreq->op.atomic.remote_vaddr = (uintptr_t) remote_addr;
    sreq->op.atomic.remote_mkey  = mkey;
    sreq->op.atomic.order        = nlong_order;

    sreq->flags = 0;
}

static inline void mca_atomic_mxm_post(mxm_send_req_t *sreq)
{
    mxm_error_t mxm_err;

    mxm_err = mxm_req_send(sreq);
    if (OPAL_UNLIKELY(MXM_OK != mxm_err)) {
        ATOMIC_ERROR("mxm_req_send failed, mxm_error = %d",
                     mxm_err);
        oshmem_shmem_abort(-1);
    }

    mxm_req_wait(&sreq->base);
    if (OPAL_UNLIKELY(MXM_OK != sreq->base.error)) {
        ATOMIC_ERROR("mxm_req_wait got non MXM_OK error: %d",
                     sreq->base.error);
        oshmem_shmem_abort(-1);
    }
}

END_C_DECLS

#endif /* MCA_ATOMIC_MXM_H */
