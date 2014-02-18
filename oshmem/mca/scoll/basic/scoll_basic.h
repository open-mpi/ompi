/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_SCOLL_BASIC_H
#define MCA_SCOLL_BASIC_H

#include "oshmem_config.h"

#include "opal/mca/mca.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/util/oshmem_util.h"

BEGIN_C_DECLS

/* These functions (BARRIER_FUNC, BCAST_FUNC)  may be called from any basic algorithm.
 * In case of shmem, the implementation of broadcast doesn't require
 * each process to know message size ( just root should know).
 * It differs from other implementations, so it may cause problems if
 * BCAST_FUNC is a callback to another implementation (e.g, fca, hcoll).
 * So we replace a callback (group->g_scoll.scoll_[func])
 * with a corresponding basic function. */

#define BARRIER_FUNC mca_scoll_basic_barrier
#define BCAST_FUNC mca_scoll_basic_broadcast

/* Globally exported variables */

OSHMEM_MODULE_DECLSPEC extern mca_scoll_base_component_1_0_0_t
mca_scoll_basic_component;

extern int mca_scoll_basic_priority_param;
OSHMEM_DECLSPEC extern int mca_scoll_basic_param_barrier_algorithm;
extern int mca_scoll_basic_param_broadcast_algorithm;
extern int mca_scoll_basic_param_collect_algorithm;
extern int mca_scoll_basic_param_reduce_algorithm;

/* API functions */

int mca_scoll_basic_init(bool enable_progress_threads, bool enable_threads);
mca_scoll_base_module_t*
mca_scoll_basic_query(struct oshmem_group_t *group, int *priority);

enum {
    SHMEM_SYNC_INIT = _SHMEM_SYNC_VALUE,
    SHMEM_SYNC_WAIT = -2,
    SHMEM_SYNC_RUN = -3,
    SHMEM_SYNC_READY = -4,
};

int mca_scoll_basic_barrier(struct oshmem_group_t *group, long *pSync, int alg);
int mca_scoll_basic_broadcast(struct oshmem_group_t *group,
                              int PE_root,
                              void *target,
                              const void *source,
                              size_t nlong,
                              long *pSync,
                              int alg);
int mca_scoll_basic_collect(struct oshmem_group_t *group,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            bool nlong_type,
                            int alg);
int mca_scoll_basic_reduce(struct oshmem_group_t *group,
                           struct oshmem_op_t *op,
                           void *target,
                           const void *source,
                           size_t nlong,
                           long *pSync,
                           void *pWrk,
                           int alg);

static inline unsigned int scoll_log2(unsigned long val)
{
    unsigned int count = 0;

    while (val > 0) {
        val = val >> 1;
        count++;
    }

    return count > 0 ? count - 1 : 0;
}

struct mca_scoll_basic_module_t {
    mca_scoll_base_module_t super;
};
typedef struct mca_scoll_basic_module_t mca_scoll_basic_module_t;
OBJ_CLASS_DECLARATION(mca_scoll_basic_module_t);

END_C_DECLS

#endif /* MCA_SCOLL_BASIC_H */
