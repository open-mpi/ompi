/**
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SCOLL_FCA_H
#define MCA_SCOLL_FCA_H
#include "oshmem_config.h"
#include "oshmem/constants.h"
#include "shmem.h"
#include "opal/mca/mca.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/util/oshmem_util.h"
#include "scoll_fca_api.h"
#include "scoll_fca_debug.h"

BEGIN_C_DECLS
struct mca_scoll_fca_component_t {
    /** Base coll component */
    mca_scoll_base_component_1_0_0_t super;

    /** MCA parameter: Priority of this component */
    int fca_priority;

    /** MCA parameter: Verbose level of this component */
    int fca_verbose;

    /** MCA parameter: Path to fca spec file */
    char* fca_spec_file;

    /** MCA parameter: FCA device */
    char* fca_dev;

    /** MCA parameter: Enable FCA */
    int fca_enable;

    /** MCA parameter: Enable FCA Barrier */
    int fca_enable_barrier;

    /** MCA parameter: Enable FCA Bcast */
    int fca_enable_bcast;

    /** MCA parameter: Enable FCA Allreduce */
    int fca_enable_allreduce;

    /** MCA parameter: Enable FCA Allgather */
    int fca_enable_allgather;

    /** MCA parameter: Enable FCA Allgatherv */
    int fca_enable_allgatherv;

    /** MCA parameter: FCA NP */
    int fca_np;

    /* FCA global stuff */
    fca_t *fca_context; /* FCA context handle */

    /*These vars are used as symmetric objects during __fca_comm_new. The proper amount of memory
     is allocated only once during fca_comm_query*/
    int *ret;
    int *rcounts;
    void *my_info_exchangeable;
    void *fca_comm_desc_exchangeable;
};
typedef struct mca_scoll_fca_component_t mca_scoll_fca_component_t;

OSHMEM_MODULE_DECLSPEC extern mca_scoll_fca_component_t mca_scoll_fca_component;

struct mca_scoll_fca_module_t {
    mca_scoll_base_module_t super;
    struct oshmem_group_t *comm;
    int rank;
    int local_proc_idx;
    int num_local_procs;
    int *local_ranks;
    fca_comm_t *fca_comm;
    fca_comm_desc_t fca_comm_desc;
    fca_comm_caps_t fca_comm_caps;

    /* Saved handlers - for fallback */
    mca_scoll_base_module_barrier_fn_t previous_barrier;
    mca_scoll_base_module_t *previous_barrier_module;
    mca_scoll_base_module_broadcast_fn_t previous_broadcast;
    mca_scoll_base_module_t *previous_broadcast_module;
    mca_scoll_base_module_collect_fn_t previous_collect;
    mca_scoll_base_module_t *previous_collect_module;
    mca_scoll_base_module_reduce_fn_t previous_reduce;
    mca_scoll_base_module_t *previous_reduce_module;
};
typedef struct mca_scoll_fca_module_t mca_scoll_fca_module_t;
OBJ_CLASS_DECLARATION(mca_scoll_fca_module_t);

/* API functions */
int mca_scoll_fca_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads);
mca_scoll_base_module_t *mca_scoll_fca_comm_query(struct oshmem_group_t *comm,
                                                  int *priority);
int mca_scoll_fca_get_fca_lib(struct oshmem_group_t *comm);

int mca_scoll_fca_barrier(struct oshmem_group_t *group,
                          long *pSync,
                          int algorithm_type);
int mca_scoll_fca_broadcast(struct oshmem_group_t *group,
                            int PE_root,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            int algorithm_type);
int mca_scoll_fca_collect(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nlong,
                          long *pSync,
                          bool nlong_type,
                          int algorithm_type);
int mca_scoll_fca_reduce(struct oshmem_group_t *group,
                         struct oshmem_op_t *op,
                         void *target,
                         const void *source,
                         size_t nlong,
                         long *pSync,
                         void *pWrk,
                         int algorithm_type);
OBJ_CLASS_DECLARATION(mca_coll_fca_module_t);
END_C_DECLS
#endif 
