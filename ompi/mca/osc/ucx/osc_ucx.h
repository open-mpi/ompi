/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_UCX_H
#define OMPI_OSC_UCX_H

#include <ucp/api/ucp.h>

#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/common/ucx/common_ucx.h"

#define OSC_UCX_ASSERT  MCA_COMMON_UCX_ASSERT
#define OSC_UCX_ERROR   MCA_COMMON_UCX_ERROR
#define OSC_UCX_VERBOSE MCA_COMMON_UCX_VERBOSE

#define OMPI_OSC_UCX_POST_PEER_MAX 32
#define OMPI_OSC_UCX_ATTACH_MAX    32
#define OMPI_OSC_UCX_RKEY_BUF_MAX  1024

typedef struct ompi_osc_ucx_win_info {
    ucp_rkey_h rkey;
    uint64_t addr;
    bool rkey_init;
} ompi_osc_ucx_win_info_t;

typedef struct ompi_osc_ucx_component {
    ompi_osc_base_component_t super;
    ucp_context_h ucp_context;
    ucp_worker_h ucp_worker;
    bool enable_mpi_threads;
    opal_free_list_t requests; /* request free list for the r* communication variants */
    bool env_initialized; /* UCX environment is initialized or not */
    int num_incomplete_req_ops;
    int num_modules;
    unsigned int priority;
} ompi_osc_ucx_component_t;

OMPI_DECLSPEC extern ompi_osc_ucx_component_t mca_osc_ucx_component;

typedef enum ompi_osc_ucx_epoch {
    NONE_EPOCH,
    FENCE_EPOCH,
    POST_WAIT_EPOCH,
    START_COMPLETE_EPOCH,
    PASSIVE_EPOCH,
    PASSIVE_ALL_EPOCH
} ompi_osc_ucx_epoch_t;

typedef struct ompi_osc_ucx_epoch_type {
    ompi_osc_ucx_epoch_t access;
    ompi_osc_ucx_epoch_t exposure;
} ompi_osc_ucx_epoch_type_t;

#define TARGET_LOCK_UNLOCKED  ((uint64_t)(0x0000000000000000ULL))
#define TARGET_LOCK_EXCLUSIVE ((uint64_t)(0x0000000100000000ULL))

#define OSC_UCX_IOVEC_MAX 128
#define OSC_UCX_OPS_THRESHOLD 1000000

#define OSC_UCX_STATE_LOCK_OFFSET 0
#define OSC_UCX_STATE_REQ_FLAG_OFFSET sizeof(uint64_t)
#define OSC_UCX_STATE_ACC_LOCK_OFFSET (sizeof(uint64_t) * 2)
#define OSC_UCX_STATE_COMPLETE_COUNT_OFFSET (sizeof(uint64_t) * 3)
#define OSC_UCX_STATE_POST_INDEX_OFFSET (sizeof(uint64_t) * 4)
#define OSC_UCX_STATE_POST_STATE_OFFSET (sizeof(uint64_t) * 5)
#define OSC_UCX_STATE_DYNAMIC_WIN_CNT_OFFSET (sizeof(uint64_t) * (5 + OMPI_OSC_UCX_POST_PEER_MAX))

typedef struct ompi_osc_dynamic_win_info {
    uint64_t base;
    size_t size;
    char rkey_buffer[OMPI_OSC_UCX_RKEY_BUF_MAX];
} ompi_osc_dynamic_win_info_t;

typedef struct ompi_osc_local_dynamic_win_info {
    ucp_mem_h memh;
    int refcnt;
} ompi_osc_local_dynamic_win_info_t;

typedef struct ompi_osc_ucx_state {
    volatile uint64_t lock;
    volatile uint64_t req_flag;
    volatile uint64_t acc_lock;
    volatile uint64_t complete_count; /* # msgs received from complete processes */
    volatile uint64_t post_index;
    volatile uint64_t post_state[OMPI_OSC_UCX_POST_PEER_MAX];
    volatile uint64_t dynamic_win_count;
    volatile ompi_osc_dynamic_win_info_t dynamic_wins[OMPI_OSC_UCX_ATTACH_MAX];
} ompi_osc_ucx_state_t;

typedef struct ompi_osc_ucx_module {
    ompi_osc_base_module_t super;
    struct ompi_communicator_t *comm;
    ucp_mem_h memh; /* remote accessible memory */
    int flavor;
    size_t size;
    ucp_mem_h state_memh;
    ompi_osc_ucx_win_info_t *win_info_array;
    ompi_osc_ucx_win_info_t *state_info_array;
    int disp_unit; /* if disp_unit >= 0, then everyone has the same
                    * disp unit size; if disp_unit == -1, then we
                    * need to look at disp_units */
    int *disp_units;

    ompi_osc_ucx_state_t state; /* remote accessible flags */
    ompi_osc_local_dynamic_win_info_t local_dynamic_win_info[OMPI_OSC_UCX_ATTACH_MAX];
    ompi_osc_ucx_epoch_type_t epoch_type;
    ompi_group_t *start_group;
    ompi_group_t *post_group;
    opal_hash_table_t outstanding_locks;
    opal_list_t pending_posts;
    int lock_count;
    int post_count;
    int global_ops_num;
    int *per_target_ops_nums;
    uint64_t req_result;
    int *start_grp_ranks;
    bool lock_all_is_nocheck;
} ompi_osc_ucx_module_t;

typedef enum locktype {
    LOCK_EXCLUSIVE,
    LOCK_SHARED
} lock_type_t;

typedef struct ompi_osc_ucx_lock {
    opal_object_t super;
    int target_rank;
    lock_type_t type;
    bool is_nocheck;
} ompi_osc_ucx_lock_t;

#define OSC_UCX_GET_EP(comm_, rank_) (ompi_comm_peer_lookup(comm_, rank_)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_UCX])
#define OSC_UCX_GET_DISP(module_, rank_) ((module_->disp_unit < 0) ? module_->disp_units[rank_] : module_->disp_unit)

int ompi_osc_ucx_win_attach(struct ompi_win_t *win, void *base, size_t len);
int ompi_osc_ucx_win_detach(struct ompi_win_t *win, const void *base);
int ompi_osc_ucx_free(struct ompi_win_t *win);

int ompi_osc_ucx_put(const void *origin_addr, int origin_count,
                     struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win);
int ompi_osc_ucx_get(void *origin_addr, int origin_count,
                     struct ompi_datatype_t *origin_dt,
                     int target, ptrdiff_t target_disp, int target_count,
                     struct ompi_datatype_t *target_dt, struct ompi_win_t *win);
int ompi_osc_ucx_accumulate(const void *origin_addr, int origin_count,
                            struct ompi_datatype_t *origin_dt,
                            int target, ptrdiff_t target_disp, int target_count,
                            struct ompi_datatype_t *target_dt,
                            struct ompi_op_t *op, struct ompi_win_t *win);
int ompi_osc_ucx_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                  void *result_addr, struct ompi_datatype_t *dt,
                                  int target, ptrdiff_t target_disp,
                                  struct ompi_win_t *win);
int ompi_osc_ucx_fetch_and_op(const void *origin_addr, void *result_addr,
                              struct ompi_datatype_t *dt, int target,
                              ptrdiff_t target_disp, struct ompi_op_t *op,
                              struct ompi_win_t *win);
int ompi_osc_ucx_get_accumulate(const void *origin_addr, int origin_count,
                                struct ompi_datatype_t *origin_datatype,
                                void *result_addr, int result_count,
                                struct ompi_datatype_t *result_datatype,
                                int target_rank, ptrdiff_t target_disp,
                                int target_count, struct ompi_datatype_t *target_datatype,
                                struct ompi_op_t *op, struct ompi_win_t *win);
int ompi_osc_ucx_rput(const void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win, struct ompi_request_t **request);
int ompi_osc_ucx_rget(void *origin_addr, int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target, ptrdiff_t target_disp, int target_count,
                      struct ompi_datatype_t *target_dt, struct ompi_win_t *win,
                      struct ompi_request_t **request);
int ompi_osc_ucx_raccumulate(const void *origin_addr, int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target, ptrdiff_t target_disp, int target_count,
                             struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                             struct ompi_win_t *win, struct ompi_request_t **request);
int ompi_osc_ucx_rget_accumulate(const void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_datatype,
                                 void *result_addr, int result_count,
                                 struct ompi_datatype_t *result_datatype,
                                 int target_rank, ptrdiff_t target_disp, int target_count,
                                 struct ompi_datatype_t *target_datatype,
                                 struct ompi_op_t *op, struct ompi_win_t *win,
                                 struct ompi_request_t **request);

int ompi_osc_ucx_fence(int assert, struct ompi_win_t *win);
int ompi_osc_ucx_start(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ucx_complete(struct ompi_win_t *win);
int ompi_osc_ucx_post(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ucx_wait(struct ompi_win_t *win);
int ompi_osc_ucx_test(struct ompi_win_t *win, int *flag);

int ompi_osc_ucx_lock(int lock_type, int target, int assert, struct ompi_win_t *win);
int ompi_osc_ucx_unlock(int target, struct ompi_win_t *win);
int ompi_osc_ucx_lock_all(int assert, struct ompi_win_t *win);
int ompi_osc_ucx_unlock_all(struct ompi_win_t *win);
int ompi_osc_ucx_sync(struct ompi_win_t *win);
int ompi_osc_ucx_flush(int target, struct ompi_win_t *win);
int ompi_osc_ucx_flush_all(struct ompi_win_t *win);
int ompi_osc_ucx_flush_local(int target, struct ompi_win_t *win);
int ompi_osc_ucx_flush_local_all(struct ompi_win_t *win);

int ompi_osc_find_attached_region_position(ompi_osc_dynamic_win_info_t *dynamic_wins,
                                           int min_index, int max_index,
                                           uint64_t base, size_t len, int *insert);

void req_completion(void *request, ucs_status_t status);
void internal_req_init(void *request);

#endif /* OMPI_OSC_UCX_H */
