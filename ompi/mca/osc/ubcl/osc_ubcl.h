/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Bull eXtreme Interconnect OSC API implementation.
 *
 * Implementation of API defined in osc.h. To see parameters and return values
 * of these functions, refer to ompi/mca/osc/osc.h.
 */

#ifndef MCA_OSC_UBCL_H
#define MCA_OSC_UBCL_H

#include <ubcl_api.h>
#include "ompi/mca/osc/osc.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_sync.h"
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/util/show_help.h"
#include "opal/mca/threads/mutex.h"

#define OSC_UBCL_IOVEC_MAX 128

struct mca_osc_ubcl_module_s {
    ompi_osc_base_module_t super;
    struct ompi_communicator_t *comm;
    struct ompi_win_t *win;
    int64_t wid;
    union {int *all; int uniq;} disp_unit;
    ubcl_win_flags_t win_flags;

    /* To avoid info access (including locking a list and string manipulations)
     * usefull info keys are stored inside the osc module.
     *
     * Note that string data such as accumulate_ordering and accumulate_ops
     * are already stored in dedicated window variables (w_acc_order and w_acc_ops)
     */
    uint32_t same_disp_unit:1;
    uint32_t no_locks:1;
    uint32_t padding_infos:30;

    /* Sync type of the entire window */
    ubcl_win_sync_type_t sync_type;
    /* Detail of locked peer, only relevant for win_[un]lock */
    ubcl_win_sync_type_t *procs_sync_type;
    /* How many remote locks are currently hold */
    int64_t passive_lock_refcount;
    /* Threadsafety for lock syncs
     * other types of sync should never be called by concurrent threads */
    opal_mutex_t sync_lock;

    /* Active target management */
    unsigned int nb_rank_waited;
    struct ompi_group_t *active_sync_access_group;
    struct ompi_group_t *active_sync_exposure_group;

    /* if non-null, this pointer should be free()ed with the window */
    void *free_after;
};
typedef struct mca_osc_ubcl_module_s mca_osc_ubcl_module_t;

struct mca_osc_ubcl_component_s {
    ompi_osc_base_component_t super;

    /** Functionnal fields **/
    volatile int64_t is_init;     /**< Whether we have been initialized, for proper close */
    int output;                   /**< Output stream */

    /** MCA parameters **/
    int priority;             /**< Priority of the component */
    int verbose;              /**< Verbosity level of the component */

    /** UBCL endpoint type capabilities **/
    opal_free_list_t req_free_list;
    unsigned int max_req;     /**< Maximum number of requests */
    unsigned int min_req;     /**< Minimum (and inititial) number of requests */
    unsigned int incr_req;    /**< Increasing (and inititial) number of requests */
    unsigned int pad_req;
};
typedef struct mca_osc_ubcl_component_s mca_osc_ubcl_component_t;
extern mca_osc_ubcl_component_t mca_osc_ubcl_component;

/* One Sided operations */
int ompi_osc_ubcl_put(const void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win);

int ompi_osc_ubcl_rput(const void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req);

int ompi_osc_ubcl_get(void *origin_addr,
                      int origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      int target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win);

int ompi_osc_ubcl_rget(void *origin_addr,
                       int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       int target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win,
                       struct ompi_request_t **ompi_req);

int ompi_osc_ubcl_accumulate(const void *origin_addr, int origin_count,
                             struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                             int target_count, struct ompi_datatype_t *target_dt,
                             struct ompi_op_t *op, struct ompi_win_t *win);

int ompi_osc_ubcl_raccumulate(const void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_dt, int target, ptrdiff_t target_disp,
                              int target_count, struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op, struct ompi_win_t *win,
                              struct ompi_request_t **ompi_req);

int ompi_osc_ubcl_get_accumulate(const void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_dt, void *result_addr,
                                 int result_count, struct ompi_datatype_t *result_dt,
                                 int target_rank, ptrdiff_t target_disp, int target_count,
                                 struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                                 struct ompi_win_t *win);

int ompi_osc_ubcl_rget_accumulate(const void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_dt, void *result_addr,
                                  int result_count, struct ompi_datatype_t *result_dt,
                                  int target_rank, ptrdiff_t target_disp, int target_count,
                                  struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                                  struct ompi_win_t *win, struct ompi_request_t **ompi_req);

int ompi_osc_ubcl_fetch_and_op(const void *origin_addr, void *result_addr,
                               struct ompi_datatype_t *dt, int target, ptrdiff_t target_disp,
                               struct ompi_op_t *op, struct ompi_win_t *win);

int ompi_osc_ubcl_compare_and_swap(const void *origin_addr, const void *compare_addr,
                                   void *result_addr, struct ompi_datatype_t *dt, int target,
                                   ptrdiff_t target_disp, struct ompi_win_t *win);

/* Sync functions */
int ompi_osc_ubcl_flush(int target,
                        struct ompi_win_t *win);
int ompi_osc_ubcl_flush_all(struct ompi_win_t *win);
int ompi_osc_ubcl_flush_local(int target,
                              struct ompi_win_t *win);
int ompi_osc_ubcl_flush_local_all(struct ompi_win_t *win);

/* ubcl custom memory descriptor management */
size_t osc_ubcl_datatype_pack(void *pack_buf, const void *usr_handle,
                              size_t pack_size, size_t offset);
size_t osc_ubcl_datatype_unpack(void *usr_handle, const void *pack_buf,
                                size_t pack_size, size_t offset);
size_t osc_ubcl_datatype_mem_size(const void *usr_handle, size_t offset);
void osc_ubcl_datatype_finish(void *usr_handle);

/* Misc */
int osc_ubcl_build_ddt_iov(const void *addr, ompi_proc_t *proc, int count,
                           ompi_datatype_t *datatype, struct iovec **output_iov,
                           size_t *output_iov_count);

#endif //MCA_OSC_UBCL_H
