/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/communicator/communicator.h"
#include "opal/align.h"
#include "opal/util/printf.h"
#include "opal/util/sys_limits.h"

#include <limits.h>
#include <stdint.h>

#include "osc_sm.h"

static inline opal_atomic_int64_t *
osc_sm_target_notify_base(ompi_osc_sm_module_t *module, int target)
{
    return module->notify_bases[target];
}

static inline int
osc_sm_check_notify_idx(ompi_osc_sm_module_t *module, int target, int notify)
{
    if (notify < 0 || (uint32_t) notify >= module->node_states[target].notify_counter_count) {
        return MPI_ERR_RMA_NOTIFICATION;
    }

    return OMPI_SUCCESS;
}

static inline void
osc_sm_notify_accumulate_done(ompi_osc_sm_module_t *module, int target, int notify)
{
    opal_atomic_mb();
    opal_atomic_add(&osc_sm_target_notify_base(module, target)[notify], 1);
}

int
ompi_osc_sm_win_get_notify_value(struct ompi_win_t *win,
                                 int notify,
                                 OMPI_MPI_COUNT_TYPE *value)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;
    int rank = ompi_comm_rank(module->comm);
    int ret;

    ret = osc_sm_check_notify_idx(module, rank, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    *value = (OMPI_MPI_COUNT_TYPE) osc_sm_target_notify_base(module, rank)[notify];
    opal_atomic_rmb();

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_win_reset_notify_value(struct ompi_win_t *win,
                                   int notify,
                                   OMPI_MPI_COUNT_TYPE *value)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;
    int rank = ompi_comm_rank(module->comm);
    int ret;

    ret = osc_sm_check_notify_idx(module, rank, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* Atomically swap the counter to 0 and return the previous value. */
    *value = (OMPI_MPI_COUNT_TYPE) opal_atomic_swap_64(
                 &osc_sm_target_notify_base(module, rank)[notify], 0);
    opal_atomic_rmb();

    return OMPI_SUCCESS;
}

/* Move every rank's notification counters into a newly created shared segment
 * sized for the capacities in new_caps.  Collective over the window's
 * communicator; every MPI process must call it with an identical new_caps.
 */
static int
osc_sm_grow_notify_counters(ompi_osc_sm_module_t *module, const unsigned long *new_caps,
                            unsigned long new_count)
{
    int comm_size = ompi_comm_size(module->comm);
    int rank = ompi_comm_rank(module->comm);
    opal_shmem_ds_t new_seg_ds;
    opal_shmem_ds_t old_seg_ds = module->notify_seg_ds;
    void *old_segment_base = module->notify_segment_base;
    void *new_base;
    unsigned long total_counters = 0;
    size_t seg_size;
    char *data_file;
    int ret, i, status;

    for (i = 0 ; i < comm_size ; ++i) {
        total_counters += new_caps[i];
    }
    seg_size = total_counters * sizeof(int64_t);
    seg_size += OPAL_ALIGN_PAD_AMOUNT(seg_size, opal_getpagesize());

    memset(&new_seg_ds, 0, sizeof(new_seg_ds));

    if (0 == rank) {
        ret = opal_asprintf(&data_file, "%s" OPAL_PATH_SEP "osc_sm_notify.%s.%x.%d.%s.%lu",
                            mca_osc_sm_component.backing_directory, ompi_process_info.nodename,
                            OMPI_PROC_MY_NAME->jobid, (int) OMPI_PROC_MY_NAME->vpid,
                            ompi_comm_print_cid(module->comm), total_counters);
        if (ret > 0) {
            (void) opal_shmem_segment_create(&new_seg_ds, data_file, seg_size);
            free(data_file);
        }
    }

    ret = module->comm->c_coll->coll_bcast(&new_seg_ds, sizeof(new_seg_ds), MPI_BYTE, 0,
                                           module->comm,
                                           module->comm->c_coll->coll_bcast_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if ('\0' == new_seg_ds.seg_name[0]) {
        return MPI_ERR_NO_MEM;
    }

    new_base = opal_shmem_segment_attach(&new_seg_ds);

    /* Attach can fail at some MPI processes and not others.  Agree on the
     * outcome before touching any shared state */
    status = (NULL == new_base) ? 1 : 0;
    ret = module->comm->c_coll->coll_allreduce(MPI_IN_PLACE, &status, 1, MPI_INT, MPI_MAX,
                                               module->comm,
                                               module->comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if (0 != status) {
        if (NULL != new_base) {
            opal_shmem_segment_detach(&new_seg_ds);
        }
        return MPI_ERR_NO_MEM;
    }

    module->notify_segment_base = new_base;
    module->notify_seg_ds = new_seg_ds;

    /* Republish the layout.  Every MPI process computes the same offsets from
     * the same new_caps, so these stores are identical everywhere; node_states
     * lives in the main segment, which does not move. */
    total_counters = 0;
    for (i = 0 ; i < comm_size ; ++i) {
        module->node_states[i].notify_counter_capacity = (uint32_t) new_caps[i];
        module->node_states[i].notify_counter_offset = total_counters * sizeof(int64_t);
        total_counters += new_caps[i];
    }

    ompi_osc_sm_refresh_notify_bases(module);

    memset((void *) module->notify_bases[rank], 0,
           module->node_states[rank].notify_counter_capacity * sizeof(int64_t));

    module->node_states[rank].notify_counter_count = (uint32_t) new_count;
    opal_atomic_wmb();

    /* Everyone has opened the new segment and published their layout.  Both
     * facts are needed before we continue: the unlink below removes the name
     * that the attach above resolves, and an origin returning from this call
     * may immediately validate a notification index against our count. */
    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    if (0 == rank) {
        opal_shmem_unlink(&module->notify_seg_ds);
    }

    /* Dropping the old mapping is purely local -- munmap in one MPI process
     * does not disturb any other process's view -- so it needs no
     * synchronization of its own.  On the first growth there is nothing to
     * drop: the counters were inline in the main segment, which stays mapped
     * for the lifetime of the window. */
    if (NULL != old_segment_base) {
        opal_shmem_segment_detach(&old_seg_ds);
    }

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_win_set_num_notify(struct ompi_win_t *win,
                               struct opal_info_t *info,
                               int num_notifications)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;
    int comm_size = ompi_comm_size(module->comm);
    int rank = ompi_comm_rank(module->comm);
    unsigned long requested = (unsigned long) num_notifications;
    unsigned long *new_caps;
    bool grow = false;
    int ret, i;

    /* "mpi_assert_same_num_notifications" would let us skip the allgather below
     * and derive the layout from our own num_notifications.  Not taken up yet;
     * the allgather is one collective on a procedure that is already
     * synchronizing and collective. */
    (void) info;

    if (num_notifications < 0) {
        return MPI_ERR_ARG;
    }

    if (0 != module->notify_max_assert &&
        requested > (unsigned long) module->notify_max_assert) {
        return MPI_ERR_ARG;
    }

    memset((void *) module->notify_bases[rank], 0,
           module->node_states[rank].notify_counter_capacity * sizeof(int64_t));
    module->node_states[rank].notify_counter_count =
        (requested > module->node_states[rank].notify_counter_capacity)
            ? module->node_states[rank].notify_counter_capacity
            : (uint32_t) requested;
    opal_atomic_wmb();

    if (1 == comm_size) {
        /* No shared segment for a single-process window; the counters are a
         * plain allocation, so growing them is a plain reallocation and none of
         * the collective machinery below applies. */
        if (requested > module->node_states[0].notify_counter_capacity) {
            void *grown = calloc(requested, sizeof(int64_t));
            if (NULL == grown) {
                return MPI_ERR_NO_MEM;
            }
            free((void *) module->notify_bases[0]);
            module->notify_bases[0] = (opal_atomic_int64_t *) grown;
            module->node_states[0].notify_counter_capacity = (uint32_t) requested;
            module->node_states[0].notify_counter_count = (uint32_t) requested;
        }
        return OMPI_SUCCESS;
    }

    new_caps = malloc(sizeof(*new_caps) * comm_size);
    if (NULL == new_caps) {
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    ret = module->comm->c_coll->coll_allgather(&requested, 1, MPI_UNSIGNED_LONG,
                                               new_caps, 1, MPI_UNSIGNED_LONG,
                                               module->comm,
                                               module->comm->c_coll->coll_allgather_module);
    if (OMPI_SUCCESS != ret) {
        free(new_caps);
        return ret;
    }

    for (i = 0 ; i < comm_size ; ++i) {
        if (new_caps[i] > module->node_states[i].notify_counter_capacity) {
            grow = true;
        } else {
            /* Never shrink: a rank that lowered its count keeps the space it
             * already has, so that only genuine growth costs a reallocation. */
            new_caps[i] = module->node_states[i].notify_counter_capacity;
        }
    }

    if (grow) {
        ret = osc_sm_grow_notify_counters(module, new_caps, requested);
        free(new_caps);
        return ret;
    }

    free(new_caps);

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_win_get_num_notify(struct ompi_win_t *win,
                               int target_rank,
                               int *num_notifications)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;

    if (target_rank < 0 || target_rank >= ompi_comm_size(module->comm)) {
        return MPI_ERR_RANK;
    }

    *num_notifications = (int) module->node_states[target_rank].notify_counter_count;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_win_get_notify_bounds(struct ompi_win_t *win,
                                  int *num_sb,
                                  int *num_ub,
                                  OMPI_MPI_COUNT_TYPE *value_ub)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;

    if (0 != module->notify_max_assert) {
        *num_sb = (int) module->notify_max_assert;
        *num_ub = (int) module->notify_max_assert;
    } else {
        *num_sb = (int) mca_osc_sm_component.num_notify_counters;
        *num_ub = INT_MAX;
    }

    /* Counters are int64_t and only ever incremented by one per notified
     * operation, so the representable maximum is the real bound. */
    *value_ub = (OMPI_MPI_COUNT_TYPE) INT64_MAX;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_rput(const void *origin_addr,
                 size_t origin_count,
                 struct ompi_datatype_t *origin_dt,
                 int target,
                 ptrdiff_t target_disp,
                 size_t target_count,
                 struct ompi_datatype_t *target_dt,
                 struct ompi_win_t *win,
                 struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rput: 0x%lx, %zu, %s, %d, %d, %zu, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                               remote_address, target_count, target_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_rput_notify(const void *origin_addr,
                 size_t origin_count,
                 struct ompi_datatype_t *origin_dt,
                 int target,
                 ptrdiff_t target_disp,
                 size_t target_count,
                 struct ompi_datatype_t *target_dt,
                 int notify,
                 struct ompi_win_t *win,
                 struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rput_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                               remote_address, target_count, target_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* Release ordering: the data must be visible at the target before the
     * notification is */
    opal_atomic_wmb();
    opal_atomic_add(&osc_sm_target_notify_base(module, target)[notify], 1);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_rget(void *origin_addr,
                 size_t origin_count,
                 struct ompi_datatype_t *origin_dt,
                 int target,
                 ptrdiff_t target_disp,
                 size_t target_count,
                 struct ompi_datatype_t *target_dt,
                 struct ompi_win_t *win,
                 struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget: 0x%lx, %zu, %s, %d, %d, %zu, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               origin_addr, origin_count, origin_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_rget_notify(void *origin_addr,
                 size_t origin_count,
                 struct ompi_datatype_t *origin_dt,
                 int target,
                 ptrdiff_t target_disp,
                 size_t target_count,
                 struct ompi_datatype_t *target_dt,
                 int notify,
                 struct ompi_win_t *win,
                 struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               origin_addr, origin_count, origin_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    opal_atomic_mb();
    opal_atomic_add(&osc_sm_target_notify_base(module, target)[notify], 1);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_raccumulate(const void *origin_addr,
                        size_t origin_count,
                        struct ompi_datatype_t *origin_dt,
                        int target,
                        ptrdiff_t target_disp,
                        size_t target_count,
                        struct ompi_datatype_t *target_dt,
                        struct ompi_op_t *op,
                        struct ompi_win_t *win,
                        struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "raccumulate: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);
    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                    remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return ret;
}


int
ompi_osc_sm_raccumulate_notify(const void *origin_addr,
                               size_t origin_count,
                               struct ompi_datatype_t *origin_dt,
                               int target,
                               ptrdiff_t target_disp,
                               size_t target_count,
                               struct ompi_datatype_t *target_dt,
                               struct ompi_op_t *op,
                               int notify,
                               struct ompi_win_t *win,
                               struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "raccumulate_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name, notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);
    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                    remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* Only notify once the accumulate actually happened -- a counter bumped for
     * an operation that failed would tell the target that data it never
     * received is ready. */
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    osc_sm_notify_accumulate_done(module, target, notify);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return ret;
}


int
ompi_osc_sm_rget_accumulate(const void *origin_addr,
                                  size_t origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  void *result_addr,
                                  size_t result_count,
                                  struct ompi_datatype_t *result_dt,
                                  int target,
                                  MPI_Aint target_disp,
                                  size_t target_count,
                                  struct ompi_datatype_t *target_dt,
                                  struct ompi_op_t *op,
                                  struct ompi_win_t *win,
                                  struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_accumulate: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               result_addr, result_count, result_dt);
    if (OMPI_SUCCESS != ret || op == &ompi_mpi_op_no_op.op) goto done;

    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                   remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }

 done:
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return ret;
}


int
ompi_osc_sm_rget_accumulate_notify(const void *origin_addr,
                                   size_t origin_count,
                                   struct ompi_datatype_t *origin_dt,
                                   void *result_addr,
                                   size_t result_count,
                                   struct ompi_datatype_t *result_dt,
                                   int target,
                                   MPI_Aint target_disp,
                                   size_t target_count,
                                   struct ompi_datatype_t *target_dt,
                                   struct ompi_op_t *op,
                                   int notify,
                                   struct ompi_win_t *win,
                                   struct ompi_request_t **ompi_req)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_accumulate_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name, notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               result_addr, result_count, result_dt);
    if (OMPI_SUCCESS != ret || op == &ompi_mpi_op_no_op.op) goto done;

    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                   remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }

 done:
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* Only notify once the operation actually happened.  MPI_NO_OP is not a
     * failure: the target window was still read into the result buffer, which is
     * an access the notification is required to cover. */
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    osc_sm_notify_accumulate_done(module, target, notify);

    /* the only valid field of RMA request status is the MPI_ERROR field.
     * ompi_request_empty has status MPI_SUCCESS and indicates the request is
     * complete. */
    *ompi_req = &ompi_request_empty;

    return ret;
}


int
ompi_osc_sm_put(const void *origin_addr,
                      size_t origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      size_t target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "put: 0x%lx, %zu, %s, %d, %d, %zu, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                               remote_address, target_count, target_dt);

    return ret;
}


int
ompi_osc_sm_put_notify(const void *origin_addr,
                size_t origin_count,
                struct ompi_datatype_t *origin_dt,
                int target,
                ptrdiff_t target_disp,
                size_t target_count,
                struct ompi_datatype_t *target_dt,
                int notify,
                struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                            "put_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %d, 0x%lx",
                            (unsigned long) origin_addr, origin_count,
                            origin_dt->name, target, (int) target_disp,
                            target_count, target_dt->name,
                            notify,
                            (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                remote_address, target_count, target_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* (MPI-5.1 section 12.3, "The notification counter will be
     * updated at the target only after the completion of the data movement
     * operation at the target"). */
    opal_atomic_wmb();
    opal_atomic_add(&osc_sm_target_notify_base(module, target)[notify], 1);

    return ret;
}

int
ompi_osc_sm_get(void *origin_addr,
                      size_t origin_count,
                      struct ompi_datatype_t *origin_dt,
                      int target,
                      ptrdiff_t target_disp,
                      size_t target_count,
                      struct ompi_datatype_t *target_dt,
                      struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %zu, %s, %d, %d, %zu, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               origin_addr, origin_count, origin_dt);

    return ret;
}


int
ompi_osc_sm_get_notify(void *origin_addr,
                size_t origin_count,
                struct ompi_datatype_t *origin_dt,
                int target,
                ptrdiff_t target_disp,
                size_t target_count,
                struct ompi_datatype_t *target_dt,
                int notify,
                struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %zu, %s, %d, %d, %zu, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               origin_addr, origin_count, origin_dt);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* Full barrier, not opal_atomic_rmb(): see ompi_osc_sm_rget_notify(). */
    opal_atomic_mb();
    opal_atomic_add(&osc_sm_target_notify_base(module, target)[notify], 1);

    return ret;
}


int
ompi_osc_sm_accumulate(const void *origin_addr,
                       size_t origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target,
                       ptrdiff_t target_disp,
                       size_t target_count,
                       struct ompi_datatype_t *target_dt,
                       struct ompi_op_t *op,
                       struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "accumulate: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);
    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                    remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    return ret;
}


int
ompi_osc_sm_accumulate_notify(const void *origin_addr,
                              size_t origin_count,
                              struct ompi_datatype_t *origin_dt,
                              int target,
                              ptrdiff_t target_disp,
                              size_t target_count,
                              struct ompi_datatype_t *target_dt,
                              struct ompi_op_t *op,
                              int notify,
                              struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "accumulate_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name, notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);
    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                    remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* Only notify once the accumulate actually happened -- a counter bumped for
     * an operation that failed would tell the target that data it never
     * received is ready. */
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    osc_sm_notify_accumulate_done(module, target, notify);

    return ret;
}


int
ompi_osc_sm_get_accumulate(const void *origin_addr,
                           size_t origin_count,
                           struct ompi_datatype_t *origin_dt,
                           void *result_addr,
                           size_t result_count,
                           struct ompi_datatype_t *result_dt,
                           int target,
                           MPI_Aint target_disp,
                           size_t target_count,
                           struct ompi_datatype_t *target_dt,
                           struct ompi_op_t *op,
                           struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get_accumulate: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               result_addr, result_count, result_dt);
    if (OMPI_SUCCESS != ret || op == &ompi_mpi_op_no_op.op) goto done;

    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                   remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }

 done:
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    return ret;
}


int
ompi_osc_sm_get_accumulate_notify(const void *origin_addr,
                                  size_t origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  void *result_addr,
                                  size_t result_count,
                                  struct ompi_datatype_t *result_dt,
                                  int target,
                                  MPI_Aint target_disp,
                                  size_t target_count,
                                  struct ompi_datatype_t *target_dt,
                                  struct ompi_op_t *op,
                                  int notify,
                                  struct ompi_win_t *win)
{
    int ret;
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get_accumulate_notify: 0x%lx, %zu, %s, %d, %d, %zu, %s, %s, %d, 0x%lx",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name,
                         op->o_name, notify,
                         (unsigned long) win));

    ret = osc_sm_check_notify_idx(module, target, notify);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    ret = ompi_datatype_sndrcv(remote_address, target_count, target_dt,
                               result_addr, result_count, result_dt);
    if (OMPI_SUCCESS != ret || op == &ompi_mpi_op_no_op.op) goto done;

    if (op == &ompi_mpi_op_replace.op) {
        ret = ompi_datatype_sndrcv((void *)origin_addr, origin_count, origin_dt,
                                   remote_address, target_count, target_dt);
    } else {
        ret = ompi_osc_base_sndrcv_op(origin_addr, origin_count, origin_dt,
                                      remote_address, target_count, target_dt,
                                      op);
    }

 done:
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    /* Only notify once the operation actually happened.  MPI_NO_OP is not a
     * failure: the target window was still read into the result buffer, which is
     * an access the notification is required to cover. */
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    osc_sm_notify_accumulate_done(module, target, notify);

    return ret;
}


int
ompi_osc_sm_compare_and_swap(const void *origin_addr,
                             const void *compare_addr,
                             void *result_addr,
                             struct ompi_datatype_t *dt,
                             int target,
                             ptrdiff_t target_disp,
                             struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;
    size_t size;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "compare_and_swap: 0x%lx, %s, %d, %d, 0x%lx",
                         (unsigned long) origin_addr,
                         dt->name, target, (int) target_disp,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    ompi_datatype_type_size(dt, &size);

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    /* fetch */
    ompi_datatype_copy_content_same_ddt(dt, 1, (char*) result_addr, (char*) remote_address);
    /* compare */
    if (0 == memcmp(result_addr, compare_addr, size)) {
        /* set */
        ompi_datatype_copy_content_same_ddt(dt, 1, (char*) remote_address, (char*) origin_addr);
    }

    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_sm_fetch_and_op(const void *origin_addr,
                         void *result_addr,
                         struct ompi_datatype_t *dt,
                         int target,
                         ptrdiff_t target_disp,
                         struct ompi_op_t *op,
                         struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;
    void *remote_address;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "fetch_and_op: 0x%lx, %s, %d, %d, %s, 0x%lx",
                         (unsigned long) origin_addr,
                         dt->name, target, (int) target_disp,
                         op->o_name,
                         (unsigned long) win));

    remote_address = ((char*) (module->bases[target])) + module->disp_units[target] * target_disp;

    opal_atomic_lock(&module->node_states[target].accumulate_lock);

    /* fetch */
    ompi_datatype_copy_content_same_ddt(dt, 1, (char*) result_addr, (char*) remote_address);
    if (op == &ompi_mpi_op_no_op.op) goto done;

    /* op */
    if (op == &ompi_mpi_op_replace.op) {
        ompi_datatype_copy_content_same_ddt(dt, 1, (char*) remote_address, (char*) origin_addr);
    } else {
        ompi_op_reduce(op, (void *)origin_addr, remote_address, 1, dt);
    }

 done:
    opal_atomic_unlock(&module->node_states[target].accumulate_lock);

    return OMPI_SUCCESS;
}
