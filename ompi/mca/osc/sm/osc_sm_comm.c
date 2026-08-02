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

#include "osc_sm.h"

static inline opal_atomic_int64_t *
osc_sm_target_notify_base(ompi_osc_sm_module_t *module, int target)
{
    if (NULL == module->segment_base) {
        /* single-rank path: notify_counters is a regular local allocation */
        return module->notify_counters;
    }

    return (opal_atomic_int64_t *) ((char *) module->segment_base +
                                    module->node_states[target].notify_counter_offset);
}

/* MPI-5.1 section 12.6.1: "The notification counter referenced by a notified
 * communication operation must be attached to the window at the target before
 * the operation is initiated at the origin.  Initiating a notified
 * communication operation that references a notification counter that is out of
 * range at the target is erroneous."
 *
 * The check is therefore against the *target's* attached count, and it must
 * happen before any data is moved -- otherwise an erroneous call would still
 * have overwritten the target window (or, for get, the origin buffer) by the
 * time the error is reported. */
static inline int
osc_sm_check_notify_idx(ompi_osc_sm_module_t *module, int target, int notify)
{
    if (notify < 0 || (uint32_t) notify >= module->node_states[target].notify_counter_count) {
        return MPI_ERR_RMA_NOTIFICATION;
    }

    return OMPI_SUCCESS;
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

    /* Acquire ordering: sample the counter first, then fence, so that window
     * loads issued by the caller after this call cannot be satisfied by values
     * read before the notification was observed.  A barrier placed ahead of the
     * counter load would order nothing useful.  MPI-5.1 section 12.6.2 requires
     * this procedure to synchronize the public and private window copies as if
     * MPI_WIN_SYNC had been called; osc/sm is a unified-memory-model component,
     * so the barrier is the whole of that synchronization.
     *
     * The load itself is atomic because notify_counters is opal_atomic_int64_t:
     * remote origins bump the same location concurrently, and the standard's
     * usage model is to poll this procedure in a loop. */
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

    /* Atomically swap the counter to 0 and return the previous value.  Must be
     * a single atomic so that increments arriving from other MPI processes
     * between the read and the zeroing are not lost. */
    *value = (OMPI_MPI_COUNT_TYPE) opal_atomic_swap_64(
                 &osc_sm_target_notify_base(module, rank)[notify], 0);
    opal_atomic_rmb();

    return OMPI_SUCCESS;
}

int
ompi_osc_sm_win_set_num_notify(struct ompi_win_t *win,
                               struct opal_info_t *info,
                               int num_notifications)
{
    ompi_osc_sm_module_t *module = (ompi_osc_sm_module_t *) win->w_osc_module;
    int rank = ompi_comm_rank(module->comm);
    int ret;

    /* "mpi_assert_same_num_notifications" is an optimization hint only, and
     * osc/sm reserves the same fixed number of counters at every rank
     * regardless, so there is nothing to specialize on. */
    (void) info;

    if (num_notifications < 0) {
        return MPI_ERR_ARG;
    }

    /* A fixed region of OSC_SM_MAX_NOTIFY_COUNTERS counters per rank is carved
     * out of the shared segment at window creation, so that is the effective
     * MPI_WIN_NOTIFICATION_NUM_UB.  Asking for more cannot be satisfied without
     * re-creating the segment. */
    if (num_notifications > OSC_SM_MAX_NOTIFY_COUNTERS) {
        return MPI_ERR_ARG;
    }

    /* MPI-5.1 section 12.6.1: "A subsequent call to MPI_WIN_GET_NUM_NOTIFY will
     * return the value given to MPI_WIN_SET_NUM_NOTIFY."  The count is set to
     * exactly what was asked for -- note this differs from earlier drafts of
     * the chapter, which forbade decreasing it.
     *
     * "All notification counters (both existing and newly attached) are reset
     * to zero by this call."  Zero the whole reserved region rather than just
     * the attached prefix, so that counters left over from a previous, larger
     * attachment cannot resurface if the count is raised again.  It is
     * erroneous to call this while an access epoch is open, so no origin can be
     * incrementing our counters concurrently and plain stores are sufficient. */
    memset((void *) osc_sm_target_notify_base(module, rank), 0,
           OSC_SM_MAX_NOTIFY_COUNTERS * sizeof(int64_t));
    module->node_states[rank].notify_counter_count = (uint32_t) num_notifications;
    opal_atomic_wmb();

    /* "MPI_WIN_SET_NUM_NOTIFY is a blocking, synchronizing collective
     * procedure; it will not return until all MPI processes in the group of the
     * window have called the function and all processes have adjusted the
     * number of notification counters attached to the window."  Each rank
     * publishes its own count into the shared segment above, so the barrier is
     * all that is needed to make every rank's count visible to every other --
     * no counts have to be exchanged. */
    ret = module->comm->c_coll->coll_barrier(module->comm,
                                             module->comm->c_coll->coll_barrier_module);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

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

    /* MPI-5.1 section 12.6.1: local procedure returning the number of counters
     * attached at target_rank.  Every rank's count is published in the shared
     * segment by MPI_WIN_SET_NUM_NOTIFY, so this is a plain read. */
    *num_notifications = (int) module->node_states[target_rank].notify_counter_count;

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
     * notification is (MPI-5.1 section 12.3, "The notification counter will be
     * updated at the target only after the completion of the data movement
     * operation at the target"). */
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

    /* Full barrier, not opal_atomic_rmb(): the notification tells the target
     * that this get has read the window, so the loads above must not be
     * reordered after the counter increment below -- that is a load-before-store
     * constraint, which a load-load fence does not express.  opal_atomic_add()
     * is relaxed (see opal/include/opal/sys/atomic_stdc.h) and supplies no
     * ordering of its own. */
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

    /* Release ordering: the data must be visible at the target before the
     * notification is (MPI-5.1 section 12.3, "The notification counter will be
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
