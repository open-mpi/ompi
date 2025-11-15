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

#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_utils.h"
#include "ompi/mca/osc/ubcl/osc_ubcl_sync.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"

#define FAIL_IF_NOT_LOCKED(win, _op)                                           \
    do {                                                                       \
        if (!ompi_osc_ubcl_is_locked(win)) {                                   \
            mca_osc_ubcl_warn(OMPI_ERR_RMA_SYNC, "Attempt %s on a non locked window", _op); \
            return OMPI_ERR_RMA_SYNC;                                          \
        }                                                                      \
    } while (0)

static const char *osc_ubcl_sync_name(ubcl_win_sync_type_t type)
{
    switch (type) {
        case UBCL_WIN_SYNC_NONE:
            return "NO_SYNC";
        case UBCL_WIN_SYNC_LOCK:
            return "LOCK";
        case UBCL_WIN_SYNC_LOCK_ALL:
            return "LOCK_ALL";
        case UBCL_WIN_SYNC_PSCW:
            return "PSCW";
        case UBCL_WIN_SYNC_FENCE:
            return "FENCE";
        case UBCL_WIN_SYNC_FENCE_EPOCH:
            return "FENCE_WITH_COMMUNICATIONS";
        default:
            return "???";
    }
}

static bool ompi_osc_ubcl_is_locked(struct ompi_win_t *win)
{
    mca_osc_ubcl_module_t *module;

    module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    return module->passive_lock_refcount || UBCL_WIN_SYNC_LOCK == module->sync_type
           || UBCL_WIN_SYNC_LOCK_ALL == module->sync_type
           || UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK == module->sync_type;
}

int ompi_osc_ubcl_check_access_epoch(int target_rank, struct ompi_win_t *win)
{
    int ret;
    ubcl_win_sync_type_t rank_lock_type;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    int real_rank;

    ret = OMPI_SUCCESS;

    switch (module->sync_type) {
    case UBCL_WIN_SYNC_LOCK:
        /* Check if there is an access epoch for this target */
        rank_lock_type = module->procs_sync_type[target_rank];
        if (UBCL_WIN_SYNC_NONE == rank_lock_type) {
            ret = OMPI_ERR_RMA_SYNC;
            mca_osc_ubcl_warn(ret, "Invalid epoch: target %d is not locked on window %s",
                              target_rank, win->w_name);
        }
        break;
    case UBCL_WIN_SYNC_LOCK_ALL:
    case UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK:
        ret = OMPI_SUCCESS;
        break;
    case UBCL_WIN_SYNC_NONE:
        ret = OMPI_ERR_RMA_SYNC;
        mca_osc_ubcl_warn(ret, "Invalid epoch: no epoch started on window %s", win->w_name);
        break;
    case UBCL_WIN_SYNC_PSCW:
        /* Check if there is an access epoch for this target */
        if (NULL == module->active_sync_access_group) {
            ret = OMPI_ERR_RMA_SYNC;
            mca_osc_ubcl_warn(ret, "Invalid epoch: no access group defined for "
                              "window %s in an active target epoch", win->w_name);
        } else if (OMPI_SUCCESS != ompi_group_translate_ranks(win->w_group, 1, &target_rank,
                                                              module->active_sync_access_group,
                                                              &real_rank)) {
            ret = OMPI_ERR_RMA_SYNC;
            mca_osc_ubcl_warn(ret, "Invalid target %d for communications on window %s",
                              target_rank, win->w_name);
        }
        break;
    case UBCL_WIN_SYNC_FENCE:
    case UBCL_WIN_SYNC_FENCE_EPOCH:
        module->sync_type = UBCL_WIN_SYNC_FENCE_EPOCH;
        ret = OMPI_SUCCESS;
        break;
    default:
        ret = OMPI_ERR_NOT_IMPLEMENTED;
        break;
    }
    return ret;
}

/* ==== FLUSH ==== */

static int osc_ubcl_flush_no_check(int target, struct ompi_win_t *win)
{
    int ret;
    int ubcl_ret;
    mca_common_ubcl_endpoint_t *endpoint;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    ret = OMPI_SUCCESS;
    /* Get proc */
    ompi_proc_t *proc = ompi_group_peer_lookup_existing(win->w_group, target);
    if (OPAL_UNLIKELY(NULL == proc)) {
        ret = OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "Unknown rank %d in window %s", target, win->w_name);
        goto exit;
    }

    /* We retrieve endpoints created by the PML at init */
    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
    ubcl_ret = ubcl_flush(endpoint->rank, module->wid);
    if (UBCL_SUCCESS != ubcl_ret) {
        ret = ubcl_error_to_ompi(ubcl_ret);
        mca_osc_ubcl_warn(ret, "ubcl_flush returned error %d", ubcl_ret);
    }
exit:
    return ret;
}

int ompi_osc_ubcl_flush(int target, struct ompi_win_t *win)
{
    FAIL_IF_NOT_LOCKED(win, "flush");
    return osc_ubcl_flush_no_check(target, win);
}

static int osc_ubcl_flush_all_no_check(struct ompi_win_t *win)
{
    int size, ret;
    size = ompi_group_size(win->w_group);

    for (int i = 0; i < size; i++) {
        ret = osc_ubcl_flush_no_check(i, win);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }
    return OMPI_SUCCESS;
}

int ompi_osc_ubcl_flush_all(struct ompi_win_t *win)
{
    FAIL_IF_NOT_LOCKED(win, "flush_all");
    return osc_ubcl_flush_all_no_check(win);
}

int ompi_osc_ubcl_flush_local(int target, struct ompi_win_t *win)
{
    return ompi_osc_ubcl_flush(target, win);
}

int ompi_osc_ubcl_flush_local_all(struct ompi_win_t *win)
{
    return ompi_osc_ubcl_flush_all(win);
}


/* ==== LOCK ==== */

int ompi_osc_ubcl_lock(int lock_type, int target, int assert, struct ompi_win_t *win)
{
    ompi_proc_t *proc;
    mca_common_ubcl_endpoint_t *endpoint;
    ubcl_win_lock_type_t ubcl_type;
    int ret;
    ubcl_error_t ubcl_err;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    if (module->no_locks) {
        mca_osc_ubcl_error(OMPI_ERR_RMA_SYNC, "MPI_Win_lock : window %d is no_locks=true", module->wid);
    }

    OPAL_THREAD_LOCK(&module->sync_lock);

    /* check synchronization type */
    if (UBCL_WIN_SYNC_NONE != module->sync_type && UBCL_WIN_SYNC_LOCK != module->sync_type
            && UBCL_WIN_SYNC_FENCE != module->sync_type) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to lock window %s already in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    if (MPI_LOCK_EXCLUSIVE == lock_type) {
        ubcl_type = UBCL_WIN_LOCK_EXCLUSIVE;
    } else if (MPI_LOCK_SHARED == lock_type) {
        ubcl_type = UBCL_WIN_LOCK_SHARED;
    } else {
        ret = OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "MPI_Win_lock : lock type %d is unknown", lock_type);
        goto return_locked;
    }

    proc = ompi_group_peer_lookup_existing(win->w_group, target);
    if (OPAL_UNLIKELY(NULL == proc)) {
        ret =  OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "Cannot lock target %d on window %s", target, win->w_name);
        goto return_locked;
    }

    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];

    /* check access epoch */
    if (UBCL_WIN_SYNC_NONE != module->procs_sync_type[target]) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Target %d is already locked on window %s",
                          target, win->w_name);
        goto return_locked;
    }

    /* As no other process will attempt to acquire this lock while we have it,
     * we don't need to actually take it
     */
    if (0 != (MPI_MODE_NOCHECK & assert)) {
        module->procs_sync_type[target] = UBCL_WIN_SYNC_LOCK_NO_CHECK;
        ret = OMPI_SUCCESS;
        goto no_check;
    }

    ubcl_err = ubcl_win_lock(ubcl_type, endpoint->rank, module->wid);
    ret = ubcl_error_to_ompi(ubcl_err);
    if (OMPI_SUCCESS != ret) {
        /* Remote rank may be locked for ever: no recovery possible */
        mca_osc_ubcl_error(ret, "MPI_Win_lock failed");
        goto return_locked;
    }

    module->procs_sync_type[target] = UBCL_WIN_SYNC_LOCK;
no_check:
    module->sync_type = UBCL_WIN_SYNC_LOCK;
    opal_atomic_fetch_add_64(&module->passive_lock_refcount, 1);

return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);

    return ret;
}

int ompi_osc_ubcl_unlock(int target, struct ompi_win_t *win)
{
    ompi_proc_t *proc;
    mca_common_ubcl_endpoint_t *endpoint;
    int ret;
    ubcl_error_t ubcl_err;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    if (module->no_locks) {
        mca_osc_ubcl_error(OMPI_ERR_RMA_SYNC, "MPI_Win_unlock : window %d is no_locks=true", module->wid);
    }

    OPAL_THREAD_LOCK(&module->sync_lock);

    /* check synchronization type */
    if (UBCL_WIN_SYNC_LOCK != module->sync_type
        || (UBCL_WIN_SYNC_LOCK != module->procs_sync_type[target]
            && UBCL_WIN_SYNC_LOCK_NO_CHECK != module->procs_sync_type[target])) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Target %d is not locked so it cannot be unlocked "
                          "window %s (sync type %s)",
                          target, win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    /* Get proc */
    proc = ompi_group_peer_lookup_existing(win->w_group, target);
    if (OPAL_UNLIKELY(NULL == proc)) {
        ret = OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "Unknown rank %d on window %s", target, win->w_name);
        goto return_locked;
    }

    /* check exposure epoch */
    endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];

    ret = osc_ubcl_flush_no_check(target, win);
    if (OMPI_SUCCESS != ret) {
        goto return_locked;
    }

    /* We did not really take this lock, so no need to release it */
    if (UBCL_WIN_SYNC_LOCK_NO_CHECK == module->procs_sync_type[target]) {
        ret = OMPI_SUCCESS;
        goto no_check;
    }

    ubcl_err = ubcl_win_unlock(endpoint->rank, module->wid);
    ret = ubcl_error_to_ompi(ubcl_err);
    if (OMPI_SUCCESS != ret) {
        mca_osc_ubcl_warn(ret, "MPI_Win_unlock failed");
        goto return_locked;
    }

no_check:
    if (0 == opal_atomic_sub_fetch_64(&module->passive_lock_refcount, 1)) {
        module->sync_type = UBCL_WIN_SYNC_NONE;
    }
    module->procs_sync_type[target] = UBCL_WIN_SYNC_NONE;

return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

static int get_all_ubcl_ranks(struct ompi_win_t *win, ubcl_rank_t *all_ranks)
{
    int group_size;
    int ret = OMPI_SUCCESS;

    group_size = ompi_group_size(win->w_group);

    for (int i = 0; i < group_size; ++i) {
        ompi_proc_t *proc;
        mca_common_ubcl_endpoint_t *endpoint;

        proc = ompi_group_peer_lookup_existing(win->w_group, i);

        if (OPAL_UNLIKELY(NULL == proc)) {
            mca_osc_ubcl_warn(ret, "Unknown %d-th proc on window %s", i, win->w_name);
            return OMPI_ERR_BAD_PARAM;
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        all_ranks[i] = endpoint->rank;
    }
    return ret;
}

/* lock_all doesn't need to check the exposure epoch because if there was another
 * one started (individual lock or lock_all) then module->sync_type would be
 * different from UBCL_WIN_SYNC_NONE therefore returning OMPI_ERR_RMA_CONFLICT.
 * Stemming from this, unlock_all doesn't need to check the epoch either
 */
int ompi_osc_ubcl_lock_all(int assert, struct ompi_win_t *win)
{
    ubcl_rank_t *all_ranks;
    int group_size, ret;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    if (module->no_locks) {
        mca_osc_ubcl_error(OMPI_ERR_RMA_SYNC, "MPI_Win_lockall : window %d is no_locks=true", module->wid);
    }

    /* check access epoch */
    if (UBCL_WIN_SYNC_NONE != module->sync_type && UBCL_WIN_SYNC_FENCE != module->sync_type) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to lock_all window %s already in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        return ret;
    }

    if (0 != (MPI_MODE_NOCHECK & assert)) {
        module->sync_type = UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK;
        ret = OMPI_SUCCESS;
        goto no_check;
    }

    group_size = ompi_group_size(win->w_group);
    all_ranks = malloc((group_size) * sizeof(ubcl_rank_t));
    ret = get_all_ubcl_ranks(win, all_ranks);
    if (OMPI_SUCCESS != ret) {
        goto exit_malloced;
    }

    ret = ubcl_error_to_ompi(ubcl_win_lock_multiple(all_ranks, group_size, module->wid));
    if (OMPI_SUCCESS != ret) {
        /* Undefined state. Nothing can be retried safely */
        mca_osc_ubcl_error(ret, "MPI_Win_lock_all failed");
    }

    opal_atomic_fetch_add_64(&module->passive_lock_refcount, group_size);
    module->sync_type = UBCL_WIN_SYNC_LOCK_ALL;

exit_malloced:
    free(all_ranks);

no_check:
    return ret;
}

int ompi_osc_ubcl_unlock_all(struct ompi_win_t *win)
{
    ubcl_rank_t *all_ranks;
    int group_size, ret;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    if (module->no_locks) {
        mca_osc_ubcl_error(OMPI_ERR_RMA_SYNC, "MPI_Win_unlockall : window %d is no_locks=true", module->wid);
    }

    if (UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK == module->sync_type) {
        osc_ubcl_flush_all_no_check(win);
        module->sync_type = UBCL_WIN_SYNC_NONE;
        ret = UBCL_SUCCESS;
        goto no_check;
    }

    /* check access epoch */
    if (UBCL_WIN_SYNC_LOCK_ALL != module->sync_type) {
        return OMPI_ERR_RMA_CONFLICT;
    }

    group_size = ompi_group_size(win->w_group);
    all_ranks = malloc((group_size) * sizeof(ubcl_rank_t));
    ret = get_all_ubcl_ranks(win, all_ranks);
    if (OMPI_SUCCESS != ret) {
        goto exit_malloced;
    }

    osc_ubcl_flush_all_no_check(win);
    ret = ubcl_error_to_ompi(ubcl_win_unlock_multiple(all_ranks, group_size, module->wid));
    if (OMPI_SUCCESS != ret) {
        /* Undefined state. Nothing can be retried safely */
        mca_osc_ubcl_error(ret, "MPI_Win_unlock_all failed");
    }

    if (0 == opal_atomic_sub_fetch_64(&module->passive_lock_refcount, group_size)) {
        module->sync_type = UBCL_WIN_SYNC_NONE;
    }

exit_malloced:
    free(all_ranks);

no_check:
    return ret;
}


/* ==== Active target Post/Start/Complete/Wait ==== */

int ompi_osc_ubcl_start(struct ompi_group_t *group, int assert, struct ompi_win_t *win)
{
    /* We cannot take benefit from this assertion:
     *     MPI_MODE_NOCHECK: As we still need to synchro the end of the epoch,
     *                       we cannot bypass synchronization calls.
     */
    (void) assert;
    int ret;
    ompi_proc_t *proc;
    int group_size;
    mca_common_ubcl_endpoint_t *endpoint;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    OPAL_THREAD_LOCK(&module->sync_lock);

    /* We should be able to create an access and an exposure epoch simultaneously */
    if (NULL != module->active_sync_access_group
            || ( UBCL_WIN_SYNC_NONE != module->sync_type
                 && UBCL_WIN_SYNC_FENCE != module->sync_type
                 && UBCL_WIN_SYNC_PSCW != module->sync_type )) {
        ret = OMPI_ERR_RMA_SYNC;
        mca_osc_ubcl_warn(ret, "Failed to start window %s already in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    group_size = ompi_group_size(group);
    for (int i = 0; i < group_size; ++i) {
        ubcl_error_t ubcl_err;

        proc = ompi_group_peer_lookup_existing(group, i);
        if (OPAL_UNLIKELY(NULL == proc)) {
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(OMPI_ERROR, "Unknown %d-th rank asked to %s", i, __func__);
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        ubcl_err = ubcl_win_initiator_waits_lock(endpoint->rank, module->wid);
        if (UBCL_SUCCESS != ubcl_err) {
            ret = ubcl_error_to_ompi(ubcl_err);
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(ret,
                    "[win %s] Start failed waiting process %d to accept the lock",
                    win->w_name, i);
            goto return_locked;
        }
    }

    module->sync_type = UBCL_WIN_SYNC_PSCW;
    OBJ_RETAIN(group);
    module->active_sync_access_group = group;

    ret = OMPI_SUCCESS;
return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

int ompi_osc_ubcl_complete(struct ompi_win_t *win)
{
    ompi_proc_t *proc;
    int group_size;
    mca_common_ubcl_endpoint_t *endpoint;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    int ret;

    OPAL_THREAD_LOCK(&module->sync_lock);

    if (UBCL_WIN_SYNC_PSCW != module->sync_type) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to complete window %s in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    if (NULL == module->active_sync_access_group) {
        ret = OMPI_ERROR;
        mca_osc_ubcl_warn(ret, "[win %s] no access group for which to complete "
                          "active target synchronization", win->w_name);
        goto return_locked;
    }

    ret = osc_ubcl_flush_all_no_check(win);
    if (OMPI_SUCCESS != ret) {
        goto return_locked;
    }

    ubcl_error_t ubcl_err;
    /* Call ubcl_win_sync to clean some NIC counterproductive caches */
    ubcl_err = ubcl_win_sync(module->wid);
    if (UBCL_SUCCESS != ubcl_err) {
        mca_osc_ubcl_error(ubcl_error_to_ompi(ubcl_err),
                           "[win %s] Call to sync failed, this is not recoverable", win->w_name);
    }

    group_size = ompi_group_size(module->active_sync_access_group);
    for (int i = 0; i < group_size; ++i) {

        proc = ompi_group_peer_lookup_existing(module->active_sync_access_group, i);
        if (OPAL_UNLIKELY(NULL == proc)) {
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(OMPI_ERROR, "Unknown %d-th rank asked to %s", i, __func__);
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        ubcl_err = ubcl_win_initiator_releases_lock(endpoint->rank, module->wid);
        if (UBCL_SUCCESS != ubcl_err) {
            ret = ubcl_error_to_ompi(ubcl_err);
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(ret, "[win %s] Complete failed while releasing lock "
                               "for process %d", win->w_name, i);
            goto return_locked;
        }
    }

    /* We want to keep the window marked as in a pscw sync scheme if an exposure epoch exists */
    if (NULL == module->active_sync_exposure_group) {
        module->sync_type = UBCL_WIN_SYNC_NONE;
    }
    OBJ_RELEASE(module->active_sync_access_group);
    module->active_sync_access_group = NULL;

    ret = OMPI_SUCCESS;

return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

int ompi_osc_ubcl_post(struct ompi_group_t *group, int assert, struct ompi_win_t *win)
{
    /* We cannot take benefit from these assertions:
     *     MPI_MODE_NOCHECK: As we still need to synchro the end of the epoch,
     *                       we cannot bypass synchronization calls.
     *     MPI_MODE_NOSTORE: Window is in unified memory model, operations are not cached.
     *     MPI_MODE_NOPUT  : Window is in unified memory model, operations are not cached.
     */
    (void) assert;
    int ret;
    ompi_proc_t *proc;
    int group_size;
    mca_common_ubcl_endpoint_t *endpoint;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;
    ubcl_error_t ubcl_err;

    OPAL_THREAD_LOCK(&module->sync_lock);

    /* We should be able to create an access and an exposure epoch simultaneously */
    if (NULL != module->active_sync_exposure_group
            || ( UBCL_WIN_SYNC_NONE != module->sync_type
                 && UBCL_WIN_SYNC_FENCE != module->sync_type
                 && UBCL_WIN_SYNC_PSCW != module->sync_type )) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to post window %s already in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    /* Call ubcl_win_sync to clean some NIC counterproductive caches */
    ubcl_err = ubcl_win_sync(module->wid);
    if (UBCL_SUCCESS != ubcl_err) {
        mca_osc_ubcl_error(ubcl_error_to_ompi(ubcl_err),
                           "[win %s] Call to sync failed, this is not recoverable", win->w_name);
    }

    group_size = ompi_group_size(group);
    for (int i = 0; i < group_size; ++i) {

        proc = ompi_group_peer_lookup_existing(group, i);
        if (OPAL_UNLIKELY(NULL == proc)) {
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(OMPI_ERROR, "Unknown %d-th rank asked to %s", i, __func__);
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        ubcl_err = ubcl_win_target_grants_lock(endpoint->rank, module->wid);
        if (UBCL_SUCCESS != ubcl_err) {
            ret = ubcl_error_to_ompi(ubcl_err);
            /* Partial retries are not possible here so errors are fatal */
            mca_osc_ubcl_error(ret, "[win %s] Post failed while accepting the "
                               "lock from process %d", win->w_name, i);
            goto return_locked;
        }
    }
    module->sync_type = UBCL_WIN_SYNC_PSCW;
    OBJ_RETAIN(group);
    module->active_sync_exposure_group = group;
    module->nb_rank_waited = 0;

    ret = OMPI_SUCCESS;

return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

int ompi_osc_ubcl_wait(struct ompi_win_t *win)
{
    int ret;
    ret = ompi_osc_ubcl_test(win, NULL);
    return ret;
}

int ompi_osc_ubcl_test(struct ompi_win_t *win, int *flag)
{
    int ret;
    ompi_proc_t *proc;
    int group_size;
    mca_common_ubcl_endpoint_t *endpoint;
    int ubcl_flag = 0;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    OPAL_THREAD_LOCK(&module->sync_lock);
    if (NULL != flag) {
        (*flag) = 0;
    }

    if (UBCL_WIN_SYNC_PSCW != module->sync_type) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to test window %s in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        goto return_locked;
    }

    if (NULL == module->active_sync_exposure_group) {
        ret = OMPI_ERR_BAD_PARAM;
        mca_osc_ubcl_warn(ret, "[win %s] no group to wait lock release from",
                          win->w_name);
        goto return_locked;
    }

    group_size = ompi_group_size(module->active_sync_exposure_group);
    for (int i = module->nb_rank_waited; i < group_size; ++i) {
        ubcl_error_t ubcl_err;

        proc = ompi_group_peer_lookup_existing(module->active_sync_exposure_group, i);
        if (OPAL_UNLIKELY(NULL == proc)) {
            /* Undefined state. Nothing can be retried safely */
            mca_osc_ubcl_error(OMPI_ERROR, "Unknown %d-th rank asked to %s", i, __func__);
        }

        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];

        if (NULL != flag) {
            ubcl_err = ubcl_win_target_tests_lock_release(endpoint->rank, module->wid, &ubcl_flag);
        } else {
            ubcl_err = ubcl_win_target_waits_lock_release(endpoint->rank, module->wid);
            ubcl_flag = 1;
        }

        if (UBCL_SUCCESS != ubcl_err) {
            ret = ubcl_error_to_ompi(ubcl_err);
            mca_osc_ubcl_warn(ret, "[win %s] Test failed while waiting release "
                              "lock from the %d-th process", win->w_name, i);
            goto return_locked;
        }

        /* if we didn't pass the tests_lock_release then we return
         * for next test we'll start again at proc n° module->nb_rank_waited
         * if it was a wait, ubcl_flag was positionned to false so it is ignored */
        if (!ubcl_flag) {
            ret = OMPI_SUCCESS;
            goto return_locked;
        }
        module->nb_rank_waited++;
    }

    /* We want to keep the window marked as in a pscw sync scheme if an exposure epoch exists */
    if (NULL == module->active_sync_access_group) {
        module->sync_type = UBCL_WIN_SYNC_NONE;
    }

    OBJ_RELEASE(module->active_sync_exposure_group);
    module->active_sync_exposure_group = NULL;
    if (NULL != flag) {
        (*flag) = 1;
    }

    ret = OMPI_SUCCESS;
return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

/* ==== Fence ==== */

int ompi_osc_ubcl_fence(int assert, struct ompi_win_t *win)
{
    /* We cannot take benefit from these assertions:
     *     MPI_MODE_NOSTORE  : The window is in unified memory model, no operations are cached.
     *     MPI_MODE_NOPUT    : The window is in unified memory model, no operations are cached.
     */
    int ret = OMPI_SUCCESS;
    mca_osc_ubcl_module_t *module = (mca_osc_ubcl_module_t *) win->w_osc_module;

    if (UBCL_WIN_SYNC_FENCE != module->sync_type
        && UBCL_WIN_SYNC_FENCE_EPOCH != module->sync_type
        && UBCL_WIN_SYNC_NONE != module->sync_type) {
        ret = OMPI_ERR_RMA_CONFLICT;
        mca_osc_ubcl_warn(ret, "Failed to fence window %s in sync type %s",
                          win->w_name, osc_ubcl_sync_name(module->sync_type));
        return ret;
    }

    OPAL_THREAD_LOCK(&module->sync_lock);

    /* If the sync_type is not UBCL_WIN_SYNC_FENCE_EPOCH this should be almost a noop */
    if (0 == (MPI_MODE_NOPRECEDE & assert)) {
        ret = osc_ubcl_flush_all_no_check(win);
        if (OMPI_SUCCESS != ret) {
            goto return_locked;
        }
    }

    /* There is no easy way to detect when the barrier is optional.
     * The remote process may have started an epoch without us, we need to wait
     * its fence to avoid concurrent access. */
    if (0 == (MPI_MODE_NOPRECEDE & assert) || 0 == (MPI_MODE_NOSUCCEED & assert)) {
        ubcl_error_t ubcl_err;
        /* Call ubcl_win_sync to clean some NIC counterproductive caches */
        ubcl_err = ubcl_win_sync(module->wid);
        if (UBCL_SUCCESS != ubcl_err) {
            mca_osc_ubcl_error(ubcl_error_to_ompi(ubcl_err),
                    "[win %s] Call to sync failed, this is not recoverable", win->w_name);
        }

        ret = module->comm->c_coll->coll_barrier(module->comm,
                                                 module->comm->c_coll->coll_barrier_module);
    }

    module->sync_type = UBCL_WIN_SYNC_FENCE;

return_locked:
    OPAL_THREAD_UNLOCK(&module->sync_lock);
    return ret;
}

int ompi_osc_ubcl_sync(struct ompi_win_t *win)
{
    (void) win;
    return OMPI_SUCCESS;
}
