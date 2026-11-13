/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/ompi_modex.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/proc/proc.h"
#include "ompi/constants.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/pml/base/base.h"

#include "opal/mca/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/proc.h"

static volatile bool ompi_modex_fence_active = false;
static volatile bool ompi_modex_all_ready_flag = false;
static pmix_status_t ompi_modex_fence_status = PMIX_SUCCESS;
static bool ompi_modex_direct = false;
/* Not reset by finalize: this is how the job is configured, not something
 * an instance learned. */
static bool ompi_modex_require_all_flag = false;

/* Whether everything this peer published is local, so a Get for one of its
 * keys can be answered on its merits. A NULL proc asks about the whole
 * job, which only a fence can answer; per-peer state lives on the proc. */
static bool ompi_modex_proc_available(ompi_proc_t *proc)
{
    if (NULL == proc) {
        return ompi_modex_all_ready_flag;
    }
    if (opal_proc_known(&proc->super, OPAL_PROC_FLAG_AVAILABLE)) {
        return true;
    }
    if (!ompi_modex_all_ready_flag) {
        return false;
    }
    /* The fence answered for the whole job, but a proc built before it
     * landed still says otherwise, so update it here, lazily. Doing it for
     * every proc from the fence callback would have the PMIx thread walk
     * the proc list under a lock an MPI thread holds across Gets. */
    opal_proc_learned(&proc->super, OPAL_PROC_FLAG_AVAILABLE);
    return true;
}

static void ompi_modex_fence_cb(pmix_status_t status, void *cbdata)
{
    (void) cbdata;
    OPAL_ACQUIRE_OBJECT(&ompi_modex_fence_active);
    ompi_modex_fence_status = status;
    /* A fence that failed exchanged nothing, so the flag must stay clear:
     * readers take it as "this peer's keys are all local", and a key
     * missing from an unfetched blob then reads as one the peer never
     * published -- ending in a live peer declared unreachable. */
    if (PMIX_SUCCESS == status) {
        ompi_modex_all_ready_flag = true;
    }
    ompi_modex_fence_active = false;
    OPAL_POST_OBJECT(&ompi_modex_fence_active);

    /* A failed fence is still the last answer there will be, so release
     * the waiters either way. */
    ompi_modex_epoch_advance();
}

static void ompi_modex_get_cb(pmix_status_t status, pmix_value_t *kv, void *cbdata)
{
    ompi_proc_t *proc = (ompi_proc_t *) cbdata;

    /* The value is ours to free, as with the blocking Get. Its contents
     * are unused: only the arrival of an answer matters. */
    if (NULL != kv) {
        PMIX_VALUE_RELEASE(kv);
    }

    /* A timeout means the peer has not published its modex yet. Re-ask on
     * the next progress iteration -- a fresh ask, because the timeout is
     * terminal at the server: it drops the request, so no later commit
     * will reach us.
     *
     * Never give up. Marking the peer AVAILABLE lets a btl read a key we
     * never fetched as a key the peer never published, and btl/sm rules
     * out the whole node on that. A dead peer needs no bound: it arrives
     * as one of the final statuses below, once the daemon fails what is
     * parked on it. */
    if (PMIX_ERR_TIMEOUT == status) {
        /* Clearing the claim lets a later ask re-issue; bumping the epoch
         * is what makes somebody ask. The Get is left to that next pass
         * to keep re-asking out of the callback, and if nobody asks then
         * nobody wants this peer. The release drops this fetch's
         * reference, not the proc -- the proc table holds its own. */
        opal_proc_forget(&proc->super, OPAL_PROC_FLAG_FETCHING);
        ompi_modex_epoch_advance();
        OBJ_RELEASE(proc);
        return;
    }

    /* Available even if the fetch failed: a key missing from data never
     * fetched is read as never published, the only reading left. Both
     * flags in one write, so nobody may see the keys as
     * published-or-absent without also seeing they were never fetched. */
    if (PMIX_SUCCESS != status) {
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,
                             "%s ompi_modex: fetch for %s failed (%s); its keys will now be "
                             "read as published or absent rather than as pending",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                             OPAL_NAME_PRINT(proc->super.proc_name),
                             PMIx_Error_string(status)));
        opal_proc_learned(&proc->super,
                          OPAL_PROC_FLAG_AVAILABLE | OPAL_PROC_FLAG_FETCH_FAILED);
    } else {
        opal_proc_learned(&proc->super, OPAL_PROC_FLAG_AVAILABLE);
    }

    /* After the flags, and on either status: a failure is as final as a
     * success, so a waiting caller has to be woken by both. */
    ompi_modex_epoch_advance();

    OBJ_RELEASE(proc);
}

/* Ask whether this peer has committed; the answer also warms the cache.
 *
 * A Get naming a non-reserved key makes the server return every
 * non-reserved key the proc has. So the status answers the question, and
 * the payload spares the per-BTL Gets that follow a round trip. The
 * payload itself is of no interest here, which is why the callback drops
 * it.
 *
 * The key has to be named and non-reserved. A null or reserved key is
 * answerable from the job-level data the server holds before any commit,
 * so it would succeed whether the peer has published or not and latch the
 * peer as available far too early. OMPI_MODEX_KEY_PML is the one
 * non-reserved key every rank publishes whenever this path can run.
 *
 * No directives. The only candidate is PMIX_TIMEOUT, which would set the
 * re-ask interval and nothing else, and naming none keeps the info array
 * off the heap -- PMIx_Get_nb keeps it by pointer and reads it after this
 * has returned.
 *
 * Caller holds OPAL_PROC_FLAG_FETCHING and the reference this drops. */
static void ompi_modex_proc_fetch(ompi_proc_t *proc)
{
    pmix_proc_t pmix_proc;
    pmix_status_t rc = PMIX_ERROR;
    char *key;

    key = mca_base_component_to_string(&mca_pml_base_modex_component);
    if (NULL != key) {
        OPAL_PMIX_CONVERT_NAME(&pmix_proc, &proc->super.proc_name);
        rc = PMIx_Get_nb(&pmix_proc, key, NULL, 0, ompi_modex_get_cb, proc);
        free(key);
    }

    if (PMIX_SUCCESS == rc) {
        return; /* the callback owns the reference now */
    }
    /* Either it answered immediately or it will never answer; both mean
     * nobody else is going to move this peer along. */
    ompi_modex_get_cb(rc, NULL, proc);
}

/* The question, plus the fetch that answers it where peers are fetched one
 * at a time. Returns whether the caller can proceed now. */
static bool ompi_modex_available_or_fetch(ompi_proc_t *proc)
{
    if (ompi_modex_proc_available(proc)) {
        return true;
    }
    if (!ompi_modex_direct || NULL == proc) {
        return false;
    }
    /* Claim the fetch: of two threads reaching the same peer only one
     * issues the Get, so a caller retrying every progress tick does not
     * start a Get per tick. */
    if (opal_proc_learned_first(&proc->super, OPAL_PROC_FLAG_FETCHING)) {
        OBJ_RETAIN(proc);
        ompi_modex_proc_fetch(proc);
    }
    return false;
}

/* Handed down to OPAL, where the connection-info Gets are. Being asked
 * about a peer is the only notice this layer gets that somebody wants it,
 * and in the on-demand mode that is what starts the fetch. */
static bool ompi_modex_peer_not_ready(const opal_process_name_t *name)
{
    ompi_proc_t *proc;

    if (ompi_modex_all_ready_flag) {
        return false;
    }

    /* Deliberately the lookup that does not create: a name with no proc
     * here is not being wired, so this Get has nothing to wait for, and
     * creating one would recurse -- building a proc reads the modex. */
    proc = (ompi_proc_t *) ompi_proc_lookup(*name);
    if (NULL == proc) {
        return false;
    }

    return !ompi_modex_available_or_fetch(proc);
}

void ompi_modex_require_all(void)
{
    ompi_modex_require_all_flag = true;
}

int ompi_modex_start_exchange(void)
{
    pmix_info_t info;
    pmix_status_t rc;
    bool collect = opal_pmix_collect_all_data || ompi_modex_require_all_flag;

    if (opal_process_info.is_singleton) {
        ompi_modex_all_ready_flag = true;
        return OMPI_SUCCESS;
    }

    if (opal_pmix_base_async_modex && !collect) {
        ompi_modex_direct = true;
    }

    /* Must be live before anything can ask: from here a peer's connection
     * info may not be local yet, and in the on-demand mode being asked is
     * what starts the fetch. */
    opal_pmix_modex_not_ready = ompi_modex_peer_not_ready;

    if (ompi_modex_direct) {
        return OMPI_SUCCESS;
    }

    /* Left in flight on purpose, but not past the end of instance init:
     * PMIx names a collective by its participants alone, so a second fence
     * over this same set joins this one rather than starting its own, and
     * the two disagree about collecting data, so the server fails both. */
    ompi_modex_fence_active = true;
    OPAL_POST_OBJECT(&ompi_modex_fence_active);
    PMIX_INFO_LOAD(&info, PMIX_COLLECT_DATA, &collect, PMIX_BOOL);
    rc = PMIx_Fence_nb(NULL, 0, &info, 1, ompi_modex_fence_cb, NULL);
    PMIX_INFO_DESTRUCT(&info);
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        ompi_modex_fence_cb(PMIX_SUCCESS, NULL);
        return OMPI_SUCCESS;
    }
    if (PMIX_SUCCESS != rc) {
        ompi_modex_fence_active = false;
        return opal_pmix_convert_status(rc);
    }
    return OMPI_SUCCESS;
}

bool ompi_modex_all_ready(void)
{
    return ompi_modex_all_ready_flag;
}

bool ompi_modex_proc_ready(ompi_proc_t *proc)
{
    return ompi_modex_available_or_fetch(proc);
}

int ompi_modex_wait_if_needed(void)
{
    if (ompi_modex_fence_active) {
        OMPI_LAZY_WAIT_FOR_COMPLETION(ompi_modex_fence_active);
    }
    /* Pairs with the callback's post, for the status it wrote first: the
     * wait loop above only reads the flag. */
    OPAL_ACQUIRE_OBJECT(&ompi_modex_fence_active);
    if (PMIX_SUCCESS != ompi_modex_fence_status) {
        return opal_pmix_convert_status(ompi_modex_fence_status);
    }
    return OMPI_SUCCESS;
}

void ompi_modex_finalize(void)
{
    /* First: nothing here will move a peer along any more, so nobody may
     * be told "not yet" from this point on. */
    opal_pmix_modex_not_ready = NULL;

    /* A fetch still in flight holds a reference to its proc, and per-peer
     * state lives on the proc, which ompi_proc_finalize() destroys: there
     * is nothing per-peer to undo. The readiness flags must go back to
     * "nothing exchanged yet", or the next instance -- which publishes
     * fresh connection info -- reads peers as ready before its own fence
     * completes and reports a live peer as UNREACH. */
    ompi_modex_all_ready_flag = false;
    ompi_modex_fence_active = false;
    ompi_modex_fence_status = PMIX_SUCCESS;
    ompi_modex_direct = false;
}
