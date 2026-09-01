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
 * an instance learned, and the next instance is configured the same way. */
static bool ompi_modex_require_all_flag = false;

/* Whether everything this peer published is local, so a Get for one of its
 * keys can be issued and answered on its merits. A NULL proc asks about
 * the whole job, which only a fence can answer.
 *
 * Where a peer stands is kept on the peer, so there is no table here and
 * nothing to look up for a caller that already holds the proc. */
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
     * landed still says otherwise, so tell it now that somebody has asked.
     * Doing this for every proc the moment the fence completes would mean
     * walking the proc list from the callback that completed it, which is
     * the PMIx thread taking a lock an MPI thread holds across Gets. A
     * proc nobody ever asks about does not need telling. */
    opal_proc_learned(&proc->super, OPAL_PROC_FLAG_AVAILABLE);
    return true;
}

static void ompi_modex_fence_cb(pmix_status_t status, void *cbdata)
{
    (void) cbdata;
    OPAL_ACQUIRE_OBJECT(&ompi_modex_fence_active);
    ompi_modex_fence_status = status;
    /* A fence that failed exchanged nothing, so saying otherwise is worse
     * than saying nothing: readers take the flag as "everything this peer
     * published is local", and act on a key that is missing from it as a
     * key the peer never published. A btl reads that as "this peer is not
     * mine", and a peer no btl claims is declared unreachable -- a live
     * peer, reported as unreachable, over data that was never fetched.
     * So the failure is kept, and handed to whoever waits. */
    if (PMIX_SUCCESS == status) {
        ompi_modex_all_ready_flag = true;
    }
    ompi_modex_fence_active = false;
    OPAL_POST_OBJECT(&ompi_modex_fence_active);
}

static void ompi_modex_get_cb(pmix_status_t status, pmix_value_t *kv, void *cbdata)
{
    ompi_proc_t *proc = (ompi_proc_t *) cbdata;

    /* Ours once the callback has it, as it is for the blocking Get: PMIx
     * hands the value over rather than keeping it to free later. Nothing
     * is read out of it -- that the peer answered at all is the whole
     * answer here -- so it goes back immediately. */
    if (NULL != kv) {
        PMIX_VALUE_RELEASE(kv);
    }

    /* Available even if the fetch failed, and a failure here is final.
     *
     * The Get carries no PMIX_IMMEDIATE, and that is what decides both
     * halves of this. A request for a key the target has not published
     * yet is held by the server until it lands -- PMIx answers not-found
     * only to a caller that asked to be answered immediately -- so the
     * one case worth waiting for never arrives here as a failure. It
     * arrives as a success, later. What does arrive here is an answer:
     * the namespace or the rank is not known, the peer is gone, the
     * connection to the local server is not there. Asking again gets the
     * same answer, so the peer is moved along rather than left pending,
     * which also keeps a caller retrying once per progress tick from
     * starting a Get per tick for something that will never change.
     *
     * Both flags in one write. The failure is only worth keeping because
     * of what the first flag now means -- keys read as published or
     * absent, on a blob that was never fetched -- so a reader must not be
     * able to see that and not this. */
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

    OBJ_RELEASE(proc);
}

/* Ask the peer for something it publishes unconditionally, and take an
 * answer of any kind as the end of the wait. Success means it has
 * committed, at which point its whole blob is local: the per-BTL Gets that
 * follow are cache hits, and one of them coming back empty means that BTL
 * is unused rather than late.
 *
 * Caller has claimed OPAL_PROC_FLAG_FETCHING and holds the reference this
 * drops, which is what keeps the proc alive until the answer arrives. */
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
 * at a time. Returns what the gate and every retrying caller want to know:
 * whether they can proceed now. */
static bool ompi_modex_available_or_fetch(ompi_proc_t *proc)
{
    if (ompi_modex_proc_available(proc)) {
        return true;
    }
    if (!ompi_modex_direct || NULL == proc) {
        return false;
    }
    /* Claim the fetch. Two threads reaching the same peer both try and one
     * wins; the loser does not repeat it, which is what keeps a caller
     * retrying once per progress tick from starting a Get per tick. */
    if (opal_proc_learned_first(&proc->super, OPAL_PROC_FLAG_FETCHING)) {
        OBJ_RETAIN(proc);
        ompi_modex_proc_fetch(proc);
    }
    return false;
}

/* Handed down to OPAL, where the connection-info Gets are. Being asked
 * about a peer is the only notice this layer gets that somebody wants it,
 * and in the on-demand mode that is what starts the fetch -- so a caller
 * that can only retry still makes progress. */
static bool ompi_modex_peer_not_ready(const opal_process_name_t *name)
{
    ompi_proc_t *proc;

    if (ompi_modex_all_ready_flag) {
        return false;
    }

    /* Deliberately the lookup that does not create: a name nobody here
     * has a proc for is a name nobody here is wiring, so this Get is not
     * part of a wire-up and has nothing to wait for. Creating one instead
     * would also recurse, since building a proc reads the modex. */
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

    /* From here a peer's connection info may not be local yet, so a Get
     * for it has to be answered rather than issued -- and in the
     * on-demand mode being asked is what starts the fetch, so this has to
     * be live before anything can ask. */
    opal_pmix_modex_not_ready = ompi_modex_peer_not_ready;

    if (ompi_modex_direct) {
        return OMPI_SUCCESS;
    }

    /* Left in flight on purpose, but not past the end of instance init:
     * PMIx names a collective by its participants alone, so a second
     * fence over this same set, posted before this one has completed, is
     * not a second collective to the server -- it is more contributions
     * to this one. The two then disagree about whether data is being
     * collected, which the server answers by failing the whole thing, and
     * neither is the barrier its caller took it for. Hence the wait
     * before the barrier that ends instance init, which is the next fence
     * over these procs. */
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
    /* Pairs with the callback's post, for the status it wrote first: that
     * loop only reads the flag, and a caller arriving after the fence
     * landed does not even do that. */
    OPAL_ACQUIRE_OBJECT(&ompi_modex_fence_active);
    if (PMIX_SUCCESS != ompi_modex_fence_status) {
        return opal_pmix_convert_status(ompi_modex_fence_status);
    }
    return OMPI_SUCCESS;
}

void ompi_modex_finalize(void)
{
    /* Before anything else: nobody may be told "not yet" from here on,
     * since there is no longer anything that would tell them otherwise. */
    opal_pmix_modex_not_ready = NULL;

    /* A fetch still in flight holds a reference to its proc, so it has
     * something to write into whenever it lands. Where each peer stands is
     * on the peer, and ompi_proc_finalize() drives every proc to
     * destruction, so there is no per-peer state to undo here.
     *
     * A later instance publishes fresh connection info (new TCP ports, a
     * new shared-memory segment) and starts its own exchange, so every
     * readiness flag has to go back to "nothing has been exchanged yet".
     * Leaving them set makes the next instance read the peers as ready
     * before its fence completes, which reports a live peer as UNREACH
     * instead of retrying it. */
    ompi_modex_all_ready_flag = false;
    ompi_modex_fence_active = false;
    ompi_modex_fence_status = PMIX_SUCCESS;
    ompi_modex_direct = false;
}
