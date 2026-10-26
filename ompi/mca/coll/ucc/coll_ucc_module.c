/**
 * Copyright (c) 2021      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2026 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "coll_ucc.h"
#include "ompi/mca/coll/base/coll_base_util.h"
#include <string.h>
#include "ompi/group/group.h"
#include "coll_ucc_common.h"
#include "coll_ucc_dtypes.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/instance/instance.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_ucc_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

static void mca_coll_ucc_module_clear(mca_coll_ucc_module_t *ucc_module)
{
    ucc_module->ucc_team                              = NULL;
    ucc_module->ep_map_ranks                          = NULL;
    ucc_module->modules_idx                           = -1;
    ucc_module->previous_allreduce                    = NULL;
    ucc_module->previous_allreduce_module             = NULL;
    ucc_module->previous_iallreduce                   = NULL;
    ucc_module->previous_iallreduce_module            = NULL;
    ucc_module->previous_barrier                      = NULL;
    ucc_module->previous_barrier_module               = NULL;
    ucc_module->previous_ibarrier                     = NULL;
    ucc_module->previous_ibarrier_module              = NULL;
    ucc_module->previous_bcast                        = NULL;
    ucc_module->previous_bcast_module                 = NULL;
    ucc_module->previous_ibcast                       = NULL;
    ucc_module->previous_ibcast_module                = NULL;
    ucc_module->previous_alltoall                     = NULL;
    ucc_module->previous_alltoall_module              = NULL;
    ucc_module->previous_ialltoall                    = NULL;
    ucc_module->previous_ialltoall_module             = NULL;
    ucc_module->previous_alltoallv                    = NULL;
    ucc_module->previous_alltoallv_module             = NULL;
    ucc_module->previous_ialltoallv                   = NULL;
    ucc_module->previous_ialltoallv_module            = NULL;
    ucc_module->previous_allgather                    = NULL;
    ucc_module->previous_allgather_module             = NULL;
    ucc_module->previous_iallgather                   = NULL;
    ucc_module->previous_iallgather_module            = NULL;
    ucc_module->previous_allgatherv                   = NULL;
    ucc_module->previous_allgatherv_module            = NULL;
    ucc_module->previous_iallgatherv                  = NULL;
    ucc_module->previous_iallgatherv_module           = NULL;
    ucc_module->previous_reduce                       = NULL;
    ucc_module->previous_reduce_module                = NULL;
    ucc_module->previous_ireduce                      = NULL;
    ucc_module->previous_ireduce_module               = NULL;
    ucc_module->previous_gather                       = NULL;
    ucc_module->previous_gather_module                = NULL;
    ucc_module->previous_igather                      = NULL;
    ucc_module->previous_igather_module               = NULL;
    ucc_module->previous_gatherv                      = NULL;
    ucc_module->previous_gatherv_module               = NULL;
    ucc_module->previous_igatherv                     = NULL;
    ucc_module->previous_igatherv_module              = NULL;
    ucc_module->previous_reduce_scatter_block         = NULL;
    ucc_module->previous_reduce_scatter_block_module  = NULL;
    ucc_module->previous_ireduce_scatter_block        = NULL;
    ucc_module->previous_ireduce_scatter_block_module = NULL;
    ucc_module->previous_reduce_scatter               = NULL;
    ucc_module->previous_reduce_scatter_module        = NULL;
    ucc_module->previous_ireduce_scatter              = NULL;
    ucc_module->previous_ireduce_scatter_module       = NULL;
    ucc_module->previous_scatterv                     = NULL;
    ucc_module->previous_scatterv_module              = NULL;
    ucc_module->previous_iscatterv                    = NULL;
    ucc_module->previous_iscatterv_module             = NULL;
    ucc_module->previous_scatter                      = NULL;
    ucc_module->previous_scatter_module               = NULL;
    ucc_module->previous_iscatter                     = NULL;
    ucc_module->previous_iscatter_module              = NULL;
    ucc_module->previous_allreduce_init               = NULL;
    ucc_module->previous_allreduce_init_module        = NULL;
    ucc_module->previous_barrier_init                 = NULL;
    ucc_module->previous_barrier_init_module          = NULL;
    ucc_module->previous_bcast_init                   = NULL;
    ucc_module->previous_bcast_init_module            = NULL;
    ucc_module->previous_alltoall_init                = NULL;
    ucc_module->previous_alltoall_init_module         = NULL;
    ucc_module->previous_alltoallv_init               = NULL;
    ucc_module->previous_alltoallv_init_module        = NULL;
    ucc_module->previous_allgather_init               = NULL;
    ucc_module->previous_allgather_init_module        = NULL;
    ucc_module->previous_allgatherv_init              = NULL;
    ucc_module->previous_allgatherv_init_module       = NULL;
    ucc_module->previous_reduce_init                  = NULL;
    ucc_module->previous_reduce_init_module           = NULL;
    ucc_module->previous_gather_init                  = NULL;
    ucc_module->previous_gather_init_module           = NULL;
    ucc_module->previous_gatherv_init                 = NULL;
    ucc_module->previous_gatherv_init_module          = NULL;
    ucc_module->previous_reduce_scatter_block_init    = NULL;
    ucc_module->previous_reduce_scatter_block_init_module = NULL;
    ucc_module->previous_reduce_scatter_init          = NULL;
    ucc_module->previous_reduce_scatter_init_module   = NULL;
    ucc_module->previous_scatterv_init                = NULL;
    ucc_module->previous_scatterv_init_module         = NULL;
    ucc_module->previous_scatter_init                 = NULL;
    ucc_module->previous_scatter_init_module          = NULL;
}

static void mca_coll_ucc_module_construct(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_module_clear(ucc_module);
    ucc_module->lazy    = false;
    ucc_module->active  = 0;
    ucc_module->team_id = -1;
    ucc_module->state   = MCA_COLL_UCC_PENDING;
    ucc_module->domain  = NULL;
    ucc_module->nosharp = false;
    ucc_module->twin    = NULL;
    ucc_module->sharp_explicit = false;
    ucc_module->sharp_key      = false;
}

static inline void mca_coll_ucc_req_account(mca_coll_ucc_req_t *coll_req, int delta)
{
    if (NULL != coll_req->module) {
        OPAL_THREAD_ADD_FETCH32(&coll_req->module->domain->active, delta);
        OPAL_THREAD_ADD_FETCH32(&coll_req->module->active, delta);
    }
}

#define UCC_EXT_TEAM_ID_MAX 32767

/* Same value on every member: folded from the extended cid shared by all of them. */
static int mca_coll_ucc_team_id_candidate(ompi_communicator_t *comm)
{
    uint64_t h = comm->c_contextid.cid_base * 0x9E3779B97F4A7C15ULL ^
                 comm->c_contextid.cid_sub.u64 * 0xC2B2AE3D27D4EB4FULL;
    h ^= h >> 29;
    h ^= h >> 17;
    return (int)(h & UCC_EXT_TEAM_ID_MAX);
}

/* Reserve id on the domain; false if an overlapping live team already uses it. */
static bool mca_coll_ucc_team_id_take(mca_coll_ucc_oob_domain_t *domain, int id)
{
    bool ok;

    OPAL_THREAD_LOCK(&mca_coll_ucc_component.lock);
    ok = !(domain->team_ids[id >> 6] & (1ULL << (id & 63)));
    if (ok) {
        domain->team_ids[id >> 6] |= 1ULL << (id & 63);
    }
    OPAL_THREAD_UNLOCK(&mca_coll_ucc_component.lock);
    return ok;
}

static void mca_coll_ucc_team_id_release(mca_coll_ucc_module_t *ucc_module)
{
    if (ucc_module->team_id >= 0 && NULL != ucc_module->domain) {
        OPAL_THREAD_LOCK(&mca_coll_ucc_component.lock);
        ucc_module->domain->team_ids[ucc_module->team_id >> 6] &= ~(1ULL << (ucc_module->team_id & 63));
        OPAL_THREAD_UNLOCK(&mca_coll_ucc_component.lock);
    }
    ucc_module->team_id = -1;
}

static void mca_coll_ucc_abandoned_destruct(mca_coll_ucc_abandoned_t *ab)
{
    free(ab->ep_map_ranks);
    if (NULL != ab->domain) {
        OBJ_RELEASE(ab->domain);
    }
}
OBJ_CLASS_INSTANCE(mca_coll_ucc_abandoned_t, opal_list_item_t, NULL, mca_coll_ucc_abandoned_destruct);

/* UCC keeps a team it cannot destroy now: quarantine it; its context leaks at finalize. */
static void mca_coll_ucc_team_quarantine(mca_coll_ucc_module_t *ucc_module, ucc_status_t status)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    mca_coll_ucc_abandoned_t *ab;

    /* Counted first: a team we cannot even record still pins its context. */
    OPAL_THREAD_ADD_FETCH32(&ucc_module->domain->quarantined, 1);
    ab = OBJ_NEW(mca_coll_ucc_abandoned_t);
    if (NULL == ab) {
        UCC_ERROR("cannot record quarantined team for comm %p: leaking it and its ep map",
                  (void*)ucc_module->comm);
        goto out;
    }
    ab->team         = ucc_module->ucc_team;
    ab->ep_map_ranks = ucc_module->ep_map_ranks;
    ab->domain       = ucc_module->domain;
    ab->team_id      = ucc_module->team_id;
    OBJ_RETAIN(ab->domain);
    OPAL_THREAD_LOCK(&cm->lock);
    opal_list_append(&cm->abandoned, &ab->super);
    OPAL_THREAD_UNLOCK(&cm->lock);
    UCC_VERBOSE(1, "quarantined ucc team for comm %p on domain %p (%s)",
                (void*)ucc_module->comm, (void*)ucc_module->domain, ucc_status_string(status));
out:
    ucc_module->ucc_team     = NULL;
    ucc_module->ep_map_ranks = NULL;    /* owned by the record (or leaked with the team), as is the team id bit */
    ucc_module->team_id      = -1;
}

/* Nonblocking team destroy, waited on for at most team_destroy_timeout seconds (<= 0: no bound).
   During finalize a destroy that needs peers is not waited on at all: the caller leaks it. */
static ucc_status_t mca_coll_ucc_team_destroy_wait(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    double                    deadline = 0.0;
    ucc_status_t              status;

    if (cm->finalizing || ompi_mpi_state >= OMPI_MPI_STATE_FINALIZE_STARTED) {
        return ucc_team_destroy(ucc_module->ucc_team);
    }
    if (cm->team_destroy_timeout > 0) {
        deadline = ompi_wtime() + cm->team_destroy_timeout;
    }
    while (UCC_INPROGRESS == (status = ucc_team_destroy(ucc_module->ucc_team))) {
        ucc_context_progress(ucc_module->domain->ucc_context);
        opal_progress();
        if (deadline > 0.0 && ompi_wtime() > deadline) {
            return UCC_ERR_TIMED_OUT;
        }
    }
    return status;
}

/* A never-active team a peer did not post: destroy it, or quarantine it. */
static void mca_coll_ucc_team_abandon(mca_coll_ucc_module_t *ucc_module, const char *why)
{
    ucc_status_t status;

    UCC_VERBOSE(1, "abandoning ucc team for comm %p: %s", (void*)ucc_module->comm, why);
    status = mca_coll_ucc_team_destroy_wait(ucc_module);
    if (UCC_OK == status) {
        ucc_module->ucc_team = NULL;
        mca_coll_ucc_team_id_release(ucc_module);
        return;
    }
    mca_coll_ucc_team_quarantine(ucc_module, status);
}

/* Retry quarantined team destroys; only a recorded team can lower domain->quarantined. */
static void mca_coll_ucc_abandoned_retry(void)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_abandoned_t  *ab, *next;

    OPAL_LIST_FOREACH_SAFE(ab, next, &cm->abandoned, mca_coll_ucc_abandoned_t) {
        if (UCC_OK == ucc_team_destroy(ab->team)) {
            ab->domain->team_ids[ab->team_id >> 6] &= ~(1ULL << (ab->team_id & 63));
            OPAL_THREAD_ADD_FETCH32(&ab->domain->quarantined, -1);
            opal_list_remove_item(&cm->abandoned, &ab->super);
            OBJ_RELEASE(ab);
        }
    }
}

static int mca_coll_ucc_progress(void)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *domain;
    int                        i, n = 0;

    if (!opal_using_threads()) {
        /* Nothing can mutate the list under us: walk it in place. */
        OPAL_LIST_FOREACH(domain, &cm->domains, mca_coll_ucc_oob_domain_t) {
            if (domain->active > 0) {
                ucc_context_progress(domain->ucc_context);
            }
        }
        return OPAL_SUCCESS;
    }
    /* One thread at a time owns progress_ctx: fill it under the lock, progress without it. */
    if (1 != OPAL_THREAD_ADD_FETCH32(&cm->progressing, 1)) {
        OPAL_THREAD_ADD_FETCH32(&cm->progressing, -1);
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&cm->lock);
    if (cm->progress_ctx_cap < cm->domain_count) {
        ucc_context_h *grown = realloc(cm->progress_ctx, cm->domain_count * sizeof(*grown));
        if (NULL != grown) {
            cm->progress_ctx     = grown;
            cm->progress_ctx_cap = cm->domain_count;
        }
    }
    OPAL_LIST_FOREACH(domain, &cm->domains, mca_coll_ucc_oob_domain_t) {
        if (domain->active > 0 && n < cm->progress_ctx_cap) {
            cm->progress_ctx[n++] = domain->ucc_context;
        }
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
    for (i = 0; i < n; i++) {
        ucc_context_progress(cm->progress_ctx[i]);
    }
    OPAL_THREAD_ADD_FETCH32(&cm->progressing, -1);
    return OPAL_SUCCESS;
}

static void mca_coll_ucc_domain_destroy(mca_coll_ucc_oob_domain_t *domain)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    bool                      sharp = !domain->nosharp;

    OPAL_THREAD_LOCK(&cm->lock);
    opal_list_remove_item(&cm->domains, &domain->super);
    OPAL_THREAD_UNLOCK(&cm->lock);
    UCC_VERBOSE(1, "destroying ucc oob domain %p for comm %p (size %d)",
                (void*)domain, (void*)domain->comm, ompi_comm_size(domain->comm));
    ucc_context_destroy(domain->ucc_context);
    UCC_VERBOSE(1, "destroyed ucc oob domain %p", (void*)domain);
    /* Drop the reference the domain held on its bootstrap communicator (only
       taken for non-intrinsic communicators; see mca_coll_ucc_domain_create). */
    if (!OMPI_COMM_IS_INTRINSIC(domain->comm)) {
        OBJ_RELEASE(domain->comm);
    }
    OBJ_RELEASE(domain);

    OPAL_THREAD_LOCK(&cm->lock);
    cm->sharp_domain_count -= sharp;
    if (0 == --cm->domain_count && 0 == cm->orphans) {
        if (cm->progress_registered) {
            opal_progress_unregister(mca_coll_ucc_progress);
            cm->progress_registered = false;
        }
        ucc_finalize(cm->ucc_lib);
        UCC_VERBOSE(1, "finalized ucc library");
        cm->ucc_lib = NULL;
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
}

/* Drop one module's reference; contexts are destroyed only by the instance finalize sweep. */
static void mca_coll_ucc_domain_unref(mca_coll_ucc_oob_domain_t *domain)
{
    if (NULL != domain) {
        OPAL_THREAD_ADD_FETCH32(&domain->refcount, -1);
    }
}


/* Total order on (bootstrap extended cid, flavour): the same on every rank, unlike local list order. */
static int mca_coll_ucc_domain_cmp(const void *pa, const void *pb)
{
    const mca_coll_ucc_oob_domain_t *a = *(mca_coll_ucc_oob_domain_t * const *)pa;
    const mca_coll_ucc_oob_domain_t *b = *(mca_coll_ucc_oob_domain_t * const *)pb;
    ompi_comm_extended_cid_t ca = a->comm->c_contextid, cb = b->comm->c_contextid;

    if (ca.cid_base != cb.cid_base) {
        return ca.cid_base < cb.cid_base ? -1 : 1;
    }
    if (ca.cid_sub.u64 != cb.cid_sub.u64) {
        return ca.cid_sub.u64 < cb.cid_sub.u64 ? -1 : 1;
    }
    return (int)a->nosharp - (int)b->nosharp;
}

/* Runs from ompi_mpi_instance_finalize_common(), before communicators and the PML go away. */
static void mca_coll_ucc_instance_finalize(void)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *domain, *next;
    mca_coll_ucc_module_t     *m;
    int                        i, n, leaked = 0, torn = 0;
    /* Waiting on peers is legal only past MPI_Finalize's fence, or with sessions_teardown. */
    bool fenced = opal_process_info.is_singleton ||
                  (ompi_mpi_state >= OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT &&
                   !ompi_async_mpi_finalize);
    bool teardown = fenced || cm->sessions_teardown;

    cm->finalize_hook_registered = false;
    cm->finalizing = true;
    n = opal_pointer_array_get_size(&cm->modules);

    /* One destroy attempt per team: one that needs peers at finalize is leaked, not waited on. */
    for (i = 0; i < n; i++) {
        m = (mca_coll_ucc_module_t *)opal_pointer_array_get_item(&cm->modules, i);
        if (NULL == m || NULL == m->ucc_team) {
            continue;
        }
        ucc_status_t st = ucc_team_destroy(m->ucc_team);
        if (UCC_OK == st) {
            m->ucc_team = NULL;
        } else {
            UCC_VERBOSE(1, "finalize: team destroy for comm %p %s: leaking it", (void*)m->comm,
                        UCC_INPROGRESS == st ? "needs peers" : ucc_status_string(st));
            mca_coll_ucc_team_quarantine(m, st);
        }
    }

    for (i = 0; i < n; i++) {
        m = (mca_coll_ucc_module_t *)opal_pointer_array_get_item(&cm->modules, i);
        if (NULL != m) {
            mca_coll_ucc_team_id_release(m);
            mca_coll_ucc_domain_unref(m->domain);
            mca_coll_ucc_domain_unref(m->twin);
            m->domain      = NULL;
            m->twin        = NULL;
            m->modules_idx = -1;
            opal_pointer_array_set_item(&cm->modules, i, NULL);
        }
    }

    /* Each context destroy barriers: go in extended-cid order, identical on every rank. */
    {
        opal_list_t ordered;
        OBJ_CONSTRUCT(&ordered, opal_list_t);
        while (!opal_list_is_empty(&cm->domains)) {
            mca_coll_ucc_oob_domain_t *min = NULL, *d;
            OPAL_LIST_FOREACH(d, &cm->domains, mca_coll_ucc_oob_domain_t) {
                if (NULL == min || mca_coll_ucc_domain_cmp(&d, &min) < 0) {
                    min = d;
                }
            }
            opal_list_remove_item(&cm->domains, &min->super);
            opal_list_append(&ordered, &min->super);
        }
        opal_list_join(&cm->domains, opal_list_get_end(&cm->domains), &ordered);
        OBJ_DESTRUCT(&ordered);
    }
    mca_coll_ucc_abandoned_retry();
    OPAL_LIST_FOREACH_SAFE(domain, next, &cm->domains, mca_coll_ucc_oob_domain_t) {
        bool destroyable;
        int  q, anyq = 0;
        destroyable = teardown && !domain->orphaned && !OMPI_COMM_IS_DYNAMIC(domain->comm);
        if (destroyable) {
            if (0 != domain->refcount) {
                UCC_ERROR("domain %p still referenced by %d modules at finalize: leaking it",
                          (void*)domain, (int)domain->refcount);
            }
            /* Rank-local reasons to leak must still reach the allreduce, or the peers hang in it. */
            q = 0 != domain->quarantined || 0 != domain->refcount;
            if (OMPI_SUCCESS != ompi_coll_base_allreduce_intra_recursivedoubling(
                                    &q, &anyq, 1, &ompi_mpi_int.dt, &ompi_mpi_op_max.op,
                                    domain->comm, NULL) || anyq) {
                UCC_VERBOSE(1, "domain %p quarantined or referenced on some rank: leaking it",
                            (void*)domain);
                destroyable = false;
            }
        }
        if (destroyable) {
            mca_coll_ucc_domain_destroy(domain);
            torn++;
        } else {
            /* Orphaned, quarantined, dynamic or unfenced: drop our references, leak the context. */
            opal_list_remove_item(&cm->domains, &domain->super);
            if (!OMPI_COMM_IS_INTRINSIC(domain->comm)) {
                OBJ_RELEASE(domain->comm);
            }
            cm->sharp_domain_count -= !domain->nosharp;
            OBJ_RELEASE(domain);
            cm->domain_count--;
            leaked++;
        }
    }
    if (!fenced && torn) {
        UCC_VERBOSE(1, "no finalize fence: tore down %d ucc contexts over retained comms", torn);
    }
    if (leaked) {
        if (cm->progress_registered) {
            opal_progress_unregister(mca_coll_ucc_progress);
            cm->progress_registered = false;
        }
        UCC_VERBOSE(1, "%s: leaking %d ucc contexts",
                    teardown ? "orphaned or dynamic" : "no finalize fence", leaked);
    }
    /* The library can exist without any domain (a comm that never ran a collective). */
    OPAL_THREAD_LOCK(&cm->lock);
    if (NULL != cm->ucc_lib && 0 == cm->domain_count && 0 == cm->orphans && 0 == leaked) {
        if (cm->progress_registered) {
            opal_progress_unregister(mca_coll_ucc_progress);
            cm->progress_registered = false;
        }
        ucc_finalize(cm->ucc_lib);
        cm->ucc_lib = NULL;
        UCC_VERBOSE(1, "finalized ucc library");
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
}

/*
 * Tear down a module's UCC state: drain its in-flight operations, destroy its
 * team and drop its domain references.  Runs when the coll base releases the
 * module at communicator destruct (coll_module_disable), so on each rank at
 * its own time: nothing here agrees with peers or uses the communicator's
 * point-to-point, which the destructor tears down next.  Team destroy is
 * collective over the team's own ranks but needs no simultaneity and, with
 * coll/ucc supplying team ids, no UCC-level OOB; the context lives until the
 * instance finalize sweep.
 */
static void mca_coll_ucc_module_teardown(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    ucc_status_t              status;

    if (ucc_module->modules_idx >= 0) {
        OPAL_THREAD_LOCK(&cm->lock);
        opal_pointer_array_set_item(&cm->modules, ucc_module->modules_idx, NULL);
        OPAL_THREAD_UNLOCK(&cm->lock);
        ucc_module->modules_idx = -1;
    }
    if (NULL != ucc_module->ucc_team) {
        /* MPI_Comm_free lets pending operations complete: drain them before the team goes. */
        if (ucc_module->active > 0) {
            UCC_VERBOSE(1, "draining %d in-flight collectives for comm %p",
                        (int)ucc_module->active, (void*)ucc_module->comm);
            while (ucc_module->active > 0) {
                ucc_context_progress(ucc_module->domain->ucc_context);
                opal_progress();
            }
        }
        /* The team's transports destroy locally; one that does not is quarantined, not waited on. */
        status = mca_coll_ucc_team_destroy_wait(ucc_module);
        if (UCC_OK == status) {
            ucc_module->ucc_team = NULL;
        } else {
            if (UCC_INPROGRESS == status &&
                (cm->finalizing || ompi_mpi_state >= OMPI_MPI_STATE_FINALIZE_STARTED)) {
                UCC_VERBOSE(1, "team destroy for comm %p needs peers at finalize: quarantining it",
                            (void*)ucc_module->comm);
            } else {
                UCC_ERROR("UCC team destroy failed for comm %p: %s", (void*)ucc_module->comm,
                          ucc_status_string(status));
            }
            mca_coll_ucc_team_quarantine(ucc_module, status);
        }
    }
    mca_coll_ucc_team_id_release(ucc_module);
    mca_coll_ucc_domain_unref(ucc_module->domain);
    ucc_module->domain = NULL;
    mca_coll_ucc_domain_unref(ucc_module->twin);
    ucc_module->twin = NULL;
}

static void mca_coll_ucc_module_destruct(mca_coll_ucc_module_t *ucc_module)
{
    /* Normally a no-op after coll_module_disable; covers a module whose enable failed. */
    mca_coll_ucc_module_teardown(ucc_module);
    /* The team (the only user of the ep_map backing array) has already been
       destroyed at this point, so it is safe to release the array now. */
    free(ucc_module->ep_map_ranks);
    mca_coll_ucc_module_clear(ucc_module);
}

typedef struct oob_allgather_req{
    void           *sbuf;
    void           *rbuf;
    void           *oob_coll_ctx;
    size_t          msglen;
    int             iter;
    ompi_request_t *reqs[2];
} oob_allgather_req_t;

static ucc_status_t oob_allgather_test(void *req)
{
    oob_allgather_req_t       *oob_req = (oob_allgather_req_t*)req;
    /* The OOB context is the domain (see mca_coll_ucc_domain_create); its
       bootstrap communicator backs the point-to-point ring. */
    mca_coll_ucc_oob_domain_t *domain  = (mca_coll_ucc_oob_domain_t *)oob_req->oob_coll_ctx;
    ompi_communicator_t       *comm    = domain->comm;
    char                *tmpsend = NULL;
    char                *tmprecv = NULL;
    size_t               msglen  = oob_req->msglen;
    int                  probe_count = 5;
    int rank, size, sendto, recvfrom, recvdatafrom,
        senddatafrom, completed, probe, rc;

    /* The OOB only runs while bootstrapping or tearing down the context, and
       always over the full bootstrap communicator.  The domain holds its own
       reference to that communicator for its whole lifetime, so it is valid
       here even if the user already freed its handle to it.  A NULL comm
       would mean that invariant was violated -- fail loudly rather than
       dereference a freed communicator. */
    if (NULL == comm) {
        UCC_ERROR("UCC OOB invoked after its bootstrap communicator was freed");
        ompi_rte_abort(1, "coll/ucc: OOB used after bootstrap communicator was freed");
    }

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    if (oob_req->iter == 0) {
        tmprecv = (char*) oob_req->rbuf + (ptrdiff_t)rank * (ptrdiff_t)msglen;
        memcpy(tmprecv, oob_req->sbuf, msglen);
    }
    sendto   = (rank + 1) % size;
    recvfrom = (rank - 1 + size) % size;
    for (; oob_req->iter < size - 1; oob_req->iter++) {
        if (oob_req->iter > 0) {
            probe = 0;
            do {
                ompi_request_test_all(2, oob_req->reqs, &completed, MPI_STATUS_IGNORE);
                probe++;
            } while (!completed && probe < probe_count);
            if (!completed) {
                return UCC_INPROGRESS;
            }
        }
        recvdatafrom = (rank - oob_req->iter - 1 + size) % size;
        senddatafrom = (rank - oob_req->iter + size) % size;
        tmprecv = (char*)oob_req->rbuf + (ptrdiff_t)recvdatafrom * (ptrdiff_t)msglen;
        tmpsend = (char*)oob_req->rbuf + (ptrdiff_t)senddatafrom * (ptrdiff_t)msglen;
        rc = MCA_PML_CALL(isend(tmpsend, msglen, MPI_BYTE, sendto, MCA_COLL_BASE_TAG_UCC,
                           MCA_PML_BASE_SEND_STANDARD, comm, &oob_req->reqs[0]));
        if (OMPI_SUCCESS != rc) {
            return UCC_ERR_NO_MESSAGE;
        }
        rc = MCA_PML_CALL(irecv(tmprecv, msglen, MPI_BYTE, recvfrom,
                           MCA_COLL_BASE_TAG_UCC, comm, &oob_req->reqs[1]));
	if (OMPI_SUCCESS != rc) {
            return UCC_ERR_NO_MESSAGE;
	}
    }
    probe = 0;
    do {
        ompi_request_test_all(2, oob_req->reqs, &completed, MPI_STATUS_IGNORE);
        probe++;
    } while (!completed && probe < probe_count);
    if (!completed) {
        return UCC_INPROGRESS;
    }
    return UCC_OK;
}

static ucc_status_t oob_allgather_free(void *req)
{
    free(req);
    return UCC_OK;
}

static ucc_status_t oob_allgather(void *sbuf, void *rbuf, size_t msglen,
                                  void *oob_coll_ctx, void **req)
{
    oob_allgather_req_t *oob_req = malloc(sizeof(*oob_req));
    oob_req->sbuf                = sbuf;
    oob_req->rbuf                = rbuf;
    oob_req->msglen              = msglen;
    oob_req->oob_coll_ctx        = oob_coll_ctx;
    oob_req->iter                = 0;
    oob_req->reqs[0]             = MPI_REQUEST_NULL;
    oob_req->reqs[1]             = MPI_REQUEST_NULL;
    *req                         = oob_req;
    return UCC_OK;
}


static void mca_coll_ucc_instance_finalize(void);

/* Disables tl/sharp in a context config: no SHARP job at context create, and every SHARP team declines. */
static ucc_status_t mca_coll_ucc_config_nosharp(ucc_context_config_h ctx_config)
{
    ucc_status_t status;

    status = ucc_context_config_modify(ctx_config, "tl/sharp", "CONTEXT_PER_TEAM", "y");
    if (UCC_OK == status) {
        status = ucc_context_config_modify(ctx_config, "tl/sharp", "TEAM_MAX_PPN", "0");
    }
    return status;
}

/* Whether tl/sharp is part of the library: without it every context is SHARP-free already. */
static void mca_coll_ucc_sharp_detect(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;
    ucc_context_config_h      ctx_config;
    ucc_status_t              status;

    cm->sharp_in_lib = false;
    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        return;
    }
    status = mca_coll_ucc_config_nosharp(ctx_config);
    ucc_context_config_release(ctx_config);
    if (UCC_OK != status && UCC_ERR_NOT_FOUND != status) {
        UCC_VERBOSE(1, "cannot disable tl/sharp in a ucc context (%s): all contexts allow SHARP",
                    ucc_status_string(status));
    }
    cm->sharp_in_lib = (UCC_OK == status);
}

/*
 * One-time initialization of the shared UCC library and the request free
 * list.  Called when the first UCC module is enabled or the first OOB domain
 * is created; the resources persist across domain churn and are torn down
 * when the last domain is destroyed or at the instance finalize hook
 * (library) / at component close (request free list).
 */
static int mca_coll_ucc_lib_init(void)
{
    mca_coll_ucc_component_t     *cm = &mca_coll_ucc_component;
    ucc_lib_config_h              lib_config;
    ucc_thread_mode_t             tm_requested;
    ucc_lib_params_t              lib_params;

    tm_requested           = ompi_mpi_thread_multiple ? UCC_THREAD_MULTIPLE :
                                                        UCC_THREAD_SINGLE;
    lib_params.mask        = UCC_LIB_PARAM_FIELD_THREAD_MODE;
    lib_params.thread_mode = tm_requested;

    if (UCC_OK != ucc_lib_config_read("OMPI", NULL, &lib_config)) {
        UCC_ERROR("UCC lib config read failed");
        return OMPI_ERROR;
    }
    if (strlen(cm->cls) > 0) {
        if (UCC_OK != ucc_lib_config_modify(lib_config, "CLS", cm->cls)) {
            ucc_lib_config_release(lib_config);
            UCC_ERROR("failed to modify UCC lib config to set CLS");
            return OMPI_ERROR;
        }
    }

    if (UCC_OK != ucc_init(&lib_params, lib_config, &cm->ucc_lib)) {
        UCC_ERROR("UCC lib init failed");
        ucc_lib_config_release(lib_config);
        return OMPI_ERROR;
    }
    ucc_lib_config_release(lib_config);

    cm->ucc_lib_attr.mask = UCC_LIB_ATTR_FIELD_THREAD_MODE |
                            UCC_LIB_ATTR_FIELD_COLL_TYPES;
    if (UCC_OK != ucc_lib_get_attr(cm->ucc_lib, &cm->ucc_lib_attr)) {
        UCC_ERROR("UCC get lib attr failed");
        goto cleanup_lib;
    }

    if (cm->ucc_lib_attr.thread_mode < tm_requested) {
        UCC_ERROR("UCC library doesn't support MPI_THREAD_MULTIPLE");
        goto cleanup_lib;
    }
    mca_coll_ucc_sharp_detect();

    if (!cm->requests_initialized) {
        OBJ_CONSTRUCT(&cm->requests, opal_free_list_t);
        opal_free_list_init(&cm->requests, sizeof(mca_coll_ucc_req_t),
                            opal_cache_line_size, OBJ_CLASS(mca_coll_ucc_req_t),
                            0, 0,                     /* no payload data */
                            8, -1, 8,                 /* num_to_alloc, max, per alloc */
                            NULL, 0, NULL, NULL, NULL /* no Mpool or init function */);
        cm->requests_initialized = true;
    }

    if (!cm->finalize_hook_registered) {
        ompi_mpi_instance_append_finalize(mca_coll_ucc_instance_finalize);
        cm->finalize_hook_registered = true;
    }

    UCC_VERBOSE(1, "initialized ucc library, tl/sharp configured: %d", (int)cm->sharp_in_lib);
    return OMPI_SUCCESS;

cleanup_lib:
    ucc_finalize(cm->ucc_lib);
    cm->ucc_lib = NULL;
    return OMPI_ERROR;
}

/* SHARP resources were refused with this many SHARP-capable contexts alive: back off until some are gone. */
static void mca_coll_ucc_note_refusal(void)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    OPAL_THREAD_LOCK(&cm->lock);
    if (0 == cm->refused_at_count || cm->sharp_domain_count < cm->refused_at_count) {
        cm->refused_at_count = cm->sharp_domain_count;
        UCC_VERBOSE(1, "ucc context creation refused resources at %d live sharp-capable contexts: backing off",
                    cm->refused_at_count);
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
}

/*
 * Create a new OOB domain (and its UCC context) bootstrapped over @comm.
 * The OOB is bound to the domain (coll_info = domain), and the domain takes
 * its own reference to @comm so the OOB communicator stays valid for the
 * whole life of the context even if the user frees its handle to @comm.
 */
static int mca_coll_ucc_domain_create(ompi_communicator_t *comm, bool nosharp,
                                      mca_coll_ucc_oob_domain_t **domain_out)
{
    ucc_status_t               ucc_status;
    mca_coll_ucc_component_t   *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t  *domain = NULL;
    ucc_context_config_h        ctx_config;
    ucc_context_params_t        ctx_params;
    char                        str_buf[256];
    unsigned                    ucc_api_major, ucc_api_minor, ucc_api_patch;
    bool                        first, inject;

    ucc_get_version(&ucc_api_major, &ucc_api_minor, &ucc_api_patch);

    OPAL_THREAD_LOCK(&cm->lock);
    if (cm->lib_failed) {
        OPAL_THREAD_UNLOCK(&cm->lock);
        return OMPI_ERROR;
    }
    /* The shared UCC library is re-created here if the last domain finalized it. */
    first = (NULL == cm->ucc_lib);
    if (first && OMPI_SUCCESS != mca_coll_ucc_lib_init()) {
        cm->lib_failed = true;
        OPAL_THREAD_UNLOCK(&cm->lock);
        return OMPI_ERROR;
    }
    cm->domain_count++;                       /* reserve: keeps the lib alive while we create */
    cm->sharp_domain_count += !nosharp;
    inject = false;
#if OPAL_ENABLE_DEBUG
    inject = (cm->fail_domain_index == cm->domains_created++);
#endif
    OPAL_THREAD_UNLOCK(&cm->lock);
    if (inject) {
        UCC_VERBOSE(1, "injected failure for ucc context create #%d", cm->fail_domain_index);
        if (cm->fail_domain_no_resource && !nosharp) {
            mca_coll_ucc_note_refusal();
        }
        goto cleanup_lib;
    }

    domain = OBJ_NEW(mca_coll_ucc_oob_domain_t);
    if (NULL == domain) {
        goto cleanup_lib;
    }
    /* The context can outlive the user's handle to the bootstrap comm, so the
       domain keeps its own reference to it for the OOB (released in
       mca_coll_ucc_domain_destroy once the context is destroyed).  Intrinsic
       communicators (MPI_COMM_WORLD) are an exception: they live until
       MPI_Finalize and are torn down with OBJ_DESTRUCT, after the instance
       finalize hook has released this domain -- taking/dropping a reference
       on a communicator while it is being destructed is both unnecessary (it
       is never freed early) and unsafe, so we simply borrow it. */
    if (!OMPI_COMM_IS_INTRINSIC(comm)) {
        OBJ_RETAIN(comm);
    }
    domain->comm     = comm;
    domain->refcount = 1;
    domain->active   = 0;
    memset(domain->team_ids, 0, sizeof(domain->team_ids));
    domain->orphaned = false;
    domain->quarantined = 0;
    domain->nosharp  = nosharp;

    ctx_params.mask          = UCC_CONTEXT_PARAM_FIELD_OOB;
    ctx_params.oob.allgather = oob_allgather;
    ctx_params.oob.req_test  = oob_allgather_test;
    ctx_params.oob.req_free  = oob_allgather_free;
    /* coll_info is the domain, not the comm: the OOB callbacks reach the
       retained bootstrap communicator through domain->comm. */
    ctx_params.oob.coll_info = (void*)domain;
    ctx_params.oob.n_oob_eps = ompi_comm_size(comm);
    ctx_params.oob.oob_ep    = ompi_comm_rank(comm);

    if (UCC_OK != ucc_context_config_read(cm->ucc_lib, NULL, &ctx_config)) {
        UCC_ERROR("UCC context config read failed");
        goto cleanup_domain;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", (unsigned)ompi_comm_size(comm));
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_EPS",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_eps");
        goto cleanup_config;
    }

    snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.num_local_peers + 1);
    if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "ESTIMATED_NUM_PPN",
                                            str_buf)) {
        UCC_ERROR("UCC context config modify failed for estimated_num_ppn");
        goto cleanup_config;
    }

    if (ucc_api_major > 1 || (ucc_api_major == 1 && ucc_api_minor >= 6)) {
        snprintf(str_buf, sizeof(str_buf), "%u", opal_process_info.my_local_rank);
        if (UCC_OK != ucc_context_config_modify(ctx_config, NULL, "NODE_LOCAL_ID",
                                                str_buf)) {
            UCC_ERROR("UCC context config modify failed for node_local_id");
            goto cleanup_config;
        }
    }

    if (nosharp) {
        ucc_status = mca_coll_ucc_config_nosharp(ctx_config);
        if (UCC_OK != ucc_status && !(UCC_ERR_NOT_FOUND == ucc_status && cm->sharp_flavor_force)) {
            UCC_ERROR("UCC context config modify failed for tl/sharp: %s", ucc_status_string(ucc_status));
            goto cleanup_config;
        }
    }

    if (UCC_OK != (ucc_status = ucc_context_create(cm->ucc_lib, &ctx_params, ctx_config,
                                                   &domain->ucc_context))) {
        UCC_ERROR("UCC context create failed: %s", ucc_status_string(ucc_status));
        if (UCC_ERR_NO_RESOURCE == ucc_status && !nosharp) {
            mca_coll_ucc_note_refusal();
        }
        goto cleanup_config;
    }
    ucc_context_config_release(ctx_config);

    OPAL_THREAD_LOCK(&cm->lock);
    opal_list_append(&cm->domains, &domain->super);
    if (!cm->progress_registered) {
        opal_progress_register(mca_coll_ucc_progress);
        cm->progress_registered = true;
    }
    OPAL_THREAD_UNLOCK(&cm->lock);

    UCC_VERBOSE(1, "created ucc oob domain %p for comm %p (size %d)%s",
                (void*)domain, (void*)comm, ompi_comm_size(comm), nosharp ? ", sharp disabled" : "");
    *domain_out = domain;
    return OMPI_SUCCESS;

cleanup_config:
    ucc_context_config_release(ctx_config);
cleanup_domain:
    /* Undo the bootstrap-comm reference taken above (non-intrinsic only). */
    if (!OMPI_COMM_IS_INTRINSIC(domain->comm)) {
        OBJ_RELEASE(domain->comm);
    }
    OBJ_RELEASE(domain);
cleanup_lib:
    OPAL_THREAD_LOCK(&cm->lock);
    cm->sharp_domain_count -= !nosharp;
    /* Only undo the library init if no other domain (live or orphaned) keeps it alive. */
    if (0 == --cm->domain_count && 0 == cm->orphans) {
        if (cm->progress_registered) {
            opal_progress_unregister(mca_coll_ucc_progress);
            cm->progress_registered = false;
        }
        ucc_finalize(cm->ucc_lib);
        cm->ucc_lib = NULL;
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
    return OMPI_ERROR;
}

/* Deterministic preference among covering domains: largest bootstrap group, then lowest global cid. */
static bool mca_coll_ucc_domain_preferred(mca_coll_ucc_oob_domain_t *a, mca_coll_ucc_oob_domain_t *b)
{
    int na = ompi_comm_size(a->comm), nb = ompi_comm_size(b->comm);
    ompi_comm_extended_cid_t ca = a->comm->c_contextid, cb = b->comm->c_contextid;

    if (na != nb) {
        return na > nb;
    }
    if (ca.cid_base != cb.cid_base) {
        return ca.cid_base < cb.cid_base;
    }
    return ca.cid_sub.u64 < cb.cid_sub.u64;
}

/* Best live domain of this flavour covering all of comm's ranks (same choice on every rank); takes a reference. */
static mca_coll_ucc_oob_domain_t *mca_coll_ucc_domain_find_covering(ompi_communicator_t *comm, bool nosharp)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *domain, *best = NULL;
    int                        n = ompi_comm_size(comm), *ranks, *out, i;

    if (OMPI_COMM_IS_INTER(comm)) {
        return NULL;
    }
    ranks = malloc(2 * n * sizeof(int));
    if (NULL == ranks) {
        return NULL;
    }
    out = ranks + n;
    for (i = 0; i < n; i++) {
        ranks[i] = i;
    }
    OPAL_THREAD_LOCK(&cm->lock);
    OPAL_LIST_FOREACH(domain, &cm->domains, mca_coll_ucc_oob_domain_t) {
        if (domain->orphaned || domain->nosharp != nosharp ||
            OMPI_COMM_IS_DYNAMIC(domain->comm) || ompi_comm_size(domain->comm) < n ||
            OMPI_SUCCESS != ompi_group_translate_ranks(comm->c_local_group, n, ranks,
                                                       domain->comm->c_local_group, out)) {
            continue;
        }
        for (i = 0; i < n && MPI_UNDEFINED != out[i]; i++) {
        }
        if (i == n && (NULL == best || mca_coll_ucc_domain_preferred(domain, best))) {
            best = domain;
        }
    }
    if (NULL != best) {
        OPAL_THREAD_ADD_FETCH32(&best->refcount, 1);
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
    free(ranks);
    return best;
}

/* Live, usable domain of this flavour bootstrapped by the comm with this extended cid; takes a reference. */
static mca_coll_ucc_oob_domain_t *mca_coll_ucc_domain_lookup(uint64_t cid_base, uint64_t cid_sub,
                                                             bool nosharp)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *domain, *found = NULL;

    OPAL_THREAD_LOCK(&cm->lock);
    OPAL_LIST_FOREACH(domain, &cm->domains, mca_coll_ucc_oob_domain_t) {
        if (!domain->orphaned && domain->nosharp == nosharp &&
            domain->comm->c_contextid.cid_base == cid_base &&
            domain->comm->c_contextid.cid_sub.u64 == cid_sub) {
            OPAL_THREAD_ADD_FETCH32(&domain->refcount, 1);
            found = domain;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&cm->lock);
    return found;
}

/* Effective flavour: SHARP-free contexts exist only when tl/sharp is in the library (or forced). */
static bool mca_coll_ucc_want_nosharp(bool requested)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    return requested && (cm->sharp_in_lib || cm->sharp_flavor_force);
}

/* Collective: all ranks reuse one domain of the flavour, all create one, or all fall back. */
static int mca_coll_ucc_domain_agree(ompi_communicator_t *comm, mca_coll_ucc_module_t *module,
                                     mca_coll_ucc_oob_domain_t *inherited, bool nosharp,
                                     mca_coll_ucc_oob_domain_t **domain_out, bool *created,
                                     bool *twin_ok)
{
    mca_coll_ucc_component_t  *cm    = &mca_coll_ucc_component;
    mca_coll_ucc_oob_domain_t *cover = inherited;    /* already referenced by comm_query */
    uint64_t                   in[9], out[9];
    int                        rc;

    *domain_out = NULL;
    *created    = false;
    *twin_ok    = false;
    if (NULL == cover && cm->domain_reuse) {
        cover = mca_coll_ucc_domain_find_covering(comm, nosharp);
    }
    OPAL_THREAD_LOCK(&cm->lock);
    /* Only a SHARP-capable context can be refused SHARP resources. */
    in[0] = !cm->lib_failed &&
            !(cm->max_domains > 0 && cm->domain_count >= cm->max_domains) &&
            !(!nosharp && cm->refused_at_count > 0 && cm->sharp_domain_count + 1 >= cm->refused_at_count);
    OPAL_THREAD_UNLOCK(&cm->lock);
    in[1] = (NULL != cover) ? cover->comm->c_contextid.cid_base    : UINT64_MAX;
    in[2] = (NULL != cover) ? cover->comm->c_contextid.cid_sub.u64 : UINT64_MAX;
    in[3] = ~in[1];
    in[4] = ~in[2];
    in[5] = nosharp;
    in[6] = !nosharp;
    /* A SHARP-capable context gets a twin only if every rank can make one and wants it. */
    in[7] = mca_coll_ucc_want_nosharp(true);
    in[8] = !cm->derived_sharp;
    rc = ompi_coll_base_allreduce_intra_recursivedoubling(in, out, 9, &ompi_mpi_uint64_t.dt,
                                                          &ompi_mpi_op_min.op, comm, &module->super);
    if (OMPI_SUCCESS != rc) {
        UCC_VERBOSE(1, "no ucc context for comm %p: agreement failed", (void*)comm);
        goto drop_cover;
    }
    *twin_ok = out[7] && out[8];
    if (0 == out[5] && 0 == out[6]) {
        UCC_VERBOSE(1, "no ucc context for comm %p: ranks disagree on ompi_comm_coll_ucc_sharp", (void*)comm);
        goto drop_cover;
    }
    if (UINT64_MAX != out[1] && out[1] == ~out[3] && out[2] == ~out[4]) {
        /* Every rank is covered by the same domain: reuse needs no permission to create. */
        if (NULL == cover || cover->comm->c_contextid.cid_base != out[1] ||
            cover->comm->c_contextid.cid_sub.u64 != out[2]) {
            if (NULL != cover) {
                OPAL_THREAD_ADD_FETCH32(&cover->refcount, -1);
            }
            cover = mca_coll_ucc_domain_lookup(out[1], out[2], nosharp);
            if (NULL == cover) {
                /* Not visible here yet: stay without a domain, the stage agreement falls back. */
                UCC_VERBOSE(1, "agreed covering domain not found locally for comm %p", (void*)comm);
                return OMPI_ERROR;
            }
        }
        UCC_VERBOSE(1, "reusing ucc oob domain %p (comm %p, size %d) for comm %p (size %d)",
                    (void*)cover, (void*)cover->comm, ompi_comm_size(cover->comm),
                    (void*)comm, ompi_comm_size(comm));
        *domain_out = cover;
        return OMPI_SUCCESS;
    }
    if (0 == out[0]) {
        UCC_VERBOSE(1, "no ucc context for comm %p: a rank is at its context cap or backing off",
                    (void*)comm);
        goto drop_cover;
    }
    if (NULL != cover) {
        OPAL_THREAD_ADD_FETCH32(&cover->refcount, -1);
    }
    if (OMPI_SUCCESS != mca_coll_ucc_domain_create(comm, nosharp, domain_out)) {
        return OMPI_ERROR;
    }
    *created = true;
    return OMPI_SUCCESS;

drop_cover:
    if (NULL != cover) {
        OPAL_THREAD_ADD_FETCH32(&cover->refcount, -1);
    }
    return OMPI_ERROR;
}

/* Some peer has no context: this one can never run its destroy barrier. */
static void mca_coll_ucc_domain_orphan(mca_coll_ucc_oob_domain_t *domain)
{
    mca_coll_ucc_component_t *cm = &mca_coll_ucc_component;

    domain->orphaned = true;
    OPAL_THREAD_LOCK(&cm->lock);
    cm->orphans++;
    OPAL_THREAD_UNLOCK(&cm->lock);
    OPAL_THREAD_ADD_FETCH32(&domain->refcount, -1);
}

/* Collective: give a newly created SHARP-capable context a SHARP-free twin. */
static void mca_coll_ucc_twin_acquire(mca_coll_ucc_module_t *module)
{
    ompi_communicator_t       *comm = module->comm;
    mca_coll_ucc_oob_domain_t *twin = NULL;
    bool                       created = false, unused;
    int                        have, all = 0;

    if (OMPI_SUCCESS != mca_coll_ucc_domain_agree(comm, module, NULL, true, &twin, &created, &unused)) {
        twin = NULL;
    }
    have = (NULL != twin);
    if (OMPI_SUCCESS == ompi_coll_base_allreduce_intra_recursivedoubling(&have, &all, 1, &ompi_mpi_int.dt,
                                                                         &ompi_mpi_op_min.op, comm,
                                                                         &module->super) && all) {
        module->twin = twin;
        UCC_VERBOSE(1, "acquired sharp-free twin %p for ucc oob domain %p (comm %p)",
                    (void*)twin, (void*)module->domain, (void*)comm);
        return;
    }
    UCC_VERBOSE(1, "no sharp-free twin for comm %p", (void*)comm);
    if (NULL != twin) {
        if (created) {
            mca_coll_ucc_domain_orphan(twin);
        } else {
            mca_coll_ucc_domain_unref(twin);
        }
    }
}

/* UCC team ep_map backing-array callback: translate a team endpoint (a rank in
   the team's communicator) into a UCC context endpoint by indexing the
   precomputed array of bootstrap-communicator ranks. */
static uint64_t ep_map_array_cb(uint64_t ep, void *cb_ctx)
{
    return (uint64_t)((const int *)cb_ctx)[ep];
}

/*
 * Build the UCC team ep_map for @comm relative to the domain's bootstrap
 * communicator @boot_comm.
 *
 * A UCC context numbers its endpoints by the OOB ep index used when the
 * context was created, which is the process's rank in the bootstrap
 * communicator (see mca_coll_ucc_domain_create: oob_ep = rank in boot_comm).
 * A team must therefore address its members by their bootstrap-communicator
 * rank, not by any global identifier.  Mapping team rank r -> rank of that
 * same process in boot_comm is exactly what ompi_group_translate_ranks gives
 * us.
 *
 * Using the global vpid (as earlier revisions did) happens to work only when
 * the context was bootstrapped over MPI_COMM_WORLD, where vpid == boot_comm
 * rank == context endpoint.  Over a subset bootstrap communicator -- e.g. an
 * MPI Sessions "half" created with MPI_Comm_create_from_group -- the vpid is
 * out of range for the context's endpoint table and the transport crashes
 * (segfault in ucp_tag_send deep under ucc_team_create_test).
 *
 * If the resulting map needs a backing array (an arbitrary, non-strided
 * permutation) it is returned in *array_out, which the caller must keep alive
 * for the team's lifetime and free afterwards; otherwise *array_out is NULL.
 */
static int get_rank_map(struct ompi_communicator_t *comm,
                        struct ompi_communicator_t *boot_comm,
                        ucc_ep_map_t *map, int **array_out)
{
    int  size = ompi_comm_size(comm);
    int *ranks, *boot_ranks;
    int  i, rc, stride, is_strided;

    *array_out  = NULL;
    map->ep_num = size;

    /* The communicator that bootstrapped the context maps onto it identically. */
    if (comm == boot_comm || comm->c_local_group == boot_comm->c_local_group) {
        map->type = UCC_EP_MAP_FULL;
        return OMPI_SUCCESS;
    }

    ranks      = malloc((size_t)size * sizeof(int));
    boot_ranks = malloc((size_t)size * sizeof(int));
    if ((NULL == ranks) || (NULL == boot_ranks)) {
        free(ranks);
        free(boot_ranks);
        UCC_ERROR("failed to allocate ucc ep map translation arrays");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i = 0; i < size; i++) {
        ranks[i] = i;
    }
    /* team rank i -> its rank in the bootstrap communicator (context endpoint) */
    rc = ompi_group_translate_ranks(comm->c_local_group, size, ranks,
                                    boot_comm->c_local_group, boot_ranks);
    free(ranks);
    if (OMPI_SUCCESS != rc) {
        free(boot_ranks);
        return rc;
    }
    for (i = 0; i < size; i++) {
        if (MPI_UNDEFINED == boot_ranks[i]) {
            UCC_ERROR("team rank %d is not in the bootstrap communicator", i);
            free(boot_ranks);
            return OMPI_ERR_BAD_PARAM;
        }
    }

    /* Detect a strided pattern (covers contiguous halves, reversed orders,
       etc.) so we can avoid keeping a backing array around. */
    stride     = (size > 1) ? (boot_ranks[1] - boot_ranks[0]) : 1;
    is_strided = 1;
    for (i = 2; i < size; i++) {
        if (boot_ranks[i] - boot_ranks[i - 1] != stride) {
            is_strided = 0;
            break;
        }
    }

    if (is_strided && 0 == boot_ranks[0] && 1 == stride && size == ompi_comm_size(boot_comm)) {
        map->type = UCC_EP_MAP_FULL;           /* e.g. dup of the bootstrap comm */
        free(boot_ranks);
        return OMPI_SUCCESS;
    }
    if (is_strided) {
        map->type           = UCC_EP_MAP_STRIDED;
        map->strided.start  = (uint64_t)boot_ranks[0];
        map->strided.stride = (int64_t)stride;
        free(boot_ranks);
        return OMPI_SUCCESS;
    }

    /* Arbitrary permutation: address the context through the translation array
       (kept alive by the caller via *array_out). */
    map->type      = UCC_EP_MAP_CB;
    map->cb.cb     = ep_map_array_cb;
    map->cb.cb_ctx = (void *)boot_ranks;
    *array_out     = boot_ranks;
    return OMPI_SUCCESS;
}

#define UCC_INSTALL_COLL_API(__comm, __ucc_module, __COLL, __api)                                                                          \
    do                                                                                                                                     \
    {                                                                                                                                      \
        if ((mca_coll_ucc_component.ucc_lib_attr.coll_types & UCC_COLL_TYPE_##__COLL))                                                     \
        {                                                                                                                                  \
            if (mca_coll_ucc_component.cts_requested & UCC_COLL_TYPE_##__COLL)                                                             \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, __api, (__ucc_module)->previous_##__api, (__ucc_module)->previous_##__api##_module, "ucc");      \
                MCA_COLL_INSTALL_API(__comm, __api, mca_coll_ucc_##__api, &__ucc_module->super, "ucc");                                    \
                (__ucc_module)->super.coll_##__api = mca_coll_ucc_##__api;                                                                 \
            }                                                                                                                              \
            if (mca_coll_ucc_component.nb_cts_requested & UCC_COLL_TYPE_##__COLL)                                                          \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, i##__api, (__ucc_module)->previous_i##__api, (__ucc_module)->previous_i##__api##_module, "ucc"); \
                MCA_COLL_INSTALL_API(__comm, i##__api, mca_coll_ucc_i##__api, &__ucc_module->super, "ucc");                                \
                (__ucc_module)->super.coll_i##__api = mca_coll_ucc_i##__api;                                                               \
            }                                                                                                                              \
            if (mca_coll_ucc_component.ps_cts_requested & UCC_COLL_TYPE_##__COLL)                                                          \
            {                                                                                                                              \
                MCA_COLL_SAVE_API(__comm, __api##_init, (__ucc_module)->previous_##__api##_init, (__ucc_module)->previous_##__api##_init_module, "ucc"); \
                MCA_COLL_INSTALL_API(__comm, __api##_init, mca_coll_ucc_##__api##_init, &__ucc_module->super, "ucc");                      \
                (__ucc_module)->super.coll_##__api##_init = mca_coll_ucc_##__api##_init;                                                   \
            }                                                                                                                              \
        }                                                                                                                                  \
    } while (0)

static int mca_coll_ucc_replace_coll_handlers(mca_coll_ucc_module_t *ucc_module)
{
    ompi_communicator_t *comm = ucc_module->comm;

    UCC_INSTALL_COLL_API(comm, ucc_module, ALLREDUCE, allreduce);
    UCC_INSTALL_COLL_API(comm, ucc_module, BARRIER, barrier);
    UCC_INSTALL_COLL_API(comm, ucc_module, BCAST, bcast);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLTOALL, alltoall);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLTOALLV, alltoallv);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLGATHER, allgather);
    UCC_INSTALL_COLL_API(comm, ucc_module, ALLGATHERV, allgatherv);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE, reduce);

    UCC_INSTALL_COLL_API(comm, ucc_module, GATHER, gather);
    UCC_INSTALL_COLL_API(comm, ucc_module, GATHERV, gatherv);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE_SCATTER, reduce_scatter_block);
    UCC_INSTALL_COLL_API(comm, ucc_module, REDUCE_SCATTERV, reduce_scatter);
    UCC_INSTALL_COLL_API(comm, ucc_module, SCATTER, scatter);
    UCC_INSTALL_COLL_API(comm, ucc_module, SCATTERV, scatterv);

    return OMPI_SUCCESS;
}

/*
 * Initialize module on the communicator
 * Enables blocking creations now; others defer to their first blocking collective.
 */
static int mca_coll_ucc_module_enable(mca_coll_base_module_t *module,
                                      struct ompi_communicator_t *comm)
{
    mca_coll_ucc_component_t  *cm         = &mca_coll_ucc_component;
    mca_coll_ucc_module_t     *ucc_module = (mca_coll_ucc_module_t *)module;
    int                        rc;

    OPAL_THREAD_LOCK(&cm->lock);
    if (cm->lib_failed || (NULL == cm->ucc_lib && OMPI_SUCCESS != mca_coll_ucc_lib_init())) {
        cm->lib_failed = true;
        OPAL_THREAD_UNLOCK(&cm->lock);
        rc = OMPI_ERROR;
        goto drop_domain;
    }
    /* Registered until coll_module_disable: the sweep and derived comms find the module here. */
    ucc_module->modules_idx = opal_pointer_array_add(&cm->modules, ucc_module);
    OPAL_THREAD_UNLOCK(&cm->lock);
    ucc_module->state = MCA_COLL_UCC_PENDING;
    mca_coll_ucc_replace_coll_handlers(ucc_module);
    /* Nonblocking creation (e.g. idup): enable at the first blocking collective instead. */
    ucc_module->lazy = mca_coll_base_comm_select_nonblocking(comm);
    if (!ucc_module->lazy) {
        (void) mca_coll_ucc_lazy_enable(ucc_module);
    }
    return OMPI_SUCCESS;

drop_domain:
    /* Never registered: no release hook will drop this. */
    if (NULL != ucc_module->domain) {
        OPAL_THREAD_ADD_FETCH32(&ucc_module->domain->refcount, -1);
        ucc_module->domain = NULL;
    }
    return rc;
}

/* Collective over comm: every rank ends up using UCC for it, or none does. */
int mca_coll_ucc_lazy_enable(mca_coll_ucc_module_t *ucc_module)
{
    mca_coll_ucc_component_t  *cm         = &mca_coll_ucc_component;
    ompi_communicator_t       *comm       = ucc_module->comm;
    mca_coll_ucc_oob_domain_t *domain     = ucc_module->domain;
    bool                       created    = false, twin_ok = false;
    bool                       inject, no_team_id = false;
    int                        stage = 0, agreed = 0;
    double                     t0 = 0.0, t_domain = 0.0, t_post = 0.0, t_wait = 0.0;
    double                     t_agree = 0.0, deadline = 0.0;
    ucc_status_t               status;
    ucc_team_params_t          team_params;

    /* Only PENDING may enable: INITIALIZING is a re-entry, DISABLED a failed enable. */
    if (MCA_COLL_UCC_PENDING != ucc_module->state) {
        return OMPI_ERROR;
    }
    ucc_module->state = MCA_COLL_UCC_INITIALIZING;
    if (cm->ucc_verbose >= 3) {
        t0 = ompi_wtime();
    }
    /* Agree on a domain when none is inherited, the comm is lazy, or the info key sets the flavour. */
    if (NULL == domain || ucc_module->lazy ||
        (ucc_module->sharp_key && mca_coll_ucc_want_nosharp(true))) {
        if (OMPI_SUCCESS != mca_coll_ucc_domain_agree(comm, ucc_module, domain,
                                                      mca_coll_ucc_want_nosharp(ucc_module->nosharp),
                                                      &domain, &created, &twin_ok)) {
            domain = NULL;
        }
        ucc_module->domain = domain;
    }
    if (t0 > 0.0) {
        t_domain = ompi_wtime();
    }
    if (NULL != domain) {
        /* Team create is collective over this communicator and addresses into the
           (possibly shared) context through an ep map; it does not run the OOB, so
           it needs no per-team OOB and works even when this communicator is a
           subset or reordering of the domain's bootstrap communicator.  The ep map
           must translate team ranks into context endpoints, i.e. into ranks of the
           domain's bootstrap communicator (see get_rank_map). */
        stage = 1;
        team_params.mask     = UCC_TEAM_PARAM_FIELD_EP_MAP |
                               UCC_TEAM_PARAM_FIELD_EP     |
                               UCC_TEAM_PARAM_FIELD_EP_RANGE;
        team_params.ep       = ompi_comm_rank(comm);
        team_params.ep_range = UCC_COLLECTIVE_EP_RANGE_CONTIG;
        /* Global-index cids and hashed pset-comm ids share the context's team id bitmap. */
        {
            int cand = OMPI_COMM_IS_GLOBAL_INDEX(comm) ? (int)ompi_comm_get_local_cid(comm)
                                                        : mca_coll_ucc_team_id_candidate(comm);
#if OPAL_ENABLE_DEBUG
            if (cm->team_id_force >= 0 && !OMPI_COMM_IS_GLOBAL_INDEX(comm)) {
                cand = cm->team_id_force;
            }
#endif
            /* A global index is c_index: bound it before it indexes the bitmap. */
            if (cand < 0 || cand > UCC_EXT_TEAM_ID_MAX) {
                UCC_VERBOSE(1, "team id %d outside the ucc external range [0, %d]: no ucc for comm %p",
                            cand, UCC_EXT_TEAM_ID_MAX, (void*)comm);
                no_team_id = true;
            } else if (!mca_coll_ucc_team_id_take(domain, cand)) {
                UCC_VERBOSE(1, "team id %d already live on domain %p: no ucc for comm %p",
                            cand, (void*)domain, (void*)comm);
                no_team_id = true;
            } else {
                ucc_module->team_id = cand;
                team_params.mask   |= UCC_TEAM_PARAM_FIELD_ID;
                team_params.id      = cand;
            }
        }
        UCC_VERBOSE(2, "creating ucc_team for comm %p, comm_size %d", (void*)comm, ompi_comm_size(comm));
        inject = false;
#if OPAL_ENABLE_DEBUG
        OPAL_THREAD_LOCK(&cm->lock);
        inject = (cm->fail_team_index == cm->teams_posted++) &&
                 (cm->fail_team_rank < 0 || (uint32_t)cm->fail_team_rank == OMPI_PROC_MY_NAME->vpid);
        OPAL_THREAD_UNLOCK(&cm->lock);
#endif
        if (no_team_id) {
            /* stage stays 1: the agreement below turns this comm over to the previous modules */
        } else if (OMPI_SUCCESS != get_rank_map(comm, domain->comm, &team_params.ep_map,
                                                &ucc_module->ep_map_ranks)) {
            UCC_ERROR("ucc ep map construction failed");
        } else if (inject) {
            UCC_VERBOSE(1, "injected failure for ucc team create post #%d", cm->fail_team_index);
        } else if (UCC_OK != ucc_team_create_post(&domain->ucc_context, 1,
                                                  &team_params, &ucc_module->ucc_team)) {
            UCC_ERROR("ucc_team_create_post failed");
            ucc_module->ucc_team = NULL;
        }
    }
    if (t0 > 0.0) {
        t_post = ompi_wtime();
    }

    if (cm->team_post_agreement) {
        /* Abandon the team on every rank unless all ranks posted it. */
        int posted = (NULL != ucc_module->ucc_team), all_posted = 0;
        ompi_coll_base_allreduce_intra_recursivedoubling(&posted, &all_posted, 1, &ompi_mpi_int.dt,
                                                         &ompi_mpi_op_min.op, comm, &ucc_module->super);
        if (!all_posted && NULL != ucc_module->ucc_team) {
            mca_coll_ucc_team_abandon(ucc_module, "a peer failed to post");
        }
    }
    if (NULL != ucc_module->ucc_team) {
        /* UCC cannot report a peer's failed post: bound the wait; the agreement below falls back. */
        if (cm->team_create_timeout > 0) {
            deadline = ompi_wtime() + cm->team_create_timeout;
        }
        while (UCC_INPROGRESS == (status = ucc_team_create_test(ucc_module->ucc_team))) {
            ucc_context_progress(domain->ucc_context);
            opal_progress();
            if (deadline > 0.0 && ompi_wtime() > deadline) {
                status = UCC_ERR_TIMED_OUT;
                break;
            }
        }
        if (UCC_ERR_TIMED_OUT == status) {
            char why[64];
            snprintf(why, sizeof(why), "create not complete after %d s", cm->team_create_timeout);
            mca_coll_ucc_team_abandon(ucc_module, why);
        } else if (UCC_OK != status) {
            UCC_ERROR("ucc_team_create_test failed: %s", ucc_status_string(status));
        } else {
            stage = 2;
        }
    }

    if (t0 > 0.0) {
        t_wait = ompi_wtime();
    }
    ompi_coll_base_allreduce_intra_recursivedoubling(&stage, &agreed, 1, &ompi_mpi_int.dt,
                                                     &ompi_mpi_op_min.op, comm, &ucc_module->super);
    if (t0 > 0.0) {
        t_agree = ompi_wtime();
        UCC_VERBOSE(3, "enable timing for comm %p (size %d): domain %.1f us, post %.1f us, "
                    "wait %.1f us, agree %.1f us",
                    (void*)comm, ompi_comm_size(comm), (t_domain - t0) * 1e6, (t_post - t_domain) * 1e6,
                    (t_wait - t_post) * 1e6, (t_agree - t_wait) * 1e6);
    }
    if (2 == agreed) {
        if (created && !domain->nosharp && twin_ok) {
            mca_coll_ucc_twin_acquire(ucc_module);
        }
        ucc_module->state = MCA_COLL_UCC_READY;
        ucc_module->lazy  = false;    /* nonblocking posts use the team from here on */
        return OMPI_SUCCESS;
    }

    UCC_VERBOSE(1, "ucc disabled for comm %p: enable stage %d (agreed %d)", (void*)comm, stage, agreed);
    if (NULL != ucc_module->ucc_team) {
        mca_coll_ucc_team_abandon(ucc_module, "peers did not reach the same stage");
    }
    mca_coll_ucc_team_id_release(ucc_module);
    if (NULL != domain) {
        if (created && 0 == agreed) {
            mca_coll_ucc_domain_orphan(domain);
        } else {
            mca_coll_ucc_domain_unref(domain);
        }
        ucc_module->domain = NULL;
    }
    ucc_module->state = MCA_COLL_UCC_DISABLED;
    return OMPI_ERROR;
}

#define UCC_UNINSTALL_COLL_API(__comm, __ucc_module, __api)                                                                          \
    do                                                                                                                               \
    {                                                                                                                                \
        if (&(__ucc_module)->super == (__comm)->c_coll->coll_##__api##_module)                                                       \
        {                                                                                                                            \
            MCA_COLL_INSTALL_API(__comm, __api, (__ucc_module)->previous_##__api, (__ucc_module)->previous_##__api##_module, "ucc"); \
            (__ucc_module)->previous_##__api = NULL;                                                                                 \
            (__ucc_module)->previous_##__api##_module = NULL;                                                                        \
        }                                                                                                                            \
    } while (0)

/**
 * The disable will be called once per collective module, in the reverse order
 * in which enable has been called. This reverse order allows the module to properly
 * unregister the collective function pointers they provide for the communicator.
 */
static int
mca_coll_ucc_module_disable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allreduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallreduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, barrier);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ibarrier);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, bcast);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ibcast);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoall);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ialltoall);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoallv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ialltoallv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallgather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iallgatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, igather);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, igatherv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_block);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce_scatter_block);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, ireduce_scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iscatter);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatterv);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, iscatterv);

    UCC_UNINSTALL_COLL_API(comm, ucc_module, allreduce_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, barrier_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, bcast_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoall_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, alltoallv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgather_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, allgatherv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gather_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, gatherv_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_block_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, reduce_scatter_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatter_init);
    UCC_UNINSTALL_COLL_API(comm, ucc_module, scatterv_init);

    mca_coll_ucc_module_teardown(ucc_module);
    return OMPI_SUCCESS;
}


/* This component's module on comm, found on the communicator's own module list. */
static mca_coll_ucc_module_t *mca_coll_ucc_module_find(ompi_communicator_t *comm)
{
    mca_coll_base_avail_coll_t *avail;

    if (NULL == comm->c_coll || NULL == comm->c_coll->module_list) {
        return NULL;
    }
    OPAL_LIST_FOREACH(avail, comm->c_coll->module_list, mca_coll_base_avail_coll_t) {
        if (avail->ac_module->coll_module_enable == mca_coll_ucc_module_enable) {
            return (mca_coll_ucc_module_t *)avail->ac_module;
        }
    }
    return NULL;
}

/* True if the info key holds a boolean (returned in *on); *present if it is set at all. */
static bool mca_coll_ucc_sharp_info(ompi_communicator_t *comm, bool *present, bool *on)
{
    opal_cstring_t *str;
    int             flag = 0, rc;

    *present = false;
    if (NULL == comm->super.s_info) {
        return false;
    }
    rc = opal_info_get_bool(comm->super.s_info, MCA_COLL_UCC_SHARP_KEY, on, &flag);
    if (!flag) {
        return false;
    }
    *present = true;
    if (OPAL_SUCCESS == rc) {
        return true;
    }
    if (OPAL_SUCCESS == opal_info_get(comm->super.s_info, MCA_COLL_UCC_SHARP_KEY, &str, &flag) && flag) {
        if (0 != strcasecmp(str->string, "default") && 0 != strcasecmp(str->string, "auto")) {
            UCC_VERBOSE(1, "comm %p: %s=%s is not a boolean, using the default",
                        (void*)comm, MCA_COLL_UCC_SHARP_KEY, str->string);
        }
        OBJ_RELEASE(str);
    }
    return false;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_ucc_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_ucc_component_t  *cm = &mca_coll_ucc_component;
    mca_coll_ucc_module_t     *ucc_module;
    mca_coll_ucc_oob_domain_t *domain = NULL;
    mca_coll_ucc_module_t     *parent_module = NULL;
    ompi_communicator_t       *parent;
    bool                       nosharp, on, key, sharp_explicit = false;
    int                        cmp;
    const char                *source;
    *priority = 0;

    if (!cm->ucc_enable){
        return NULL;
    }

    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < cm->ucc_np
        || ompi_comm_size(comm) < 2){
        return NULL;
    }

    /*
     * Domain discovery.  If the parent has a UCC module with an OOB domain,
     * inherit it: this communicator is a subset/reordering of its parent, so
     * the ep map makes it addressable in the parent's context and the whole
     * family shares one heavyweight context.  Otherwise lazy_enable agrees
     * on reusing a covering domain or bootstrapping a fresh one here.  Only
     * a parent domain (or its SHARP-free twin) of the SHARP flavour chosen
     * below is inherited.
     */
    parent = mca_coll_base_comm_select_parent(comm);
    if (NULL != parent && parent != comm) {
        parent_module = mca_coll_ucc_module_find(parent);
    }

    /* SHARP choice: WORLD on, else info key, else explicit value inherited over the same group, else default. */
    if (comm == &ompi_mpi_comm_world.comm) {
        nosharp = false;
        key     = false;
        source  = "world";
    } else if (mca_coll_ucc_sharp_info(comm, &key, &on)) {
        nosharp        = !on;
        sharp_explicit = true;
        source         = "info";
    } else if (NULL != parent_module && parent_module->sharp_explicit &&
               OMPI_SUCCESS == ompi_group_compare(comm->c_local_group, parent->c_local_group, &cmp) &&
               MPI_IDENT == cmp) {
        nosharp        = parent_module->nosharp;
        sharp_explicit = true;
        source         = "inherited";
    } else {
        nosharp = !cm->derived_sharp;
        source  = "default";
    }
    UCC_VERBOSE(2, "comm %p: sharp %s (%s)", (void*)comm, nosharp ? "off" : "on", source);

    if (NULL != parent_module) {
        bool want = mca_coll_ucc_want_nosharp(nosharp);
        if (NULL != parent_module->domain && parent_module->domain->nosharp == want) {
            domain = parent_module->domain;
        } else if (NULL != parent_module->twin && parent_module->twin->nosharp == want) {
            domain = parent_module->twin;
        }
        if (NULL != domain) {
            OPAL_THREAD_ADD_FETCH32(&domain->refcount, 1);
        }
    }

    ucc_module = OBJ_NEW(mca_coll_ucc_module_t);
    if (!ucc_module) {
        if (NULL != domain) {
            OPAL_THREAD_ADD_FETCH32(&domain->refcount, -1);
        }
        return NULL;
    }
    ucc_module->comm                      = comm;
    ucc_module->domain                    = domain;
    ucc_module->nosharp                   = nosharp;
    ucc_module->sharp_explicit            = sharp_explicit;
    ucc_module->sharp_key                 = key;
    ucc_module->super.coll_module_enable  = mca_coll_ucc_module_enable;
    ucc_module->super.coll_module_disable = mca_coll_ucc_module_disable;
    *priority                             = cm->ucc_priority;

    return &ucc_module->super;
}


OBJ_CLASS_INSTANCE(mca_coll_ucc_module_t,
                   mca_coll_base_module_t,
                   mca_coll_ucc_module_construct,
                   mca_coll_ucc_module_destruct);

OBJ_CLASS_INSTANCE(mca_coll_ucc_req_t, ompi_request_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(mca_coll_ucc_oob_domain_t, opal_list_item_t,
                   NULL, NULL);

int mca_coll_ucc_req_free(struct ompi_request_t **ompi_req)
{
    {
        mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t *) ompi_req[0];
        /* Freed before completion: its post failed. */
        if (!coll_req->super.req_persistent && !REQUEST_COMPLETE(&coll_req->super)) {
            mca_coll_ucc_req_account(coll_req, -1);
            coll_req->module = NULL;
        }
        if (true == coll_req->super.req_persistent) {
            UCC_VERBOSE(5, "%s free %p", "<coll>_init", (void *)coll_req);
            if (NULL != coll_req->ucc_req) {
                ucc_status_t rc_ucc;
                rc_ucc = ucc_collective_finalize(coll_req->ucc_req);
                if (UCC_OK != rc_ucc) {
                    UCC_ERROR("ucc_collective_finalize failed: %s", ucc_status_string(rc_ucc));
                }
            }
        }
    }
    /* Reset the request to the invalid state (and drop any f2c handle)
       before handing it back to the free list.  Without this the item is
       returned still marked active/inactive, and ompi_request_destruct()
       asserts on it when the free list is torn down at component close. */
    OMPI_REQUEST_FINI(*ompi_req);
    opal_free_list_return (&mca_coll_ucc_component.requests,
                           (opal_free_list_item_t *)(*ompi_req));
    *ompi_req = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}


void mca_coll_ucc_completion(void *data, ucc_status_t status)
{
    mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t*)data;

    if (UCC_OK != status) {
        UCC_ERROR("ucc collective completed with %s",
                  ucc_status_string(status));
        coll_req->super.req_status.MPI_ERROR = MPI_ERR_OTHER;
    }
    if (false == coll_req->super.req_persistent) {
        ucc_collective_finalize(coll_req->ucc_req);
    } else {
        UCC_VERBOSE(5, "%s done %p", "<coll>_init", (void *)coll_req);
        assert(!REQUEST_COMPLETE(&coll_req->super));
    }
    /* Only now does the operation stop using the team: a free draining ->active may destroy it. */
    mca_coll_ucc_req_account(coll_req, -1);
    ompi_request_complete(&coll_req->super, true);
}

/* req_start() : ompi_request_start_fn_t */
int mca_coll_ucc_req_start(size_t count, struct ompi_request_t **requests)
{
    size_t ii;
    int rc = OMPI_SUCCESS;

    for (ii = 0; ii < count; ++ii) {
        mca_coll_ucc_req_t *coll_req = (mca_coll_ucc_req_t *) requests[ii];
        ucc_status_t rc_ucc;

        if ((NULL == coll_req) || (OMPI_REQUEST_COLL != coll_req->super.req_type)) {
            continue;
        }
        if (true != coll_req->super.req_persistent) {
            coll_req->super.req_status.MPI_ERROR = MPI_ERR_REQUEST;
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERROR;
            }
            continue;
        }
        UCC_VERBOSE(5, "%s post %p", "<coll>_init", (void *)coll_req);
        assert(REQUEST_COMPLETE(&coll_req->super));
        assert(OMPI_REQUEST_INACTIVE == coll_req->super.req_state);

        coll_req->super.req_status.MPI_TAG = MPI_ANY_TAG;
        coll_req->super.req_status.MPI_ERROR = OMPI_SUCCESS;
        coll_req->super.req_status._cancelled = 0;
        coll_req->super.req_complete = REQUEST_PENDING;
        coll_req->super.req_state = OMPI_REQUEST_ACTIVE;

        mca_coll_ucc_req_account(coll_req, 1);
        rc_ucc = ucc_collective_post(coll_req->ucc_req);
        if (UCC_OK != rc_ucc) {
            UCC_ERROR("ucc_collective_post failed: %s", ucc_status_string(rc_ucc));
            mca_coll_ucc_req_account(coll_req, -1);
            coll_req->super.req_complete = REQUEST_COMPLETED;
            coll_req->super.req_status.MPI_ERROR = MPI_ERR_OTHER;
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERROR;
            }
            continue;
        }
    }

    return rc;
}
