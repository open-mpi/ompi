/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2022 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include "opal/mca/btl/sm/btl_sm.h"
#include "opal/mca/btl/sm/btl_sm_fbox.h"
#include "opal/mca/btl/sm/btl_sm_fifo.h"
#include "opal/mca/btl/sm/btl_sm_frag.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/util/argv.h"

#include <stdlib.h>
#include <string.h>

static int sm_del_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers);

static int sm_register_error_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_module_error_cb_fn_t cbfunc);

static int sm_finalize(struct mca_btl_base_module_t *btl);

static struct mca_btl_base_descriptor_t *sm_prepare_src(struct mca_btl_base_module_t *btl,
                                                        struct mca_btl_base_endpoint_t *endpoint,
                                                        struct opal_convertor_t *convertor,
                                                        uint8_t order, size_t reserve, size_t *size,
                                                        uint32_t flags);

static int sm_add_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                        struct opal_bitmap_t *status);

static int init_sm_endpoint(struct mca_btl_base_endpoint_t **ep_out, struct opal_proc_t *proc);

static int sm_map_local_procs(void);

mca_btl_sm_t mca_btl_sm = {
    {&mca_btl_sm_component.super, .btl_add_procs = sm_add_procs, .btl_del_procs = sm_del_procs,
     .btl_finalize = sm_finalize, .btl_alloc = mca_btl_sm_alloc, .btl_free = mca_btl_sm_free,
     .btl_prepare_src = sm_prepare_src, .btl_send = mca_btl_sm_send, .btl_sendi = mca_btl_sm_sendi,
     .btl_dump = mca_btl_base_dump, .btl_register_error = sm_register_error_cb}};

static int sm_btl_first_time_init(mca_btl_sm_t *sm_btl, int n)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    int rc;

    /* generate the endpoints. They stay unpublished until every local
     * peer has been mapped -- see mca_btl_sm_attach_local_peers(). */
    component->endpoints_storage = (struct mca_btl_base_endpoint_t *)
        calloc(n + 1, sizeof(struct mca_btl_base_endpoint_t));
    if (NULL == component->endpoints_storage) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    component->endpoints_storage[n].peer_smp_rank = -1;

    component->local_procs = (opal_proc_t **) calloc(n + 1, sizeof(opal_proc_t *));
    if (NULL == component->local_procs) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    component->fbox_in_endpoints = calloc(n + 1, sizeof(void *));
    if (NULL == component->fbox_in_endpoints) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    component->mpool = mca_mpool_basic_create((void *) (component->my_segment
                                                        + MCA_BTL_SM_FIFO_SIZE),
                                              (unsigned long) (mca_btl_sm_component.segment_size
                                                               - MCA_BTL_SM_FIFO_SIZE),
                                              64);
    if (NULL == component->mpool) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* Fast box buffers are prepended with a metadata section. */
    rc = opal_free_list_init(&component->sm_fboxes, sizeof(opal_free_list_item_t), 8,
                             OBJ_CLASS(opal_free_list_item_t), mca_btl_sm_component.fbox_size +
                             sizeof (mca_btl_sm_fbox_metadata_t),
                             opal_cache_line_size, 0, mca_btl_sm_component.fbox_max, 4,
                             component->mpool, 0, NULL, NULL, NULL);
    if (OPAL_SUCCESS != rc) {
        goto cleanup;
    }

    /* initialize fragment descriptor free lists */
    /* initialize free list for small send and inline fragments */
    rc = opal_free_list_init(&component->sm_frags_user, sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                             mca_btl_sm_component.max_inline_send + sizeof(mca_btl_sm_hdr_t),
                             opal_cache_line_size, component->sm_free_list_num,
                             component->sm_free_list_max, component->sm_free_list_inc,
                             component->mpool, 0, NULL, mca_btl_sm_frag_init,
                             &component->sm_frags_user);
    if (OPAL_SUCCESS != rc) {
        goto cleanup;
    }

    /* initialize free list for buffered send fragments */
    rc = opal_free_list_init(&component->sm_frags_eager, sizeof(mca_btl_sm_frag_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                             mca_btl_sm.super.btl_eager_limit + sizeof(mca_btl_sm_hdr_t),
                             opal_cache_line_size, component->sm_free_list_num,
                             component->sm_free_list_max, component->sm_free_list_inc,
                             component->mpool, 0, NULL, mca_btl_sm_frag_init,
                             &component->sm_frags_eager);
    if (OPAL_SUCCESS != rc) {
        goto cleanup;
    }

    if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
        /* initialize free list for buffered send fragments */
        rc = opal_free_list_init(&component->sm_frags_max_send, sizeof(mca_btl_sm_frag_t),
                                 opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag_t),
                                 mca_btl_sm.super.btl_max_send_size + sizeof(mca_btl_sm_hdr_t),
                                 opal_cache_line_size, component->sm_free_list_num,
                                 component->sm_free_list_max, component->sm_free_list_inc,
                                 component->mpool, 0, NULL, mca_btl_sm_frag_init,
                                 &component->sm_frags_max_send);
        if (OPAL_SUCCESS != rc) {
            goto cleanup;
        }
    }

    /* set flag indicating btl has been inited. A thread that only tests
     * the flag must see everything allocated above, starting with
     * component->endpoints_storage. */
    opal_atomic_wmb();
    sm_btl->btl_inited = 1;

    return OPAL_SUCCESS;

cleanup:
    /* sm_finalize() gives up on a btl that never inited, and component
     * close only knows about the free lists, the lock and the mpool, so
     * everything taken here has to go back now.
     *
     * The free lists belong to the component (constructed on open,
     * destructed on close), so reset them rather than release them: a
     * partially initialized list holds items carved out of the mpool, and
     * releasing those items is what a destruct does. Reconstructing also
     * clears fl_mpool, so the destruct at component close does not reach
     * into the mpool finalized just below. */
    OBJ_DESTRUCT(&component->sm_frags_max_send);
    OBJ_CONSTRUCT(&component->sm_frags_max_send, opal_free_list_t);
    OBJ_DESTRUCT(&component->sm_frags_eager);
    OBJ_CONSTRUCT(&component->sm_frags_eager, opal_free_list_t);
    OBJ_DESTRUCT(&component->sm_frags_user);
    OBJ_CONSTRUCT(&component->sm_frags_user, opal_free_list_t);
    OBJ_DESTRUCT(&component->sm_fboxes);
    OBJ_CONSTRUCT(&component->sm_fboxes, opal_free_list_t);

    if (NULL != component->mpool) {
        component->mpool->mpool_finalize(component->mpool);
        component->mpool = NULL;
    }

    free(component->fbox_in_endpoints);
    component->fbox_in_endpoints = NULL;
    free(component->local_procs);
    component->local_procs = NULL;
    component->local_procs_mapped = false;
    free(component->endpoints_storage);
    component->endpoints_storage = NULL;
    component->endpoints = NULL;

    return rc;
}

/* A Get miss is two different answers, and the peer's own state is what
 * tells them apart.
 *
 * While that peer's data can still arrive, a miss may be a GDS race: it
 * has committed, but this Get ran before the node server had the key. So
 * the answer is NOT_READY and BML/PML come back.
 *
 * Once the data is local -- the fence landed, or the fetch for this peer
 * answered -- it has committed and everything it published is here, which
 * is what OPAL_PROC_FLAG_AVAILABLE promises its readers: a key missing
 * from the answer is missing for good rather than merely late. A peer
 * publishing no shared memory of its own is a peer that does not use this
 * btl, which is a final no. Answering NOT_READY to that would be a retry
 * with nothing left to wait for, and nothing above bounds it: the peer is
 * never declared unreachable, so a send to it is parked and re-driven from
 * every progress tick for the life of the job.
 *
 * There is a third reading, which ends the same way and has to be told
 * apart all the same: the fetch for that peer failed, so its keys are
 * being read as absent only because there was nothing to read. No
 * shared memory can be attached either way, and this btl is all of the
 * node or none of it, so the answer is still a final no -- but it is a
 * no about the runtime and not about this peer's shared memory, and
 * whoever ends up reporting the peer unreachable should not offer the
 * usual advice about which btls to select.
 *
 * An OPAL status, as everything reaching here is: what a PMIx Get reports
 * is converted where it enters, since the two number their errors from the
 * same small negatives and mean different things by them -- PMIx spells
 * "not found" -46, which is OPAL_ERR_TAKE_NEXT_OPTION. */
static int sm_modex_not_ready(const struct opal_proc_t *proc, int rc)
{
    if (OPAL_ERR_NOT_READY == rc) {
        return OPAL_ERR_NOT_READY;
    }
    if (OPAL_ERR_NOT_FOUND != rc) {
        return rc;
    }
    if (!opal_proc_known(proc, OPAL_PROC_FLAG_AVAILABLE)) {
        return OPAL_ERR_NOT_READY;
    }

    /* Final either way; which of the two only decides what is said about
     * it. FETCH_FAILED is never set on its own, so it needs no second
     * look at the flag above. */
    if (opal_proc_known(proc, OPAL_PROC_FLAG_FETCH_FAILED)) {
        BTL_VERBOSE(("no shared memory for peer %s: nothing this peer published was ever "
                     "fetched, so this btl is unavailable for the whole node",
                     OPAL_NAME_PRINT(proc->proc_name)));
    } else {
        BTL_VERBOSE(("peer %s published no shared memory of its own, so it does not use "
                     "this btl and neither can the rest of the node",
                     OPAL_NAME_PRINT(proc->proc_name)));
    }

    return OPAL_ERR_NOT_FOUND;
}

/*
 * Which SMP local rank is this peer? The map sm_ensure_inited() built
 * answers without going back to the runtime; only a peer that was
 * missing from it has to be asked about.
 */
static int sm_local_rank_of(struct opal_proc_t *proc, uint16_t *local_rank)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    uint16_t *ptr = local_rank;
    int rc;

    if (NULL != component->local_procs) {
        for (uint16_t lr = 0; lr <= (uint16_t) MCA_BTL_SM_NUM_LOCAL_PEERS; ++lr) {
            if (proc == component->local_procs[lr]) {
                *local_rank = lr;
                return OPAL_SUCCESS;
            }
        }
    }

    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_RANK, &proc->proc_name, &ptr, PMIX_UINT16);

    /* That macro reports what PMIx said, unlike its OPAL_MODEX_RECV_LOCAL
     * neighbour, and this status is about to leave the file. Convert it
     * here so that everything downstream reads one set of codes: the two
     * overlap numerically and disagree on what the numbers mean. */
    return opal_pmix_convert_status(rc);
}

static int init_sm_endpoint(struct mca_btl_base_endpoint_t **ep_out, struct opal_proc_t *proc)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_sm_modex_t *modex = NULL;
    char *segment_base;
    size_t msg_size;
    int rc;

    uint16_t peer_local_rank;
    rc = sm_local_rank_of(proc, &peer_local_rank);
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("could not read the local rank for peer. rc=%d", rc));
        return sm_modex_not_ready(proc, rc);
    }

    mca_btl_base_endpoint_t *ep = component->endpoints_storage + peer_local_rank;
    *ep_out = ep;
    /* segment_base is this one endpoint's publication point, so a setup
     * path that finds it set may use the rest of it without the lock.
     * The fragment paths do not read it to decide anything; they read
     * component->endpoints, which is published only once every one of
     * these is set. Either way the barrier is paid here, once per
     * endpoint rather than per fragment. */
    if (NULL != ep->segment_base) {
        opal_atomic_rmb();
        return OPAL_SUCCESS;
    }

    if (peer_local_rank != MCA_BTL_SM_LOCAL_RANK) {
        OPAL_MODEX_RECV_LOCAL(rc, &component->super.btl_version, &proc->proc_name,
                              (void **) &modex, &msg_size);
        if (OPAL_SUCCESS != rc) {
            *ep_out = NULL;
            return sm_modex_not_ready(proc, rc);
        }
    }

    OPAL_THREAD_LOCK(&component->lock);
    if (NULL != ep->segment_base) {
        opal_atomic_rmb();
        OPAL_THREAD_UNLOCK(&component->lock);
        if (NULL != modex) {
            free(modex);
        }
        *ep_out = ep;
        return OPAL_SUCCESS;
    }

    OBJ_CONSTRUCT(ep, mca_btl_sm_endpoint_t);

    ep->peer_smp_rank = peer_local_rank;

    if (!mca_btl_is_self_endpoint(ep)) {
        /* attach to the remote segment */
        ep->smsc_endpoint = NULL;  /* assume no one sided support */
        if( NULL != mca_smsc ) {
            ep->smsc_endpoint = MCA_SMSC_CALL(get_endpoint, proc);
        }
        if (NULL == ep->smsc_endpoint) {
            /* disable RDMA */
            mca_btl_sm.super.btl_get = NULL;
            mca_btl_sm.super.btl_put = NULL;
            mca_btl_sm.super.btl_flags &= ~MCA_BTL_FLAGS_RDMA;
        }
            /* Validate the peer-supplied descriptor length before trusting it.
             * The modex must actually contain seg_ds_size bytes of seg_ds, and
             * that length must fit in opal_shmem_ds_t. */
            const size_t modex_hdr_size = sizeof(*modex) - sizeof(modex->seg_ds);
            if (modex->seg_ds_size <= 0
                || (size_t) modex->seg_ds_size > sizeof(opal_shmem_ds_t)
                || msg_size < modex_hdr_size
                || (size_t) modex->seg_ds_size > msg_size - modex_hdr_size) {
                free(modex);
                OBJ_DESTRUCT(ep);
                *ep_out = NULL;
                OPAL_THREAD_UNLOCK(&component->lock);
                return OPAL_ERR_BAD_PARAM;
            }

            /* Always allocate the full struct so later consumers (detach,
             * opal_shmem_sizeof_shmem_ds) cannot read or write past the end
             * of the heap object. */
            ep->seg_ds = calloc(1, sizeof(opal_shmem_ds_t));
            if (NULL == ep->seg_ds) {
                free(modex);
                OBJ_DESTRUCT(ep);
                *ep_out = NULL;
                OPAL_THREAD_UNLOCK(&component->lock);
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            memcpy(ep->seg_ds, &modex->seg_ds, modex->seg_ds_size);
            /* Guarantee seg_name is NUL-terminated even if the peer sent an
             * unterminated path, so opal_shmem_sizeof_shmem_ds()'s strlen
             * cannot run past the buffer. */
            ep->seg_ds->seg_name[OPAL_PATH_MAX - 1] = '\0';

            segment_base = opal_shmem_segment_attach(ep->seg_ds);
            if (NULL == segment_base) {
                free(modex);
                OBJ_DESTRUCT(ep);
                *ep_out = NULL;
                OPAL_THREAD_UNLOCK(&component->lock);
                return OPAL_ERROR;
            }

        OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);

        free(modex);
    } else {
        /* set up the segment base so we can calculate a virtual to real for local pointers */
        segment_base = component->my_segment;
    }

    /* Publish segment_base last: a reader outside this lock decides the
     * endpoint is usable on that one store, and it has to find the fifo,
     * the smsc endpoint and the lock already there. The fragment path
     * reads no other field before it has the segment, which is why it
     * needs no matching read barrier. */
    ep->fifo = (struct sm_fifo_t *) segment_base;
    opal_atomic_wmb();
    ep->segment_base = segment_base;
    OPAL_THREAD_UNLOCK(&component->lock);

    return OPAL_SUCCESS;
}

static int sm_ensure_inited(void)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    bool raise = false;
    int rc = OPAL_SUCCESS;

    /* first_time_init() only allocates, and the pool it carves the free
     * lists out of never gives anything back, so a failure is permanent
     * and btl_inited keeps it: whichever peer's add_procs happened to run
     * it first, this btl is now out for every peer. Report that instead of
     * retrying per peer and per incoming fragment. */
    if (OPAL_UNLIKELY(0 > mca_btl_sm.btl_inited)) {
        return mca_btl_sm.btl_inited;
    }

    if (0 == mca_btl_sm.btl_inited) {
        OPAL_THREAD_LOCK(&component->lock);
        if (0 == mca_btl_sm.btl_inited) {
            rc = sm_btl_first_time_init(&mca_btl_sm, 1 + MCA_BTL_SM_NUM_LOCAL_PEERS);
            if (OPAL_SUCCESS != rc) {
                mca_btl_sm.btl_inited = rc;
                raise = true;
                BTL_ERROR(("could not initialize the shared memory btl (%d). A "
                           "btl_sm_segment_size too small to hold btl_sm_free_list_num "
                           "fragments and btl_sm_fbox_max fast boxes is the usual cause.",
                           rc));
            }
        } else if (0 > mca_btl_sm.btl_inited) {
            /* Another thread failed it while we waited for the lock. */
            rc = mca_btl_sm.btl_inited;
        }
        OPAL_THREAD_UNLOCK(&component->lock);

        /* Falling back on another btl is not on the table: component_init
         * published this process's segment in the modex before anything here
         * ran, so a local peer whose own init succeeded will keep writing
         * into our fifo, and draining it needs the very endpoints this
         * function failed to build. Take the job down with a diagnostic
         * rather than let it hang on an undrainable fifo. */
        if (raise && NULL != mca_btl_sm.error_cb) {
            mca_btl_sm.error_cb(&mca_btl_sm.super, MCA_BTL_ERROR_FLAGS_FATAL, NULL,
                                "the shared memory btl could not allocate its fragment "
                                "pools, and it has already advertised its segment: local "
                                "peers can reach this process, which can no longer "
                                "receive from them");
        }
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    } else {
        /* Pairs with the write barrier in sm_btl_first_time_init(): the
         * flag was read without the lock, and what it announces --
         * component->endpoints_storage before anything else -- is read
         * below. This runs once per add_procs, never per fragment. */
        opal_atomic_rmb();
    }

    /* An incoming fragment carries its sender's local rank and nothing
     * else, so the map that turns one into a proc has to be there before
     * the first one is read. An incomplete map is not fatal, and not
     * final: an unresolved fragment stays in the fifo and comes back
     * through here until the map fills in. */
    if (!component->local_procs_mapped) {
        (void) sm_map_local_procs();
    }

    return rc;
}

/*
 * Fill in local_procs, the SMP-local-rank to proc map an incoming
 * fragment is resolved through. The runtime hands this data out at
 * startup rather than through the modex, so unlike an endpoint it can be
 * had for every local peer at once, without waiting on any of them.
 *
 * It has to cover every local peer, not just the procs an add_procs
 * passed in: a fragment names its sender by local rank and nothing else,
 * and that sender may be a peer this process never sent to, so nothing
 * can be relied on to have introduced it here. The reverse question --
 * which local rank is this proc -- is the easy one, and sm_local_rank_of()
 * answers it one peer at a time.
 *
 * Hence one pass, once, and off the receive path: sm_fifo_read() runs
 * from opal_progress(), and therefore from inside MPI_Wait and btl
 * completion callbacks, where a PMIx round trip per fragment does not
 * belong. While the map is incomplete such a round trip can still happen
 * there -- an unmapped node reaches sm_ensure_inited() through
 * mca_btl_sm_attach_local_peers(), which retries this -- and that is
 * what eventually un-stalls that fragment. It stops once the map has no
 * holes.
 *
 * The cost is one string Get, a local-rank Get per peer, and a proc for
 * each of them: opal_proc_for_name() creates what it does not find, so
 * this instantiates every node-local peer. Bounded by the peers on the
 * node rather than by the job, and they are the peers most likely to be
 * used.
 */
static int sm_map_local_procs(void)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    opal_process_name_t wildcard, name;
    char *peers_str = NULL;
    char **peers;
    int rc;

    wildcard = OPAL_PROC_MY_NAME;
    wildcard.vpid = OPAL_VPID_WILDCARD;
    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_PEERS, &wildcard, &peers_str, PMIX_STRING);
    if (PMIX_SUCCESS != rc || NULL == peers_str) {
        /* Converted because it leaves this file, and the two sets of codes
         * overlap: see sm_modex_not_ready(). */
        return (PMIX_SUCCESS == rc) ? OPAL_ERR_NOT_FOUND : opal_pmix_convert_status(rc);
    }

    peers = opal_argv_split(peers_str, ',');
    free(peers_str);
    if (NULL == peers) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    name.jobid = OPAL_PROC_MY_NAME.jobid;
    for (int i = 0; NULL != peers[i]; ++i) {
        uint16_t lr, *ptr = &lr;
        name.vpid = (opal_vpid_t) strtoul(peers[i], NULL, 10);
        OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_RANK, &name, &ptr, PMIX_UINT16);
        if (PMIX_SUCCESS != rc || lr > MCA_BTL_SM_NUM_LOCAL_PEERS) {
            continue;
        }
        if (NULL == component->local_procs[lr]) {
            component->local_procs[lr] = opal_proc_for_name(name);
        }
    }
    opal_argv_free(peers);

    /* Only a complete map retires this call: a hole in it would leave
     * whoever sits at that local rank permanently unresolvable, and its
     * fragments unreadable. */
    for (uint16_t lr = 0; lr <= (uint16_t) MCA_BTL_SM_NUM_LOCAL_PEERS; ++lr) {
        if (NULL == component->local_procs[lr]) {
            return OPAL_ERR_NOT_FOUND;
        }
    }
    component->local_procs_mapped = true;

    return OPAL_SUCCESS;
}

int mca_btl_sm_attach_local_peers(void)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    int rc, missing = OPAL_SUCCESS;

    if (NULL != component->endpoints) {
        return OPAL_SUCCESS;
    }

    rc = sm_ensure_inited();
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* Nothing can be mapped before the map naming the peers is whole. */
    if (!component->local_procs_mapped) {
        return OPAL_ERR_NOT_READY;
    }

    /* Every peer in one pass rather than a stop at the first miss: where
     * a peer's blob becomes local only because somebody asked for it,
     * the miss is what starts the fetch, so asking about all of them now
     * is what lets one retry finish the node instead of one peer per
     * retry. */
    for (uint16_t lr = 0; lr <= (uint16_t) MCA_BTL_SM_NUM_LOCAL_PEERS; ++lr) {
        mca_btl_base_endpoint_t *ep = NULL;

        if (NULL != component->endpoints_storage[lr].segment_base) {
            continue;
        }

        rc = init_sm_endpoint(&ep, component->local_procs[lr]);
        if (OPAL_SUCCESS == rc) {
            continue;
        }
        if (OPAL_ERR_NOT_READY == rc) {
            /* Its data can still arrive, and this btl is all of the node
             * or none of it, so the node stays unpublished until it does.
             * Keep asking about the rest all the same -- that is what the
             * one pass is for. */
            missing = rc;
            continue;
        }
        /* Final, and this btl is all of the node or none of it. A local
         * peer that publishes no shared memory of its own can never be
         * attached, and the endpoint array must not be published with a
         * hole in it: a fragment names its sender by local rank alone and
         * is read out of that sender's segment, so the receive path would
         * dereference the hole. This btl is therefore unavailable in this
         * process, which sm_add_procs() reports by answering about nobody.
         * Saying that is what bounds the wait: every peer then keeps the
         * decided default instead of NO_INFO, so another btl gets to
         * answer and a peer no btl can reach is declared unreachable
         * rather than retried for the life of the job. */
        return rc;
    }

    if (OPAL_SUCCESS != missing) {
        return missing;
    }

    /* Publish last, behind a barrier: a fragment path decides on this
     * one pointer that the node is mapped, and then addresses every
     * endpoint field it reads off the value it loaded, which is what
     * lets it do so without a barrier of its own. */
    opal_atomic_wmb();
    component->endpoints = component->endpoints_storage;

    return OPAL_SUCCESS;
}

static int fini_sm_endpoint(struct mca_btl_base_endpoint_t *ep)
{
    /* check if the endpoint is initialized. avoids a double-destruct */
    if (ep->segment_base) {
        /* The published array is a claim that every local peer is
         * mapped, and the fragment paths act on it without looking
         * further. Withdraw the claim before breaking it; the next
         * fragment either way re-establishes it, remapping this peer if
         * it is still wanted. */
        mca_btl_sm_component.endpoints = NULL;
        OBJ_DESTRUCT(ep);
    }

    return OPAL_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 *
 * @param btl (IN)
 * @param proc (IN)
 * @param peer (OUT)
 * @return     OPAL_SUCCESS or error status on failure.
 *
 */

static int sm_add_procs(struct mca_btl_base_module_t *btl __opal_attribute_unused__, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                        opal_bitmap_t *status)
{
    const opal_proc_t *my_proc;
    int rc = OPAL_SUCCESS;
    bool node_mapped;

    /* initializion */

    /* get pointer to my proc structure */
    if (NULL == (my_proc = opal_proc_local_get())) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* jump out if there's not someone we can talk to. Every proc keeps
     * the default MCA_BTL_PROC_NOT_ELIGIBLE. */
    if (1 > MCA_BTL_SM_NUM_LOCAL_PEERS) {
        return OPAL_SUCCESS;
    }

    /* All of them or none: an endpoint handed out here is one a send
     * will post a fragment to, and posting chains that fragment onto
     * whichever local peer wrote the destination's fifo last, whose
     * segment this process therefore has to hold as well. */
    rc = mca_btl_sm_attach_local_peers();
    if (OPAL_SUCCESS != rc && OPAL_ERR_NOT_READY != rc) {
        return rc;
    }
    node_mapped = (OPAL_SUCCESS == rc);
    rc = OPAL_SUCCESS;

    for (int32_t proc = 0; proc < (int32_t) nprocs; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job). Neither question
           needs anything the peer published, so a no here is final:
           leave the default MCA_BTL_PROC_NOT_ELIGIBLE. */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid
            || !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        /* Some local peer's segment has not reached us yet, and without
         * all of them this btl can send to none. They are local peers of
         * this job, so they will arrive: say that rather than let a btl
         * of lower exclusivity carry these procs in the meantime. */
        if (!node_mapped) {
            peers[proc] = NULL;
            MCA_BTL_PROC_STATUS_SET(status, proc, MCA_BTL_PROC_NO_INFO);
            continue;
        }

        /* setup endpoint */
        rc = init_sm_endpoint(peers + proc, procs[proc]);
        if (OPAL_ERR_NOT_READY == rc) {
            /* The node is mapped, so this is the peer's own local rank
             * that could not be read -- equally transient. */
            peers[proc] = NULL;
            MCA_BTL_PROC_STATUS_SET(status, proc, MCA_BTL_PROC_NO_INFO);
            rc = OPAL_SUCCESS;
            continue;
        }
        if (OPAL_ERR_NOT_FOUND == rc) {
            /* Its data is local and holds no shared memory of ours, so
             * this btl is not one of its own. Final, and only for this
             * peer: leave the default MCA_BTL_PROC_NOT_ELIGIBLE and keep
             * answering about the others. */
            peers[proc] = NULL;
            rc = OPAL_SUCCESS;
            continue;
        }
        if (OPAL_SUCCESS != rc) {
            peers[proc] = NULL;
            break;
        }

        /* Self gets an endpoint but no claim. The endpoint is what
         * translates the fragments that come back through our own fifo,
         * and what a one-sided operation on ourselves lands in; the
         * absence of a claim is how this btl says "do not pick me to
         * message myself", leaving that to self. */
        if (my_proc != procs[proc]) {
            MCA_BTL_PROC_STATUS_SET(status, proc, MCA_BTL_PROC_CONNECTED);
        }
    }

    return rc;
}

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */

static int sm_del_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers)
{
    for (size_t i = 0; i < nprocs; ++i) {
        if (peers[i]) {
            fini_sm_endpoint(peers[i]);
            peers[i] = NULL;
        }
    }

    return OPAL_SUCCESS;
}

/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

static int sm_finalize(struct mca_btl_base_module_t *btl)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_sm_t *sm_btl = (mca_btl_sm_t *) btl;

    /* Nothing to unwind unless the init ran to completion: a failed one
     * released what it had taken, and left the arrays below NULL. */
    if (1 != sm_btl->btl_inited) {
        return OPAL_SUCCESS;
    }

    for (int i = 0; i < (int) (1 + MCA_BTL_SM_NUM_LOCAL_PEERS); ++i) {
        fini_sm_endpoint(component->endpoints_storage + i);
    }

    free(component->endpoints_storage);
    component->endpoints_storage = NULL;
    component->endpoints = NULL;

    free(component->local_procs);
    component->local_procs = NULL;
    component->local_procs_mapped = false;

    sm_btl->btl_inited = 0;

    free(component->fbox_in_endpoints);
    component->fbox_in_endpoints = NULL;

    return OPAL_SUCCESS;
}

/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @param cbfunc (IN)  function to call on error
 * @return             Status indicating if cleanup was successful
 */
static int sm_register_error_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_module_error_cb_fn_t cbfunc)
{
    ((mca_btl_sm_t *) btl)->error_cb = cbfunc;
    return OPAL_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t *mca_btl_sm_alloc(struct mca_btl_base_module_t *btl,
                                            struct mca_btl_base_endpoint_t *endpoint, uint8_t order,
                                            size_t size, uint32_t flags)
{
    mca_btl_sm_frag_t *frag = NULL;

    if (size <= (size_t) mca_btl_sm_component.max_inline_send) {
        MCA_BTL_SM_FRAG_ALLOC_USER(frag, endpoint);
    } else if (size <= mca_btl_sm.super.btl_eager_limit) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
    } else if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
               && size <= mca_btl_sm.super.btl_max_send_size) {
        MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].seg_len = size;

        frag->base.des_flags = flags;
        frag->base.order = order;
    }

    return (mca_btl_base_descriptor_t *) frag;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_sm_free(struct mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des)
{
    MCA_BTL_SM_FRAG_RETURN((mca_btl_sm_frag_t *) des);

    return OPAL_SUCCESS;
}

/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
static struct mca_btl_base_descriptor_t *sm_prepare_src(struct mca_btl_base_module_t *btl,
                                                        struct mca_btl_base_endpoint_t *endpoint,
                                                        struct opal_convertor_t *convertor,
                                                        uint8_t order, size_t reserve, size_t *size,
                                                        uint32_t flags)
{
    const size_t total_size = reserve + *size;
    mca_btl_sm_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer(convertor, &data_ptr);
    assert(NULL != data_ptr);

    /* in place send fragment */
    if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor) ||
                      opal_convertor_on_discrete_device(convertor) ||
                      (opal_convertor_on_unified_device(convertor) &&
                       total_size > (size_t) mca_btl_sm_component.max_inline_send))) {
        uint32_t iov_count = 1;
        struct iovec iov;

        /* non-contiguous data requires the convertor */
        if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
            && total_size > mca_btl_sm.super.btl_eager_limit) {
            MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
        } else {
            MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
        }

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        iov.iov_len = *size;
        iov.iov_base = (IOVBASE_TYPE *) (((uintptr_t)(frag->segments[0].seg_addr.pval)) + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, size);
        if (OPAL_UNLIKELY(rc < 0)) {
            MCA_BTL_SM_FRAG_RETURN(frag);
            return NULL;
        }

        frag->segments[0].seg_len = *size + reserve;
    } else {
        if (!mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)) {
            if (OPAL_LIKELY(total_size <= mca_btl_sm.super.btl_eager_limit)) {
                MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint);
            } else {
                MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint);
            }
        } else {
            MCA_BTL_SM_FRAG_ALLOC_USER(frag, endpoint);
        }

        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        /* use single-copy to send this segment if it is above the max inline send size */
        if (mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP)
            && total_size > (size_t) mca_btl_sm_component.max_inline_send) {
            /* single copy send */
            frag->hdr->flags = MCA_BTL_SM_FLAG_SINGLE_COPY;

            /* set up single copy io vector */
            frag->hdr->sc_iov.iov_base = data_ptr;
            frag->hdr->sc_iov.iov_len = *size;

            frag->segments[0].seg_len = reserve;
            frag->segments[1].seg_len = *size;
            frag->segments[1].seg_addr.pval = data_ptr;
            frag->base.des_segment_count = 2;
        } else {
            /* NTH: the covertor adds some latency so we bypass it here */
            memcpy((void *) ((uintptr_t) frag->segments[0].seg_addr.pval + reserve), data_ptr,
                   *size);
            frag->segments[0].seg_len = total_size;
        }
    }

    frag->base.order = order;
    frag->base.des_flags = flags;

    return &frag->base;
}

static void mca_btl_sm_endpoint_constructor(mca_btl_sm_endpoint_t *ep)
{
    OBJ_CONSTRUCT(&ep->pending_frags, opal_list_t);
    OBJ_CONSTRUCT(&ep->pending_frags_lock, opal_mutex_t);
    ep->segment_base = NULL;
    ep->fifo = NULL;
    ep->fbox_out.fbox = NULL;
    ep->seg_ds = NULL;
    ep->smsc_endpoint = NULL;
}

static void mca_btl_sm_endpoint_destructor(mca_btl_sm_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->pending_frags);
    OBJ_DESTRUCT(&ep->pending_frags_lock);

    if (ep->seg_ds) {
        /* ep->seg_ds is allocated full-size in init_sm_endpoint, so detach
         * cannot read or write past the end of it. */
        opal_shmem_segment_detach(ep->seg_ds);
        free(ep->seg_ds);
        ep->seg_ds = NULL;
    }

    if (ep->fbox_out.fbox) {
        opal_free_list_return(&mca_btl_sm_component.sm_fboxes, ep->fbox_out.fbox);
    }

    if (ep->smsc_endpoint) {
        MCA_SMSC_CALL(return_endpoint, ep->smsc_endpoint);
        ep->smsc_endpoint = NULL;
    }

    ep->fbox_in.buffer = ep->fbox_out.buffer = NULL;
    ep->fbox_out.fbox = NULL;
    ep->segment_base = NULL;
    ep->fifo = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_sm_endpoint_t, opal_list_item_t, mca_btl_sm_endpoint_constructor,
                   mca_btl_sm_endpoint_destructor);
