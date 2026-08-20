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
                        struct opal_bitmap_t *reachability);

static int init_sm_endpoint(struct mca_btl_base_endpoint_t **ep_out, struct opal_proc_t *proc);

mca_btl_sm_t mca_btl_sm = {
    {&mca_btl_sm_component.super, .btl_add_procs = sm_add_procs, .btl_del_procs = sm_del_procs,
     .btl_finalize = sm_finalize, .btl_alloc = mca_btl_sm_alloc, .btl_free = mca_btl_sm_free,
     .btl_prepare_src = sm_prepare_src, .btl_send = mca_btl_sm_send, .btl_sendi = mca_btl_sm_sendi,
     .btl_dump = mca_btl_base_dump, .btl_register_error = sm_register_error_cb}};

static int sm_btl_first_time_init(mca_btl_sm_t *sm_btl, int n)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    int rc;

    /* generate the endpoints */
    component->endpoints = (struct mca_btl_base_endpoint_t *)
        calloc(n + 1, sizeof(struct mca_btl_base_endpoint_t));
    if (NULL == component->endpoints) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    component->endpoints[n].peer_smp_rank = -1;

    component->local_procs = (opal_proc_t **) calloc(n + 1, sizeof(opal_proc_t *));
    if (NULL == component->local_procs) {
        free(component->endpoints);
        component->endpoints = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    component->fbox_in_endpoints = calloc(n + 1, sizeof(void *));
    if (NULL == component->fbox_in_endpoints) {
        free(component->local_procs);
        component->local_procs = NULL;
        free(component->endpoints);
        component->endpoints = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    component->mpool = mca_mpool_basic_create((void *) (component->my_segment
                                                        + MCA_BTL_SM_FIFO_SIZE),
                                              (unsigned long) (mca_btl_sm_component.segment_size
                                                               - MCA_BTL_SM_FIFO_SIZE),
                                              64);
    if (NULL == component->mpool) {
        free(component->fbox_in_endpoints);
        component->fbox_in_endpoints = NULL;
        free(component->local_procs);
        component->local_procs = NULL;
        free(component->endpoints);
        component->endpoints = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Fast box buffers are prepended with a metadata section. */
    rc = opal_free_list_init(&component->sm_fboxes, sizeof(opal_free_list_item_t), 8,
                             OBJ_CLASS(opal_free_list_item_t), mca_btl_sm_component.fbox_size +
                             sizeof (mca_btl_sm_fbox_metadata_t),
                             opal_cache_line_size, 0, mca_btl_sm_component.fbox_max, 4,
                             component->mpool, 0, NULL, NULL, NULL);
    if (OPAL_SUCCESS != rc) {
        return rc;
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
        return rc;
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
        return rc;
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
            return rc;
        }
    }

    /* set flag indicating btl has been inited */
    sm_btl->btl_inited = true;

    return OPAL_SUCCESS;
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

static int init_sm_endpoint(struct mca_btl_base_endpoint_t **ep_out, struct opal_proc_t *proc)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    mca_btl_sm_modex_t *modex = NULL;
    size_t msg_size;
    int rc;

    uint16_t peer_local_rank;
    uint16_t *ptr = &peer_local_rank;
    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_RANK, &proc->proc_name, &ptr, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        BTL_VERBOSE(("could not read the local rank for peer. rc=%d", rc));
        /* That macro reports what PMIx said, unlike its
         * OPAL_MODEX_RECV_LOCAL neighbour below, so convert it: the two
         * sets of codes overlap numerically and disagree on what the
         * numbers mean. */
        return sm_modex_not_ready(proc, opal_pmix_convert_status(rc));
    }

    mca_btl_base_endpoint_t *ep = component->endpoints + peer_local_rank;
    *ep_out = ep;
    if (NULL != ep->fifo) {
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
    if (NULL != ep->fifo) {
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

            ep->segment_base = opal_shmem_segment_attach(ep->seg_ds);
            if (NULL == ep->segment_base) {
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
        ep->segment_base = component->my_segment;
    }

    ep->fifo = (struct sm_fifo_t *) ep->segment_base;
    OPAL_THREAD_UNLOCK(&component->lock);

    return OPAL_SUCCESS;
}

static int sm_ensure_inited(void)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    int rc = OPAL_SUCCESS;

    if (!mca_btl_sm.btl_inited) {
        OPAL_THREAD_LOCK(&component->lock);
        if (!mca_btl_sm.btl_inited) {
            rc = sm_btl_first_time_init(&mca_btl_sm, 1 + MCA_BTL_SM_NUM_LOCAL_PEERS);
        }
        OPAL_THREAD_UNLOCK(&component->lock);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    /* Completions written back into our FIFO are tagged with our local
     * rank. That slot must exist even when add_procs never included
     * self (lazy single-peer wire-up). */
    if (NULL == component->endpoints[MCA_BTL_SM_LOCAL_RANK].fifo) {
        struct mca_btl_base_endpoint_t *self_ep = NULL;
        rc = init_sm_endpoint(&self_ep, opal_proc_local_get());
    }
    return rc;
}

static opal_proc_t *sm_proc_for_local_rank(uint16_t local_rank)
{
    mca_btl_sm_component_t *component = &mca_btl_sm_component;
    opal_process_name_t wildcard, name;
    char *peers_str = NULL;
    char **peers;
    int rc;

    if (NULL != component->local_procs[local_rank]) {
        return component->local_procs[local_rank];
    }

    wildcard = OPAL_PROC_MY_NAME;
    wildcard.vpid = OPAL_VPID_WILDCARD;
    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_PEERS, &wildcard, &peers_str, PMIX_STRING);
    if (OPAL_SUCCESS != rc || NULL == peers_str) {
        return NULL;
    }

    peers = opal_argv_split(peers_str, ',');
    free(peers_str);
    if (NULL == peers) {
        return NULL;
    }

    name.jobid = OPAL_PROC_MY_NAME.jobid;
    for (int i = 0; NULL != peers[i]; ++i) {
        uint16_t lr, *ptr = &lr;
        name.vpid = (opal_vpid_t) strtoul(peers[i], NULL, 10);
        OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_RANK, &name, &ptr, PMIX_UINT16);
        if (OPAL_SUCCESS != rc || lr > MCA_BTL_SM_NUM_LOCAL_PEERS) {
            continue;
        }
        if (NULL == component->local_procs[lr]) {
            component->local_procs[lr] = opal_proc_for_name(name);
        }
    }
    opal_argv_free(peers);

    return component->local_procs[local_rank];
}

int mca_btl_sm_attach_peer(uint16_t local_rank)
{
    mca_btl_base_endpoint_t *ep = NULL;
    opal_proc_t *proc;
    int rc;

    rc = sm_ensure_inited();
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    if (local_rank > MCA_BTL_SM_NUM_LOCAL_PEERS) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (NULL != mca_btl_sm_component.endpoints[local_rank].segment_base) {
        return OPAL_SUCCESS;
    }

    proc = sm_proc_for_local_rank(local_rank);
    if (NULL == proc) {
        return OPAL_ERR_NOT_READY;
    }

    return init_sm_endpoint(&ep, proc);
}

static int fini_sm_endpoint(struct mca_btl_base_endpoint_t *ep)
{
    /* check if the endpoint is initialized. avoids a double-destruct */
    if (ep->fifo) {
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

static int sm_add_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                        struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                        opal_bitmap_t *reachability)
{
    const opal_proc_t *my_proc;
    int rc = OPAL_SUCCESS;

    (void) btl;

    /* initializion */

    /* get pointer to my proc structure */
    if (NULL == (my_proc = opal_proc_local_get())) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* jump out if there's not someone we can talk to */
    if (1 > MCA_BTL_SM_NUM_LOCAL_PEERS) {
        return OPAL_SUCCESS;
    }

    rc = sm_ensure_inited();
    if (rc != OPAL_SUCCESS) {
        return rc;
    }

    bool not_ready = false;

    for (int32_t proc = 0; proc < (int32_t) nprocs; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid
            || !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        /* setup endpoint */
        rc = init_sm_endpoint(peers + proc, procs[proc]);
        if (OPAL_ERR_NOT_READY == rc) {
            /* Peer has not published yet; leave unwired and keep going. */
            peers[proc] = NULL;
            not_ready = true;
            rc = OPAL_SUCCESS;
            continue;
        }
        if (OPAL_SUCCESS != rc) {
            peers[proc] = NULL;
            break;
        }

        if (my_proc != procs[proc] && NULL != reachability) {
            int brc = opal_bitmap_set_bit(reachability, proc);
            if (OPAL_SUCCESS != brc) {
                return brc;
            }
        }
    }

    if (OPAL_SUCCESS == rc && not_ready) {
        return OPAL_ERR_NOT_READY;
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

    if (!sm_btl->btl_inited) {
        return OPAL_SUCCESS;
    }

    for (int i = 0; i < (int) (1 + MCA_BTL_SM_NUM_LOCAL_PEERS); ++i) {
        fini_sm_endpoint(component->endpoints + i);
    }

    free(component->endpoints);
    component->endpoints = NULL;

    free(component->local_procs);
    component->local_procs = NULL;

    sm_btl->btl_inited = false;

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
