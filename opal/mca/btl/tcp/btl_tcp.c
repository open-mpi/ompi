/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
 * Copyright (c) 2019      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 *
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"
#include "opal/class/opal_bitmap.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/opal_socket_errno.h"
#include "opal/mca/timer/base/base.h"

#include "btl_tcp.h"
#include "btl_tcp_endpoint.h"
#include "btl_tcp_frag.h"
#include "btl_tcp_proc.h"

static int mca_btl_tcp_register_error_cb(struct mca_btl_base_module_t *btl,
                                         mca_btl_base_module_error_cb_fn_t cbfunc);

mca_btl_tcp_module_t mca_btl_tcp_module =
    {.super =
         {
             .btl_component = &mca_btl_tcp_component.super,
             .btl_add_procs = mca_btl_tcp_add_procs,
             .btl_del_procs = mca_btl_tcp_del_procs,
             .btl_finalize = mca_btl_tcp_finalize,
             .btl_alloc = mca_btl_tcp_alloc,
             .btl_free = mca_btl_tcp_free,
             .btl_prepare_src = mca_btl_tcp_prepare_src,
             .btl_send = mca_btl_tcp_send,
             .btl_put = mca_btl_tcp_put,
             .btl_dump = mca_btl_base_dump,
             .btl_register_error = mca_btl_tcp_register_error_cb, /* register error */
         },
     .tcp_endpoints_mutex = OPAL_MUTEX_STATIC_INIT};

static int mca_btl_tcp_register_error_cb(struct mca_btl_base_module_t *btl,
                                         mca_btl_base_module_error_cb_fn_t cbfunc)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    tcp_btl->tcp_error_cb = cbfunc;
    return OPAL_SUCCESS;
}

/**
 *
 */

int mca_btl_tcp_add_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                          struct opal_proc_t **procs, struct mca_btl_base_endpoint_t **peers,
                          opal_bitmap_t *status)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    const opal_proc_t *my_proc; /* pointer to caller's proc structure */
    int i, rc;

    /* get pointer to my proc structure */
    if (NULL == (my_proc = opal_proc_local_get())) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < (int) nprocs; i++) {

        struct opal_proc_t *opal_proc = procs[i];
        mca_btl_tcp_proc_t *tcp_proc;
        mca_btl_base_endpoint_t *tcp_endpoint;

        /* Do not create loopback TCP connections. Final, so leave the
         * default MCA_BTL_PROC_NOT_ELIGIBLE. */
        if (my_proc == opal_proc) {
            continue;
        }

        if (NULL == (tcp_proc = mca_btl_tcp_proc_create(opal_proc, &rc))) {
            if (OPAL_ERR_NOT_READY == rc) {
                MCA_BTL_PROC_STATUS_SET(status, i, MCA_BTL_PROC_NO_INFO);
            }
            continue;
        }

        /* The btl_proc datastructure is shared by all TCP BTL instances
         * that are trying to reach this destination, and an endpoint on it
         * may already have been built by an inbound connection the peer
         * opened first, so ask for ours rather than assume it is absent.
         */
        OPAL_THREAD_LOCK(&tcp_proc->proc_lock);
        tcp_endpoint = mca_btl_tcp_proc_endpoint(tcp_proc, tcp_btl, &rc);
        OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);

        if (NULL == tcp_endpoint) {
            if (OPAL_ERR_OUT_OF_RESOURCE == rc) {
                return rc;
            }
            /* We have the peer's addresses and no interface matches,
             * so this is final: leave MCA_BTL_PROC_NOT_ELIGIBLE. */
            continue;
        }

        /* The socket opens on the first send and this module queues what
         * it cannot write yet, so an endpoint is usable once it exists. */
        MCA_BTL_PROC_STATUS_SET(status, i, MCA_BTL_PROC_CONNECTED);

        peers[i] = tcp_endpoint;
    }

    return OPAL_SUCCESS;
}

int mca_btl_tcp_del_procs(struct mca_btl_base_module_t *btl, size_t nprocs,
                          struct opal_proc_t **procs,
                          struct mca_btl_base_endpoint_t **endpoints __opal_attribute_unused__)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    size_t i;

    /* The endpoints argument is deliberately not read. mca_bml_r2_del_procs()
     * files one endpoint in both its btl_send and its btl_rdma array and
     * tells the two apart by comparing the slot it has already handed us,
     * so it can ask twice for the same endpoint -- and the slot it
     * compares is the one OBJ_RELEASE would have nullified. Taking the
     * peer's proc as the authority instead makes this idempotent: a
     * second ask finds no endpoint for this module and does nothing.
     *
     * That the proc can answer at all is what makes this work: an
     * endpoint reaches no other data structure without first being
     * recorded on its peer's proc by mca_btl_tcp_proc_insert(), so the
     * proc knows every endpoint any caller could name, and knows which
     * module each one belongs to.
     */
    for (i = 0; i < nprocs; i++) {
        mca_btl_tcp_proc_t *tcp_proc = mca_btl_tcp_proc_peek(&procs[i]->proc_name);
        mca_btl_base_endpoint_t *tcp_endpoint;

        if (NULL == tcp_proc) {
            continue; /* never wired to this peer */
        }

        /* proc_lock then tcp_endpoints_mutex, the order
         * mca_btl_tcp_add_procs() establishes. Unlinking here rather
         * than in the endpoint destructor is not a choice: the
         * destructor does not know the module's list, so an endpoint
         * released while still on it would leave mca_btl_tcp_finalize()
         * draining freed memory.
         */
        OPAL_THREAD_LOCK(&tcp_proc->proc_lock);
        tcp_endpoint = mca_btl_tcp_proc_find_endpoint(tcp_proc, tcp_btl);
        if (NULL != tcp_endpoint) {
            OPAL_THREAD_LOCK(&tcp_btl->tcp_endpoints_mutex);
            opal_list_remove_item(&tcp_btl->tcp_endpoints, (opal_list_item_t *) tcp_endpoint);
            OPAL_THREAD_UNLOCK(&tcp_btl->tcp_endpoints_mutex);
        }
        OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);

        if (NULL == tcp_endpoint) {
            continue; /* this module's endpoint is already retired */
        }

        /* Released holding neither lock. The destructor reaches
         * mca_btl_tcp_proc_remove(), which takes proc_lock, and on the
         * proc's last endpoint releases the proc, whose destructor takes
         * the component's tcp_lock; doing that from inside either lock
         * is an ABBA against a concurrent wire-up on this module.
         */
        OBJ_RELEASE(tcp_endpoint);
    }
    return OPAL_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t *mca_btl_tcp_alloc(struct mca_btl_base_module_t *btl,
                                             struct mca_btl_base_endpoint_t *endpoint,
                                             uint8_t order, size_t size, uint32_t flags)
{
    mca_btl_tcp_frag_t *frag = NULL;

    if (size <= btl->btl_eager_limit) {
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag);
    } else if (size <= btl->btl_max_send_size) {
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag);
    }
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    frag->segments[0].seg_len = size;
    frag->segments[0].seg_addr.pval = frag + 1;

    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;
    frag->base.des_flags = flags;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->btl = (mca_btl_tcp_module_t *) btl;
    return (mca_btl_base_descriptor_t *) frag;
}

/**
 * Return a segment
 */

int mca_btl_tcp_free(struct mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des)
{
    mca_btl_tcp_frag_t *frag = (mca_btl_tcp_frag_t *) des;
    MCA_BTL_TCP_FRAG_RETURN(frag);
    return OPAL_SUCCESS;
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t *mca_btl_tcp_prepare_src(struct mca_btl_base_module_t *btl,
                                                   struct mca_btl_base_endpoint_t *endpoint,
                                                   struct opal_convertor_t *convertor,
                                                   uint8_t order, size_t reserve, size_t *size,
                                                   uint32_t flags)
{
    mca_btl_tcp_frag_t *frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if (OPAL_UNLIKELY(max_data > UINT32_MAX)) { /* limit the size to what we support */
        max_data = (size_t) UINT32_MAX;
    }
    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
     */
    if (max_data + reserve <= btl->btl_eager_limit) {
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag);
    } else {
        /*
         * otherwise pack as much data as we can into a fragment
         * that is the max send size.
         */
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag);
    }
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    frag->segments[0].seg_addr.pval = (frag + 1);
    frag->segments[0].seg_len = reserve;

    frag->base.des_segment_count = 1;
    if (opal_convertor_need_buffers(convertor) || opal_convertor_on_device(convertor)) {

        if (max_data + reserve > frag->size) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE *) (((unsigned char *) (frag->segments[0].seg_addr.pval))
                                         + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        if (OPAL_UNLIKELY(rc < 0)) {
            mca_btl_tcp_free(btl, &frag->base);
            return NULL;
        }

        frag->segments[0].seg_len += max_data;

    } else {

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        if (OPAL_UNLIKELY(rc < 0)) {
            mca_btl_tcp_free(btl, &frag->base);
            return NULL;
        }

        frag->segments[1].seg_addr.pval = iov.iov_base;
        frag->segments[1].seg_len = max_data;
        frag->base.des_segment_count = 2;
    }

    frag->base.des_segments = frag->segments;
    frag->base.des_flags = flags;
    frag->base.order = MCA_BTL_NO_ORDER;
    *size = max_data;
    return &frag->base;
}

/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_tcp_send(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                     struct mca_btl_base_descriptor_t *descriptor, mca_btl_base_tag_t tag)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    mca_btl_tcp_frag_t *frag = (mca_btl_tcp_frag_t *) descriptor;
    int i;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->iov_cnt = 1;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE *) &frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->hdr.size = 0;
    for (i = 0; i < (int) frag->base.des_segment_count; i++) {
        frag->hdr.size += frag->segments[i].seg_len;
        frag->iov[i + 1].iov_len = frag->segments[i].seg_len;
        frag->iov[i + 1].iov_base = (IOVBASE_TYPE *) frag->segments[i].seg_addr.pval;
        frag->iov_cnt++;
    }
    frag->hdr.base.tag = tag;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_SEND;
    frag->hdr.count = 0;
    if (endpoint->endpoint_nbo) {
        MCA_BTL_TCP_HDR_HTON(frag->hdr);
    }
    return mca_btl_tcp_endpoint_send(endpoint, frag);
}

static void fake_rdma_complete(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                               mca_btl_base_descriptor_t *desc, int rc)
{
    mca_btl_tcp_frag_t *frag = (mca_btl_tcp_frag_t *) desc;

    frag->cb.func(btl, endpoint, frag->segments[0].seg_addr.pval, NULL, frag->cb.context,
                  frag->cb.data, rc);
}

/**
 * Initiate an asynchronous put.
 */

int mca_btl_tcp_put(mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    void *local_address, uint64_t remote_address,
                    mca_btl_base_registration_handle_t *local_handle,
                    mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                    void *cbdata)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    mca_btl_tcp_frag_t *frag = NULL;
    int i;

    MCA_BTL_TCP_FRAG_ALLOC_USER(frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    frag->endpoint = endpoint;

    frag->segments->seg_len = size;
    frag->segments->seg_addr.pval = local_address;

    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;
    frag->base.order = MCA_BTL_NO_ORDER;

    frag->segments[0].seg_addr.pval = local_address;
    frag->segments[0].seg_len = size;

    frag->segments[1].seg_addr.lval = remote_address;
    frag->segments[1].seg_len = size;
    if (endpoint->endpoint_nbo) {
        MCA_BTL_BASE_SEGMENT_HTON(frag->segments[1]);
    }

    frag->base.des_flags = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.des_cbfunc = fake_rdma_complete;

    frag->cb.func = cbfunc;
    frag->cb.data = cbdata;
    frag->cb.context = cbcontext;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->hdr.size = 0;
    frag->iov_cnt = 2;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE *) &frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE *) (frag->segments + 1);
    frag->iov[1].iov_len = sizeof(mca_btl_base_segment_t);
    for (i = 0; i < (int) frag->base.des_segment_count; i++) {
        frag->hdr.size += frag->segments[i].seg_len;
        frag->iov[i + 2].iov_len = frag->segments[i].seg_len;
        frag->iov[i + 2].iov_base = (IOVBASE_TYPE *) frag->segments[i].seg_addr.pval;
        frag->iov_cnt++;
    }
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_PUT;
    frag->hdr.count = 1;
    if (endpoint->endpoint_nbo) {
        MCA_BTL_TCP_HDR_HTON(frag->hdr);
    }
    return ((i = mca_btl_tcp_endpoint_send(endpoint, frag)) >= 0 ? OPAL_SUCCESS : i);
}

/**
 * Initiate an asynchronous get.
 */

int mca_btl_tcp_get(mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    void *local_address, uint64_t remote_address,
                    mca_btl_base_registration_handle_t *local_handle,
                    mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                    void *cbdata)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    mca_btl_tcp_frag_t *frag = NULL;
    int rc;

    MCA_BTL_TCP_FRAG_ALLOC_USER(frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
        ;
    }

    frag->endpoint = endpoint;

    frag->segments->seg_len = size;
    frag->segments->seg_addr.pval = local_address;

    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;
    frag->base.order = MCA_BTL_NO_ORDER;

    frag->segments[0].seg_addr.pval = local_address;
    frag->segments[0].seg_len = size;

    frag->segments[1].seg_addr.lval = remote_address;
    frag->segments[1].seg_len = size;

    /* call the rdma callback through the descriptor callback. this is
     * tcp so the extra latency is not an issue */
    frag->base.des_flags = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    frag->base.des_cbfunc = fake_rdma_complete;

    frag->cb.func = cbfunc;
    frag->cb.data = cbdata;
    frag->cb.context = cbcontext;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->hdr.size = 0;
    frag->iov_cnt = 2;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE *) &frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE *) &frag->segments[1];
    frag->iov[1].iov_len = sizeof(mca_btl_base_segment_t);
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_GET;
    frag->hdr.count = 1;
    if (endpoint->endpoint_nbo) {
        MCA_BTL_TCP_HDR_HTON(frag->hdr);
    }
    return ((rc = mca_btl_tcp_endpoint_send(endpoint, frag)) >= 0 ? OPAL_SUCCESS : rc);
}

/*
 * Cleanup/release module resources.
 */

int mca_btl_tcp_finalize(struct mca_btl_base_module_t *btl)
{
    mca_btl_tcp_module_t *tcp_btl = (mca_btl_tcp_module_t *) btl;
    opal_list_item_t *item;

    /* Don't lock the tcp_endpoints_mutex, at this point a single
     * thread should be active.
     */
    for (item = opal_list_remove_first(&tcp_btl->tcp_endpoints); item != NULL;
         item = opal_list_remove_first(&tcp_btl->tcp_endpoints)) {
        mca_btl_tcp_endpoint_t *endpoint = (mca_btl_tcp_endpoint_t *) item;
        OBJ_RELEASE(endpoint);
    }
    free(tcp_btl);
    return OPAL_SUCCESS;
}

void mca_btl_tcp_dump(struct mca_btl_base_module_t *base_btl,
                      struct mca_btl_base_endpoint_t *endpoint, int verbose)
{
    mca_btl_tcp_module_t *btl = (mca_btl_tcp_module_t *) base_btl;
    mca_btl_base_err("%s TCP %p kernel_id %d\n"
#if MCA_BTL_TCP_STATISTICS
                     " |   statistics: sent %lu recv %lu\n"
#endif /* MCA_BTL_TCP_STATISTICS */
                     " |   latency %u bandwidth %u\n",
                     OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), (void *) btl, btl->tcp_ifkindex,
#if MCA_BTL_TCP_STATISTICS
                     btl->tcp_bytes_sent, btl->btl_bytes_recv,
#endif /* MCA_BTL_TCP_STATISTICS */
                     btl->super.btl_latency, btl->super.btl_bandwidth);
#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP
    if (NULL != endpoint) {
        MCA_BTL_TCP_ENDPOINT_DUMP(10, endpoint, false, "TCP");

    } else if (verbose) {
        opal_list_item_t *item;

        OPAL_THREAD_LOCK(&btl->tcp_endpoints_mutex);
        for (item = opal_list_get_first(&btl->tcp_endpoints);
             item != opal_list_get_end(&btl->tcp_endpoints); item = opal_list_get_next(item)) {
            MCA_BTL_TCP_ENDPOINT_DUMP(10, (mca_btl_base_endpoint_t *) item, false, "TCP");
        }
        OPAL_THREAD_UNLOCK(&btl->tcp_endpoints_mutex);
    }
#endif /* OPAL_ENABLE_DEBUG && WANT_PEER_DUMP */
}

/*
 * A blocking recv for both blocking and non-blocking socket.
 * Used to receive the small amount of connection information
 * that identifies the endpoints
 *
 * when the socket is blocking (the caller introduces timeout)
 * which happens during initial handshake otherwise socket is
 * non-blocking most of the time.
 */

int mca_btl_tcp_recv_blocking(int sd, void *data, size_t size)
{
    unsigned char *ptr = (unsigned char *) data;
    opal_timer_t start = opal_timer_base_get_usec(), now;

    size_t cnt = 0;
    while (cnt < size) {
        int retval = recv(sd, ((char *) ptr) + cnt, size - cnt, 0);
        now = opal_timer_base_get_usec();
        if (0 >= retval) {
            /* remote closed connection */
            if (0 == retval) {
                OPAL_OUTPUT_VERBOSE((100, opal_btl_base_framework.framework_output,
                                     "remote peer unexpectedly closed connection while I was waiting "
                                     "for a blocking message"));
                break;
            }

            /* socket is non-blocking so handle errors */
            if (opal_socket_errno != EINTR && opal_socket_errno != EAGAIN
                && opal_socket_errno != EWOULDBLOCK) {
                mca_btl_tcp_print_info_about_socket(sd,
                                                    "recv(%d) failed: %s (%d) during connection "
                                                    "handshake. Maybe not an OMPI process ?",
                                                    sd, strerror(opal_socket_errno), opal_socket_errno);
                    break;
            }
            /* socket is blocking, we need to enforce some timeout */
            if (opal_socket_errno != EINTR) { /* EAGAIN | EWOULDBLOCK */
                if ( (now - start) >= (unsigned long long)mca_btl_tcp_component.tcp_handshake_timeout ) {
                    mca_btl_tcp_print_info_about_socket(sd, "timeout while waiting for the connection handshake. Maybe the peer is not an Open MPI process?");
                    break;
                }
            }
            continue;  /* skip the counting */
        }
        cnt += retval;
    }
    return cnt;
}

/*
 * A blocking send on a non-blocking socket. Used to send the small
 * amount of connection information used during the initial handshake
 * (magic string plus process guid)
 *
 * An error return leaves the endpoint untouched: whoever decides it failed
 * owns the state change and the close. Otherwise every byte is written
 * before returning, so the count is never short.
 */

int mca_btl_tcp_send_blocking(int sd, const void *data, size_t size)
{
    unsigned char *ptr = (unsigned char *) data;
    size_t cnt = 0;
    while (cnt < size) {
        int retval = send(sd, ((const char *) ptr) + cnt, size - cnt, 0);
        if (retval < 0) {
            if (opal_socket_errno != EINTR && opal_socket_errno != EAGAIN
                && opal_socket_errno != EWOULDBLOCK) {
                BTL_VERBOSE(
                    ("send() failed: %s (%d)", strerror(opal_socket_errno), opal_socket_errno));
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}
