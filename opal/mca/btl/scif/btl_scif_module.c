/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_scif.h"
#include "btl_scif_frag.h"
#include "btl_scif_endpoint.h"

static int
mca_btl_scif_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des);

static int
mca_btl_scif_module_finalize (struct mca_btl_base_module_t* btl);

static mca_btl_base_registration_handle_t *mca_btl_scif_register_mem (struct mca_btl_base_module_t *btl,
                                                                      mca_btl_base_endpoint_t *endpoint,
                                                                      void *base, size_t size, uint32_t flags);
static int mca_btl_scif_deregister_mem (struct mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle);

static struct mca_btl_base_descriptor_t *
mca_btl_scif_prepare_src (struct mca_btl_base_module_t *btl,
                          struct mca_btl_base_endpoint_t *endpoint,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags);

mca_btl_scif_module_t mca_btl_scif_module = {
    .super = {
        .btl_component      = &mca_btl_scif_component.super,
        .btl_add_procs      = mca_btl_scif_add_procs,
        .btl_del_procs      = mca_btl_scif_del_procs,
        .btl_finalize       = mca_btl_scif_module_finalize,
        .btl_alloc          = mca_btl_scif_alloc,
        .btl_free           = mca_btl_scif_free,
        .btl_prepare_src    = mca_btl_scif_prepare_src,
        .btl_send           = mca_btl_scif_send,
        .btl_sendi          = mca_btl_scif_sendi,
        .btl_put            = mca_btl_scif_put,
        .btl_get            = mca_btl_scif_get,
        .btl_register_mem   = mca_btl_scif_register_mem,
        .btl_deregister_mem = mca_btl_scif_deregister_mem,
    }
};

int mca_btl_scif_module_init (void)
{
    int rc;

    /* create an endpoint to listen for connections */
    mca_btl_scif_module.scif_fd = scif_open ();
    if (-1 == mca_btl_scif_module.scif_fd) {
        BTL_VERBOSE(("scif_open failed. errno = %d", errno));
        return OPAL_ERROR;
    }

    /* bind the endpoint to a port */
    mca_btl_scif_module.port_id.port = scif_bind (mca_btl_scif_module.scif_fd, 0);
    if (-1 == mca_btl_scif_module.port_id.port) {
        BTL_VERBOSE(("scif_bind failed. errno = %d", errno));
        scif_close (mca_btl_scif_module.scif_fd);
        mca_btl_scif_module.scif_fd = -1;
        return OPAL_ERROR;
    }

    /* determine this processes node id */
    rc = scif_get_nodeIDs (NULL, 0, &mca_btl_scif_module.port_id.node);
    if (-1 == rc) {
        BTL_VERBOSE(("btl/scif error getting node id of this node"));
        return OPAL_ERROR;
    }

    /* Listen for connections */
    /* TODO - base the maximum backlog off something */
    rc = scif_listen (mca_btl_scif_module.scif_fd, 64);
    if (-1 == rc) {
        BTL_VERBOSE(("scif_listen failed. errno = %d", errno));
        scif_close (mca_btl_scif_module.scif_fd);
        mca_btl_scif_module.scif_fd = -1;
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("btl/scif: listening @ port %u on node %u\n",
                 mca_btl_scif_module.port_id.port, mca_btl_scif_module.port_id.node));

    OBJ_CONSTRUCT(&mca_btl_scif_module.dma_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_scif_module.eager_frags, opal_free_list_t);

    return OPAL_SUCCESS;
}

static int
mca_btl_scif_module_finalize (struct mca_btl_base_module_t *btl)
{
    mca_btl_scif_module_t *scif_module = (mca_btl_scif_module_t *) btl;
    unsigned int i;

    OBJ_DESTRUCT(&mca_btl_scif_module.dma_frags);
    OBJ_DESTRUCT(&mca_btl_scif_module.eager_frags);

    mca_btl_scif_module.exiting = true;

    /* close all open connections and release endpoints */
    if (NULL != scif_module->endpoints) {
        for (i = 0 ; i < scif_module->endpoint_count ; ++i) {
            mca_btl_scif_ep_release (scif_module->endpoints + i);
        }

        free (scif_module->endpoints);

        scif_module->endpoint_count = 0;
        scif_module->endpoints = NULL;
    }

    /* close the listening endpoint */
    if (mca_btl_scif_module.listening && -1 != mca_btl_scif_module.scif_fd) {
        /* wake up the scif thread */
        scif_epd_t tmpfd;
        tmpfd = scif_open();
        scif_connect (tmpfd, &mca_btl_scif_module.port_id);
        pthread_join(mca_btl_scif_module.listen_thread, NULL);
        scif_close(tmpfd);
        scif_close (mca_btl_scif_module.scif_fd);
    }

    mca_btl_scif_module.scif_fd = -1;

    return OPAL_SUCCESS;
}

mca_btl_base_descriptor_t *
mca_btl_scif_alloc(struct mca_btl_base_module_t *btl,
                   struct mca_btl_base_endpoint_t *endpoint,
                   uint8_t order, size_t size, uint32_t flags)
{
    mca_btl_scif_base_frag_t *frag = NULL;

    BTL_VERBOSE(("allocating fragment of size: %u", (unsigned int)size));

    if (size <= mca_btl_scif_module.super.btl_eager_limit) {
        (void) MCA_BTL_SCIF_FRAG_ALLOC_EAGER(endpoint, frag);
    }

    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    BTL_VERBOSE(("btl/scif_module allocated frag of size: %u, flags: %x. frag = %p",
                 (unsigned int)size, flags, (void *) frag));

    frag->base.des_flags = flags;
    frag->base.order = order;
    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;

    frag->segments[0].seg_len       = size;

    return &frag->base;
}

static int
mca_btl_scif_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des)
{
    return mca_btl_scif_frag_return ((mca_btl_scif_base_frag_t *) des);
}

static mca_btl_base_registration_handle_t *mca_btl_scif_register_mem (struct mca_btl_base_module_t *btl,
                                                                      mca_btl_base_endpoint_t *endpoint,
                                                                      void *base, size_t size, uint32_t flags)
{
    mca_btl_scif_reg_t *scif_reg;
    int rc;

    if (MCA_BTL_ENDPOINT_ANY == endpoint) {
        /* it probably isn't possible to support registering memory to use with any endpoint so
         * return NULL */
        return NULL;
    }

    if (OPAL_LIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
        /* the endpoint needs to be connected before the fragment can be
         * registered. */
        rc = mca_btl_scif_ep_connect (endpoint);
        if (OPAL_LIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
            /* not yet connected */
            return NULL;
        }
    }

    rc = btl->btl_mpool->mpool_register(btl->btl_mpool, base, size, 0,
                                        (mca_mpool_base_registration_t **) &scif_reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return NULL;
    }

    /* register the memory location with this peer if it isn't already */
    if ((off_t) -1 == scif_reg->handles[endpoint->id].btl_handle.scif_offset) {
        size_t seg_size = (size_t)((uintptr_t) scif_reg->base.bound - (uintptr_t) scif_reg->base.base) + 1;

        /* NTH: until we determine a way to pass permissions to the mpool just make all segments
         * read/write */
        scif_reg->handles[endpoint->id].btl_handle.scif_offset =
            scif_register (endpoint->scif_epd, scif_reg->base.base, seg_size, 0, SCIF_PROT_READ |
                           SCIF_PROT_WRITE, 0);
        BTL_VERBOSE(("registered fragment for scif DMA transaction. offset = %lu",
                     (unsigned long) scif_reg->handles[endpoint->id].btl_handle.scif_offset));
    }

    return &scif_reg->handles[endpoint->id].btl_handle;
}

static int mca_btl_scif_deregister_mem (struct mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_scif_registration_handle_t *scif_handle = (mca_btl_scif_registration_handle_t *) handle;
    mca_btl_scif_reg_t *scif_reg = scif_handle->reg;

    btl->btl_mpool->mpool_deregister (btl->btl_mpool, &scif_reg->base);

    return OPAL_SUCCESS;
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_scif_prepare_src_send (struct mca_btl_base_module_t *btl,
                               mca_btl_base_endpoint_t *endpoint,
                               struct opal_convertor_t *convertor,
                               uint8_t order, size_t reserve, size_t *size,
                               uint32_t flags)
{
    mca_btl_scif_base_frag_t *frag = NULL;
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_size = *size;
    int rc;

    if (OPAL_LIKELY((mca_btl_scif_module.super.btl_flags & MCA_BTL_FLAGS_SEND_INPLACE) &&
                    !opal_convertor_need_buffers (convertor) &&
                    reserve <= 128)) {
        /* inplace send */
        void *data_ptr;
        opal_convertor_get_current_pointer (convertor, &data_ptr);

        (void) MCA_BTL_SCIF_FRAG_ALLOC_DMA(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->segments[0].seg_len       = reserve;
        frag->segments[1].seg_addr.pval = data_ptr;
        frag->segments[1].seg_len       = *size;
        frag->base.des_segment_count = 2;
    } else {
        /* buffered send */
        (void) MCA_BTL_SCIF_FRAG_ALLOC_EAGER(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        if (*size) {
            iov.iov_len  = *size;
            iov.iov_base = (IOVBASE_TYPE *) ((uintptr_t) frag->segments[0].seg_addr.pval + reserve);

            rc = opal_convertor_pack (convertor, &iov, &iov_count, &max_size);
            if (OPAL_UNLIKELY(rc < 0)) {
                mca_btl_scif_frag_return (frag);
                return NULL;
            }
            *size = max_size;
        }

        frag->segments[0].seg_len = reserve + *size;
        frag->base.des_segment_count = 1;
    }

    frag->base.des_segments = frag->segments;
    frag->base.order        = order;
    frag->base.des_flags    = flags;

    return &frag->base;
}

static mca_btl_base_descriptor_t *mca_btl_scif_prepare_src (struct mca_btl_base_module_t *btl,
                                                            mca_btl_base_endpoint_t *endpoint,
                                                            struct opal_convertor_t *convertor,
                                                            uint8_t order, size_t reserve, size_t *size,
                                                            uint32_t flags)
{
    return mca_btl_scif_prepare_src_send (btl, endpoint, convertor, order, reserve, size, flags);
}
