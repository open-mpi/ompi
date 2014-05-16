/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_scif.h"
#include "btl_scif_frag.h"
#include "btl_scif_endpoint.h"

static int
mca_btl_scif_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des);

static int
mca_btl_scif_module_finalize (struct mca_btl_base_module_t* btl);

static mca_btl_base_descriptor_t *
mca_btl_scif_prepare_dst (mca_btl_base_module_t *btl,
                          mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          opal_convertor_t *convertor, uint8_t order,
                          size_t reserve, size_t *size, uint32_t flags);

static struct mca_btl_base_descriptor_t *
mca_btl_scif_prepare_src (struct mca_btl_base_module_t *btl,
                          struct mca_btl_base_endpoint_t *endpoint,
                          mca_mpool_base_registration_t *registration,
                          struct opal_convertor_t *convertor,
                          uint8_t order, size_t reserve, size_t *size,
                          uint32_t flags);

mca_btl_scif_module_t mca_btl_scif_module = {
    .super = {
        .btl_component      = &mca_btl_scif_component.super,
        .btl_add_procs      = mca_btl_scif_add_procs,
        .btl_del_procs      = mca_btl_scif_del_procs,
        .btl_register       = NULL,
        .btl_finalize       = mca_btl_scif_module_finalize,
        .btl_alloc          = mca_btl_scif_alloc,
        .btl_free           = mca_btl_scif_free,
        .btl_prepare_src    = mca_btl_scif_prepare_src,
        .btl_prepare_dst    = mca_btl_scif_prepare_dst,
        .btl_send           = mca_btl_scif_send,
        .btl_sendi          = mca_btl_scif_sendi,
        .btl_put            = mca_btl_scif_put,
        .btl_get            = mca_btl_scif_get,
        .btl_dump           = NULL,
        .btl_mpool          = NULL,
        .btl_register_error = NULL,
        .btl_ft_event       = NULL,
    }
};

int mca_btl_scif_module_init (void)
{
    int rc;

    /* create an endpoint to listen for connections */
    mca_btl_scif_module.scif_fd = scif_open ();
    if (-1 == mca_btl_scif_module.scif_fd) {
        BTL_VERBOSE(("scif_open failed. errno = %d", errno));
        return OMPI_ERROR;
    }

    /* bind the endpoint to a port */
    mca_btl_scif_module.port_id.port = scif_bind (mca_btl_scif_module.scif_fd, 0);
    if (-1 == mca_btl_scif_module.port_id.port) {
        BTL_VERBOSE(("scif_bind failed. errno = %d", errno));
        scif_close (mca_btl_scif_module.scif_fd);
        mca_btl_scif_module.scif_fd = -1;
        return OMPI_ERROR;
    }

    /* determine this processes node id */
    rc = scif_get_nodeIDs (NULL, 0, &mca_btl_scif_module.port_id.node);
    if (-1 == rc) {
        BTL_VERBOSE(("btl/scif error getting node id of this node"));
        return OMPI_ERROR;
    }

    /* Listen for connections */
    /* TODO - base the maximum backlog off something */
    rc = scif_listen (mca_btl_scif_module.scif_fd, 64);
    if (-1 == rc) {
        BTL_VERBOSE(("scif_listen failed. errno = %d", errno));
        scif_close (mca_btl_scif_module.scif_fd);
        mca_btl_scif_module.scif_fd = -1;
        return OMPI_ERROR;
    }

    BTL_VERBOSE(("btl/scif: listening @ port %u on node %u\n",
                 mca_btl_scif_module.port_id.port, mca_btl_scif_module.port_id.node));

    OBJ_CONSTRUCT(&mca_btl_scif_module.dma_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_scif_module.eager_frags, ompi_free_list_t);

    return OMPI_SUCCESS;
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
    if (-1 != mca_btl_scif_module.scif_fd) {
        /* wake up the scif thread */
        scif_epd_t tmpfd;
        tmpfd = scif_open();
        scif_connect (tmpfd, &mca_btl_scif_module.port_id);
        pthread_join(mca_btl_scif_module.listen_thread, NULL);
        scif_close(tmpfd);
        scif_close (mca_btl_scif_module.scif_fd);
    }

    mca_btl_scif_module.scif_fd = -1;

    return OMPI_SUCCESS;
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
    frag->base.des_src = &frag->segments[0].base;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = &frag->segments[0].base;
    frag->base.des_dst_cnt = 1;

    frag->segments[0].base.seg_len       = size;

    return &frag->base;
}

static int
mca_btl_scif_free (struct mca_btl_base_module_t *btl,
                   mca_btl_base_descriptor_t *des)
{
    return mca_btl_scif_frag_return ((mca_btl_scif_base_frag_t *) des);
}

static inline int mca_btl_scif_prepare_dma (struct mca_btl_base_module_t *btl,
                                            mca_btl_base_endpoint_t *endpoint,
                                            void *data_ptr, size_t size,
                                            mca_mpool_base_registration_t *registration,
                                            uint8_t order, uint32_t flags,
                                            mca_btl_scif_base_frag_t **frag_out)
{
    mca_btl_scif_base_frag_t *frag;
    mca_btl_scif_reg_t *scif_reg;
    int rc;

    if (OPAL_LIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
        /* the endpoint needs to be connected before the fragment can be
         * registered. */
        rc = mca_btl_scif_ep_connect (endpoint);
        if (OPAL_LIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED != endpoint->state)) {
            /* not yet connected */
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    (void) MCA_BTL_SCIF_FRAG_ALLOC_DMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (NULL == registration) {
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool, data_ptr, size, 0,
                                            (mca_mpool_base_registration_t **) &registration);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            mca_btl_scif_frag_return (frag);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        frag->registration = (mca_btl_scif_reg_t *) registration;
    }
 
    scif_reg = (mca_btl_scif_reg_t *) registration;

    /* register the memory location with this peer if it isn't already */
    if ((off_t) -1 == scif_reg->registrations[endpoint->id]) {
        size_t seg_size = (size_t)((uintptr_t) registration->bound - (uintptr_t) registration->base) + 1;
        scif_reg->registrations[endpoint->id] = scif_register (endpoint->scif_epd, registration->base,
                                                               seg_size, 0, SCIF_PROT_READ |
                                                               SCIF_PROT_WRITE, 0);
        BTL_VERBOSE(("registered fragment for scif DMA transaction. offset = %lu",
                     (unsigned long) scif_reg->registrations[endpoint->id]));
    }

    if (OPAL_UNLIKELY((off_t) -1 == scif_reg->registrations[endpoint->id])) {
        mca_btl_scif_frag_return (frag);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    frag->segments[0].base.seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
    frag->segments[0].base.seg_len       = size;
    frag->segments[0].scif_offset        = scif_reg->registrations[endpoint->id] +
        (off_t) ((ptrdiff_t) data_ptr - (ptrdiff_t) registration->base);
    /* save the original pointer so the offset can be adjusted if needed (this is
     * required for osc/rdma) */
    frag->segments[0].orig_ptr           = (uint64_t)(uintptr_t) data_ptr;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    *frag_out = frag;

    return OMPI_SUCCESS;
}

static mca_btl_base_descriptor_t *mca_btl_scif_prepare_src_dma (struct mca_btl_base_module_t *btl,
                                                                mca_btl_base_endpoint_t *endpoint,
                                                                mca_mpool_base_registration_t *registration,
                                                                struct opal_convertor_t *convertor,
                                                                uint8_t order, size_t *size,
                                                                uint32_t flags)
{
    mca_btl_scif_base_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    rc = mca_btl_scif_prepare_dma (btl, endpoint, data_ptr, *size, registration,
                                   order, flags, &frag);
    if (OMPI_SUCCESS != rc) {
        return NULL;
    }

    frag->base.des_src     = &frag->segments->base;
    frag->base.des_src_cnt = 1;

    return &frag->base;
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

        frag->segments[0].base.seg_len       = reserve;
        frag->segments[1].base.seg_addr.pval = data_ptr;
        frag->segments[1].base.seg_len       = *size;
        frag->base.des_src_cnt = 2;
    } else {
        /* buffered send */
        (void) MCA_BTL_SCIF_FRAG_ALLOC_EAGER(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        if (*size) {
            iov.iov_len  = *size;
            iov.iov_base = (IOVBASE_TYPE *) ((uintptr_t) frag->segments[0].base.seg_addr.pval + reserve);

            rc = opal_convertor_pack (convertor, &iov, &iov_count, &max_size);
            if (OPAL_UNLIKELY(rc < 0)) {
                mca_btl_scif_frag_return (frag);
                return NULL;
            }
            *size = max_size;
        }

        frag->segments[0].base.seg_len = reserve + *size;
        frag->base.des_src_cnt = 1;
    }

    frag->base.des_src     = &frag->segments->base;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

static mca_btl_base_descriptor_t *mca_btl_scif_prepare_src (struct mca_btl_base_module_t *btl,
                                                            mca_btl_base_endpoint_t *endpoint,
                                                            mca_mpool_base_registration_t *registration,
                                                            struct opal_convertor_t *convertor,
                                                            uint8_t order, size_t reserve, size_t *size,
                                                            uint32_t flags)
{
    if (OPAL_LIKELY(reserve)) {
        return mca_btl_scif_prepare_src_send (btl, endpoint, convertor,
                                              order, reserve, size, flags);
    } else {
        return mca_btl_scif_prepare_src_dma (btl, endpoint, registration,
                                              convertor, order, size, flags);
    }
}

static mca_btl_base_descriptor_t *mca_btl_scif_prepare_dst (mca_btl_base_module_t *btl,
                                                            mca_btl_base_endpoint_t *endpoint,
                                                            mca_mpool_base_registration_t *registration,
                                                            opal_convertor_t *convertor, uint8_t order,
                                                            size_t reserve, size_t *size, uint32_t flags)
{
    mca_btl_scif_base_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    rc = mca_btl_scif_prepare_dma (btl, endpoint, data_ptr, *size, registration,
                                   order, flags, &frag);
    if (OMPI_SUCCESS != rc) {
        return NULL;
    }

    frag->base.des_dst     = &frag->segments->base;
    frag->base.des_dst_cnt = 1;

    return &frag->base;
}
