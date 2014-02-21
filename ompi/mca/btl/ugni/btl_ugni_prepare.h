/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UGNI_PREPARE_H)
#define MCA_BTL_UGNI_PREPARE_H

#include "ompi_config.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send_nodata (struct mca_btl_base_module_t *btl,
                                      mca_btl_base_endpoint_t *endpoint,
                                      uint8_t order, size_t reserve,
                                      uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag = NULL;
    int rc;

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    BTL_VERBOSE(("preparing src for send fragment. size = %u", (unsigned int) reserve));

    frag->hdr_size = reserve + sizeof (frag->hdr.send);

    frag->segments[0].base.seg_addr.pval = frag->hdr.send_ex.pml_header;
    frag->segments[0].base.seg_len       = reserve;

    frag->segments[1].base.seg_addr.pval = NULL;
    frag->segments[1].base.seg_len       = 0;

    frag->base.des_src     = &frag->segments->base;
    frag->base.des_src_cnt = 1;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send_inplace (struct mca_btl_base_module_t *btl,
                                       mca_btl_base_endpoint_t *endpoint,
                                       struct opal_convertor_t *convertor,
                                       uint8_t order, size_t reserve, size_t *size,
                                       uint32_t flags)
{
    bool use_eager_get = (*size + reserve) > mca_btl_ugni_component.smsg_max_data;
    mca_btl_ugni_base_frag_t *frag = NULL;
    mca_btl_ugni_reg_t *registration = NULL;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);

    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    BTL_VERBOSE(("preparing src for send fragment. size = %u",
                 (unsigned int)(*size + reserve)));

    if (OPAL_UNLIKELY(true == use_eager_get)) {
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool, data_ptr, *size, 0,
                                            (mca_mpool_base_registration_t **)&registration);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            mca_btl_ugni_frag_return (frag);
            return NULL;
        }

        frag->flags = MCA_BTL_UGNI_FRAG_EAGER | MCA_BTL_UGNI_FRAG_IGNORE;

        frag->registration = registration;
        frag->segments[1].memory_handle = registration->memory_hdl;

        frag->hdr_size = reserve + sizeof (frag->hdr.eager);
        frag->segments[0].base.seg_addr.pval = frag->hdr.eager_ex.pml_header;
    } else {
        frag->hdr_size = reserve + sizeof (frag->hdr.send);
        frag->segments[0].base.seg_addr.pval = frag->hdr.send_ex.pml_header;
    }

    frag->segments[0].base.seg_len       = reserve;

    frag->segments[1].base.seg_addr.pval = data_ptr;
    frag->segments[1].base.seg_len       = *size;

    frag->base.des_src     = &frag->segments->base;
    frag->base.des_src_cnt = 2;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send_buffered (struct mca_btl_base_module_t *btl,
                                        mca_btl_base_endpoint_t *endpoint,
                                        struct opal_convertor_t *convertor,
                                        uint8_t order, size_t reserve, size_t *size,
                                        uint32_t flags)
{
    bool use_eager_get = (*size + reserve) > mca_btl_ugni_component.smsg_max_data;
    mca_btl_ugni_reg_t *registration = NULL;
    mca_btl_ugni_base_frag_t *frag = NULL;
    uint32_t iov_count = 1;
    struct iovec iov;
    int rc;

    if (OPAL_UNLIKELY(true == use_eager_get)) {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_EAGER_SEND(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->flags = MCA_BTL_UGNI_FRAG_EAGER | MCA_BTL_UGNI_FRAG_IGNORE;

        registration = (mca_btl_ugni_reg_t *) frag->base.super.registration;

        frag->segments[1].memory_handle = registration->memory_hdl;

        frag->hdr_size = reserve + sizeof (frag->hdr.eager);
        frag->segments[0].base.seg_addr.pval = frag->hdr.eager_ex.pml_header;
    } else {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_SMSG(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->hdr_size = reserve + sizeof (frag->hdr.send);
        frag->segments[0].base.seg_addr.pval = frag->hdr.send_ex.pml_header;
    }

    frag->flags |= MCA_BTL_UGNI_FRAG_BUFFERED;

    iov.iov_len  = *size;
    iov.iov_base = (IOVBASE_TYPE *) frag->base.super.ptr;

    rc = opal_convertor_pack (convertor, &iov, &iov_count, size);
    if (OPAL_UNLIKELY(rc < 0)) {
        mca_btl_ugni_frag_return (frag);
        return NULL;
    }

    frag->segments[0].base.seg_len       = reserve;

    frag->segments[1].base.seg_addr.pval = frag->base.super.ptr;
    frag->segments[1].base.seg_len       = *size;

    frag->base.des_src     = &frag->segments->base;
    frag->base.des_src_cnt = 2;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send (struct mca_btl_base_module_t *btl,
                               mca_btl_base_endpoint_t *endpoint,
                               struct opal_convertor_t *convertor,
                               uint8_t order, size_t reserve, size_t *size,
                               uint32_t flags)
{
    bool use_eager_get = (*size + reserve) > mca_btl_ugni_component.smsg_max_data;
    bool send_in_place;
    void *data_ptr;

    if (!(*size)) {
        return mca_btl_ugni_prepare_src_send_nodata (btl, endpoint, order, reserve, flags);
    }

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    send_in_place = !(opal_convertor_need_buffers(convertor) ||
                      (use_eager_get && ((uintptr_t)data_ptr & 3)));

    if (send_in_place) {
        return mca_btl_ugni_prepare_src_send_inplace (btl, endpoint, convertor, order,
                                                      reserve, size, flags);
    } else {
        return mca_btl_ugni_prepare_src_send_buffered (btl, endpoint, convertor, order,
                                                       reserve, size, flags);
    }
}

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_rdma (struct mca_btl_base_module_t *btl,
                               mca_btl_base_endpoint_t *endpoint,
                               mca_mpool_base_registration_t *registration,
                               struct opal_convertor_t *convertor,
                               uint8_t order, size_t *size,
                               uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    /*
     * For medium message use FMA protocols and for large message
     * use BTE protocols
     */
    /* No need to register while using FMA Put (registration is
     * non-null in get-- is this always true?) */
    if (*size >= mca_btl_ugni_component.ugni_fma_limit || (flags & MCA_BTL_DES_FLAGS_GET)) {
        if (NULL == registration) {
            rc = btl->btl_mpool->mpool_register(btl->btl_mpool, data_ptr, *size, 0,
                                                (mca_mpool_base_registration_t **) &registration);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                mca_btl_ugni_frag_return (frag);
                return NULL;
            }

            frag->registration = (mca_btl_ugni_reg_t *) registration;
        }

        frag->segments[0].memory_handle = ((mca_btl_ugni_reg_t *)registration)->memory_hdl;
    } else {
        memset ((void *) &frag->segments[0].memory_handle, 0,
                sizeof (frag->segments[0].memory_handle));
    }

    if ((flags & MCA_BTL_DES_FLAGS_GET) && (*size & 0x3)) {
        memmove (frag->segments[0].extra_bytes, (char *) data_ptr + (*size & ~0x3),
                 *size & 0x3);
        frag->segments[0].extra_byte_count = *size & 0x3;
    } else {
        frag->segments[0].extra_byte_count = 0;
    }

    frag->segments[0].base.seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
    frag->segments[0].base.seg_len       = *size;

    frag->base.des_src     = &frag->segments->base;
    frag->base.des_src_cnt = 1;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    return &frag->base;
}

#endif
