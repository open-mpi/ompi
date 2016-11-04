/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
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

#include "opal_config.h"

#include "btl_ugni.h"
#include "btl_ugni_frag.h"

static inline struct mca_btl_base_descriptor_t *
mca_btl_ugni_prepare_src_send_nodata (struct mca_btl_base_module_t *btl,
                                      mca_btl_base_endpoint_t *endpoint,
                                      uint8_t order, size_t reserve,
                                      uint32_t flags)
{
    mca_btl_ugni_base_frag_t *frag = NULL;

    (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(endpoint, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    BTL_VERBOSE(("preparing src for send fragment. size = %u", (unsigned int) reserve));

    frag->hdr_size = reserve + sizeof (frag->hdr.send);

    frag->segments[0].seg_addr.pval = frag->hdr.send_ex.pml_header;
    frag->segments[0].seg_len       = reserve;

    frag->segments[1].seg_addr.pval = NULL;
    frag->segments[1].seg_len       = 0;

    frag->base.des_segments      = frag->segments;
    frag->base.des_segment_count = 1;
    frag->base.order           = order;
    frag->base.des_flags       = flags;

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
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
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
        rc = ugni_module->rcache->rcache_register (ugni_module->rcache, data_ptr, *size, 0,
                                                   MCA_RCACHE_ACCESS_REMOTE_READ,
                                                   (mca_rcache_base_registration_t **)&registration);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            mca_btl_ugni_frag_return (frag);
            return NULL;
        }

        frag->flags = MCA_BTL_UGNI_FRAG_EAGER | MCA_BTL_UGNI_FRAG_IGNORE;

        frag->registration = registration;
        frag->hdr.eager.memory_handle = registration->handle;;

        frag->hdr_size = reserve + sizeof (frag->hdr.eager);
        frag->segments[0].seg_addr.pval = frag->hdr.eager_ex.pml_header;
    } else {
        frag->hdr_size = reserve + sizeof (frag->hdr.send);
        frag->segments[0].seg_addr.pval = frag->hdr.send_ex.pml_header;
    }

    frag->segments[0].seg_len       = reserve;

    frag->segments[1].seg_addr.pval = data_ptr;
    frag->segments[1].seg_len       = *size;

    frag->base.des_segments       = frag->segments;
    frag->base.des_segment_count = 2;
    frag->base.order           = order;
    frag->base.des_flags       = flags;

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

        frag->hdr.eager.memory_handle = registration->handle;
        frag->hdr_size = reserve + sizeof (frag->hdr.eager);
        frag->segments[0].seg_addr.pval = frag->hdr.eager_ex.pml_header;
    } else {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_SMSG(endpoint, frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->hdr_size = reserve + sizeof (frag->hdr.send);
        frag->segments[0].seg_addr.pval = frag->hdr.send_ex.pml_header;
    }

    frag->flags |= MCA_BTL_UGNI_FRAG_BUFFERED;

    iov.iov_len  = *size;
    iov.iov_base = (IOVBASE_TYPE *) frag->base.super.ptr;

    rc = opal_convertor_pack (convertor, &iov, &iov_count, size);
    if (OPAL_UNLIKELY(rc < 0)) {
        mca_btl_ugni_frag_return (frag);
        return NULL;
    }

    frag->segments[0].seg_len       = reserve;

    frag->segments[1].seg_addr.pval = frag->base.super.ptr;
    frag->segments[1].seg_len       = *size;

    frag->base.des_segments       = frag->segments;
    frag->base.des_segment_count = 2;
    frag->base.order           = order;
    frag->base.des_flags       = flags;

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

#endif
