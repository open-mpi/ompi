/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_endpoint.h"

/**
 * Initiate an synchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_vader_put (struct mca_btl_base_module_t *btl,
		       struct mca_btl_base_endpoint_t *endpoint,
		       struct mca_btl_base_descriptor_t *des)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) des;
    mca_btl_base_segment_t *src = des->des_src;
    mca_btl_base_segment_t *dst = des->des_dst;
    size_t size = min(dst->seg_len, src->seg_len);
    mca_mpool_base_registration_t *reg;
    void *rem_ptr;

    reg = vader_get_registation (endpoint->peer_smp_rank,
				 (void *) dst->seg_key.ptr,
				 dst->seg_len, 0);
    if (OPAL_UNLIKELY(NULL == reg)) {
	return OMPI_ERROR;
    }

    rem_ptr = vader_reg_to_ptr (reg, (void *) dst->seg_key.ptr);

    if (OPAL_LIKELY((uintptr_t)rem_ptr != dst->seg_key.ptr) &&
	dst->seg_len >= mca_btl_vader_memcpy_limit) {
	/* memcpy is faster at certain sizes but is undefined if the
	   pointers are aliased */
	memcpy (rem_ptr, (void *) src->seg_key.ptr, size);
    } else {
	memmove (rem_ptr, (void *) src->seg_key.ptr, size);
    }

    vader_return_registration (reg, endpoint->peer_smp_rank);

    /* always call the callback function (if it exists) */
    if (OPAL_LIKELY(NULL != frag->base.des_cbfunc)) {
        frag->base.des_cbfunc(&mca_btl_vader.super, frag->endpoint,
			      &frag->base, OMPI_SUCCESS);
    }

    if (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        MCA_BTL_VADER_FRAG_RETURN(frag);
    }

    return OMPI_SUCCESS;
}
