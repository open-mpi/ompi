/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "opal/class/opal_bitmap.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/sys/atomic.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/base/base.h"
#include "btl_self.h"
#include "btl_self_frag.h"
#include "opal/util/proc.h"

static int mca_btl_self_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                             uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                             mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                             int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

static int mca_btl_self_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                             uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                             mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                             int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

mca_btl_base_module_t mca_btl_self = {
    .btl_component = &mca_btl_self_component.super,
    .btl_add_procs = mca_btl_self_add_procs,
    .btl_del_procs = mca_btl_self_del_procs,
    .btl_finalize = mca_btl_self_finalize,
    .btl_alloc = mca_btl_self_alloc,
    .btl_free = mca_btl_self_free,
    .btl_prepare_src = mca_btl_self_prepare_src,
    .btl_send = mca_btl_self_send,
    .btl_put = mca_btl_self_put,
    .btl_get = mca_btl_self_get,
    .btl_dump = mca_btl_base_dump,
    .btl_ft_event = mca_btl_self_ft_event,
};


int mca_btl_self_add_procs( struct mca_btl_base_module_t* btl, 
                            size_t nprocs, 
                            struct opal_proc_t **procs, 
                            struct mca_btl_base_endpoint_t **peers,
                            opal_bitmap_t* reachability )
{
    int i;

    for( i = 0; i < (int)nprocs; i++ ) {
        if( 0 == opal_compare_proc(procs[i]->proc_name, OPAL_PROC_MY_NAME) ) {
            opal_bitmap_set_bit( reachability, i );
            break;  /* there will always be only one ... */
        }
    }
    return OPAL_SUCCESS;
}


int mca_btl_self_del_procs( struct mca_btl_base_module_t* btl, 
                            size_t nprocs,
                            struct opal_proc_t **procs, 
                            struct mca_btl_base_endpoint_t **peers )
{
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

int mca_btl_self_finalize(struct mca_btl_base_module_t* btl)
{
    return OPAL_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t* mca_btl_self_alloc(
        struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* endpoint,
        uint8_t order,
        size_t size,
        uint32_t flags)
{
    mca_btl_self_frag_t* frag = NULL;

    if(size <= mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_ALLOC_EAGER(frag);
    } else if (size <= btl->btl_max_send_size) {
        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag);
    }
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL; 
    }
    
    frag->segment.seg_len = size;
    frag->base.des_flags       = flags;
    frag->base.des_segments       = &(frag->segment);
    frag->base.des_segment_count = 1;
    return (mca_btl_base_descriptor_t*)frag;
}
                                                                                                                   
/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_self_free( struct mca_btl_base_module_t* btl,
                       mca_btl_base_descriptor_t* des )
{
    mca_btl_self_frag_t* frag = (mca_btl_self_frag_t*)des;

    frag->base.des_segments        = NULL;
    frag->base.des_segment_count  = 0;

    if(frag->size == mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_RETURN_EAGER(frag);
    } else if (frag->size == mca_btl_self.btl_max_send_size) {
        MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
    } else {
        MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
    }
    return OPAL_SUCCESS;
}


/**
 * Prepare data for send/put
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t*
mca_btl_self_prepare_src( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct opal_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags )
{
    mca_btl_self_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    /* non-contigous data */
    if( opal_convertor_need_buffers(convertor) ||
        max_data < mca_btl_self.btl_max_send_size ||
        reserve != 0 ) {

        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        if(reserve + max_data > frag->size) {
            max_data = frag->size - reserve;
        } 
        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)(frag+1) + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = frag+1;
        frag->segment.seg_len = reserve + max_data;
        *size = max_data;
    } else {
        MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        /* convertor should return offset into users buffer */
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
            return NULL;
        }
        frag->segment.seg_addr.lval = (uint64_t)(uintptr_t) iov.iov_base;
        frag->segment.seg_len = max_data;
        *size = max_data;
    }
    frag->base.des_flags = flags;
    frag->base.des_segments       = &frag->segment;
    frag->base.des_segment_count = 1;

    return &frag->base;
}
 
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

int mca_btl_self_send( struct mca_btl_base_module_t* btl,
                       struct mca_btl_base_endpoint_t* endpoint,
                       struct mca_btl_base_descriptor_t* des,
                       mca_btl_base_tag_t tag )
{
    mca_btl_active_message_callback_t* reg;
    int btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

    /* upcall */
    reg = mca_btl_base_active_message_trigger + tag;
    reg->cbfunc( btl, tag, des, reg->cbdata );

    /* send completion */
    if( des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK ) {
        des->des_cbfunc( btl, endpoint, des, OPAL_SUCCESS );
    }
    if( btl_ownership ) {
        mca_btl_self_free( btl, des );
    }
    return 1;
}


static int mca_btl_self_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                             uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                             mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                             int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    memcpy ((void *)(intptr_t) remote_address, local_address, size);

    cbfunc (btl, endpoint, local_address, NULL, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}

static int mca_btl_self_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                             uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                             mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                             int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    memcpy (local_address, (void *)(intptr_t) remote_address, size);

    cbfunc (btl, endpoint, local_address, NULL, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}

int mca_btl_self_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OPAL_SUCCESS;
}
