/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include <string.h>
#include "opal/class/opal_bitmap.h"
#include "opal/mca/btl/btl.h"
#include "opal/datatype/opal_convertor.h" 
#include "opal/mca/mpool/base/base.h" 
#include "opal/mca/mpool/mpool.h" 

#include "btl_tcp.h"
#include "btl_tcp_frag.h" 
#include "btl_tcp_proc.h"
#include "btl_tcp_endpoint.h"

mca_btl_tcp_module_t mca_btl_tcp_module = {
    .super = {
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
        .btl_ft_event = mca_btl_tcp_ft_event
    }
};

/**
 *
 */

int mca_btl_tcp_add_procs( struct mca_btl_base_module_t* btl, 
                           size_t nprocs, 
                           struct opal_proc_t **procs, 
                           struct mca_btl_base_endpoint_t** peers, 
                           opal_bitmap_t* reachable )
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*)btl;
    const opal_proc_t* my_proc; /* pointer to caller's proc structure */
    int i, rc;

    /* get pointer to my proc structure */
    if( NULL == (my_proc = opal_proc_local_get()) )
        return OPAL_ERR_OUT_OF_RESOURCE;

    for(i = 0; i < (int) nprocs; i++) {

        struct opal_proc_t* opal_proc = procs[i];
        mca_btl_tcp_proc_t* tcp_proc;
        mca_btl_base_endpoint_t* tcp_endpoint;

        /* Do not create loopback TCP connections */
        if( my_proc == opal_proc ) {
            continue;
        }

        if(NULL == (tcp_proc = mca_btl_tcp_proc_create(opal_proc))) {
            continue;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this BTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&tcp_proc->proc_lock);

        /* The btl_proc datastructure is shared by all TCP BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        tcp_endpoint = OBJ_NEW(mca_btl_tcp_endpoint_t);
        if(NULL == tcp_endpoint) {
            OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        tcp_endpoint->endpoint_btl = tcp_btl;
        rc = mca_btl_tcp_proc_insert(tcp_proc, tcp_endpoint);
        if(rc != OPAL_SUCCESS) {
            OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);
            OBJ_RELEASE(tcp_endpoint);
            continue;
        }

        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);
        peers[i] = tcp_endpoint;
        opal_list_append(&tcp_btl->tcp_endpoints, (opal_list_item_t*)tcp_endpoint);

        /* we increase the count of MPI users of the event library
           once per peer, so that we are used until we aren't
           connected to a peer */
        opal_progress_event_users_increment();
    }

    return OPAL_SUCCESS;
}

int mca_btl_tcp_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct opal_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** endpoints)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*)btl;
    size_t i;
    for(i=0; i<nprocs; i++) {
        mca_btl_tcp_endpoint_t* tcp_endpoint = endpoints[i];
        if(tcp_endpoint->endpoint_proc != mca_btl_tcp_proc_local()) {
            opal_list_remove_item(&tcp_btl->tcp_endpoints, (opal_list_item_t*)tcp_endpoint);
            OBJ_RELEASE(tcp_endpoint);
        }
        opal_progress_event_users_decrement();
    }
    return OPAL_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_tcp_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_tcp_frag_t* frag = NULL;
    
    if(size <= btl->btl_eager_limit) { 
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag); 
    } else if (size <= btl->btl_max_send_size) { 
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag); 
    }
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL;
    }
    
    frag->segments[0].seg_len = size;
    frag->segments[0].seg_addr.pval = frag+1;

    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;
    frag->base.des_flags = flags; 
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->btl = (mca_btl_tcp_module_t*)btl;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
 */

int mca_btl_tcp_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_tcp_frag_t* frag = (mca_btl_tcp_frag_t*)des; 
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
mca_btl_base_descriptor_t* mca_btl_tcp_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_tcp_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if( OPAL_UNLIKELY(max_data > UINT32_MAX) ) {  /* limit the size to what we support */
        max_data = (size_t)UINT32_MAX;
    }
    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
     */
    if (max_data+reserve <= btl->btl_eager_limit) {
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag);
    } else {
        /* 
         * otherwise pack as much data as we can into a fragment
         * that is the max send size.
         */
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag);
    }
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL;
    }

    frag->segments[0].seg_addr.pval = (frag + 1);
    frag->segments[0].seg_len = reserve;

    frag->base.des_segment_count = 1;
    if(opal_convertor_need_buffers(convertor)) {

        if (max_data + reserve > frag->size) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)(frag->segments[0].seg_addr.pval)) + reserve);
        
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( OPAL_UNLIKELY(rc < 0) ) {
            mca_btl_tcp_free(btl, &frag->base);
            return NULL;
        }
        
        frag->segments[0].seg_len += max_data;

    } else {

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( OPAL_UNLIKELY(rc < 0) ) {
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
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_tcp_send( struct mca_btl_base_module_t* btl,
                      struct mca_btl_base_endpoint_t* endpoint,
                      struct mca_btl_base_descriptor_t* descriptor, 
                      mca_btl_base_tag_t tag )
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t* frag = (mca_btl_tcp_frag_t*)descriptor; 
    int i;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->iov_cnt = 1;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->hdr.size = 0;
    for( i = 0; i < (int)frag->base.des_segment_count; i++) {
        frag->hdr.size += frag->segments[i].seg_len;
        frag->iov[i+1].iov_len = frag->segments[i].seg_len;
        frag->iov[i+1].iov_base = (IOVBASE_TYPE*)frag->segments[i].seg_addr.pval;
        frag->iov_cnt++;
    }
    frag->hdr.base.tag = tag;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_SEND;
    frag->hdr.count = 0;
    if (endpoint->endpoint_nbo) MCA_BTL_TCP_HDR_HTON(frag->hdr);
    return mca_btl_tcp_endpoint_send(endpoint,frag);
}

static void fake_rdma_complete (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                                mca_btl_base_descriptor_t *desc, int rc)
{
    mca_btl_tcp_frag_t *frag = (mca_btl_tcp_frag_t *) desc;

    frag->cb.func (btl, endpoint, frag->segments[0].seg_addr.pval, NULL, frag->cb.context, frag->cb.data,
                   rc);
}

/**
 * Initiate an asynchronous put.
 */

int mca_btl_tcp_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
		     uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
		     mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
		     int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t *frag = NULL;
    int i;

    MCA_BTL_TCP_FRAG_ALLOC_USER(frag);
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return OPAL_ERR_OUT_OF_RESOURCE;;
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
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE*) (frag->segments + 1);
    frag->iov[1].iov_len = sizeof(mca_btl_base_segment_t);
    for( i = 0; i < (int)frag->base.des_segment_count; i++ ) {
        frag->hdr.size += frag->segments[i].seg_len;
        frag->iov[i+2].iov_len = frag->segments[i].seg_len;
        frag->iov[i+2].iov_base = (IOVBASE_TYPE*)frag->segments[i].seg_addr.pval;
        frag->iov_cnt++;
    }
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_PUT;
    frag->hdr.count = 1;
    if (endpoint->endpoint_nbo) MCA_BTL_TCP_HDR_HTON(frag->hdr);
    return ((i = mca_btl_tcp_endpoint_send(endpoint,frag)) >= 0 ? OPAL_SUCCESS : i);
}


/**
 * Initiate an asynchronous get.
 */

int mca_btl_tcp_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
		     uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
		     mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
		     int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t* frag = NULL;
    int rc;

    MCA_BTL_TCP_FRAG_ALLOC_USER(frag);
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return OPAL_ERR_OUT_OF_RESOURCE;;
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
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE*) &frag->segments[1];
    frag->iov[1].iov_len = sizeof(mca_btl_base_segment_t);
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_GET;
    frag->hdr.count = 1;
    if (endpoint->endpoint_nbo) MCA_BTL_TCP_HDR_HTON(frag->hdr);
    return ((rc = mca_btl_tcp_endpoint_send(endpoint,frag)) >= 0 ? OPAL_SUCCESS : rc);
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_tcp_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    opal_list_item_t* item;
    for( item = opal_list_remove_first(&tcp_btl->tcp_endpoints);
         item != NULL;
         item = opal_list_remove_first(&tcp_btl->tcp_endpoints)) {
        mca_btl_tcp_endpoint_t *endpoint = (mca_btl_tcp_endpoint_t*)item;
        OBJ_RELEASE(endpoint);
        opal_progress_event_users_decrement();
    }
    free(tcp_btl);
    return OPAL_SUCCESS;
}
