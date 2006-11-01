/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "btl_tcp.h"
#include "btl_tcp_frag.h" 
#include "btl_tcp_proc.h"
#include "btl_tcp_endpoint.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/proc/proc.h"

mca_btl_tcp_module_t mca_btl_tcp_module = {
    {
        &mca_btl_tcp_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_tcp_add_procs,
        mca_btl_tcp_del_procs,
        mca_btl_tcp_register, 
        mca_btl_tcp_finalize,
        mca_btl_tcp_alloc, 
        mca_btl_tcp_free, 
        mca_btl_tcp_prepare_src,
        mca_btl_tcp_prepare_dst,
        mca_btl_tcp_send,
        mca_btl_tcp_put,
        NULL, /* get */ 
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL /* register error */
    }
};

/**
 *
 */

int mca_btl_tcp_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*)btl;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    int i, rc;

    /* get pointer to my proc structure */
    my_proc = ompi_proc_local();
    if( NULL == my_proc ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_tcp_proc_t* tcp_proc;
        mca_btl_base_endpoint_t* tcp_endpoint;

        /* Do not create loopback TCP connections */
        if( my_proc == ompi_proc ) continue;

        if(NULL == (tcp_proc = mca_btl_tcp_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
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
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        tcp_endpoint->endpoint_btl = tcp_btl;
        rc = mca_btl_tcp_proc_insert(tcp_proc, tcp_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(tcp_endpoint);
            OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&tcp_proc->proc_lock);
        peers[i] = tcp_endpoint;
        opal_list_append(&tcp_btl->tcp_endpoints, (opal_list_item_t*)tcp_endpoint);

        /* we increase the count of MPI users of the event library
           once per peer, so that we are used until we aren't
           connected to a peer */
        opal_progress_event_increment();
    }

    return OMPI_SUCCESS;
}

int mca_btl_tcp_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
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
        opal_progress_event_decrement();
    }
    return OMPI_SUCCESS;
}


/**
 * Register callback function to support send/recv semantics
 */

int mca_btl_tcp_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    tcp_btl->tcp_reg[tag].cbfunc = cbfunc; 
    tcp_btl->tcp_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_tcp_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_tcp_frag_t* frag;
    int rc;
    
    if(size <= btl->btl_eager_limit) { 
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag, rc); 
        frag->segments[0].seg_len = size;
    } else if (size <= btl->btl_max_send_size) { 
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag, rc); 
        frag->segments[0].seg_len = size;
    } else { 
        return NULL;
    }
    
    frag->segments[0].seg_addr.pval = frag+1;

    frag->base.des_src = frag->segments;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0; 
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
    return OMPI_SUCCESS; 
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
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_tcp_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
    */

    if (max_data+reserve <= btl->btl_eager_limit) {
        MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag, rc);
    }

    /* 
     * otherwise pack as much data as we can into a fragment
     * that is the max send size.
     */
    else {
        MCA_BTL_TCP_FRAG_ALLOC_MAX(frag, rc);
    }
    if(NULL == frag) {
        return NULL;
    }

    if(max_data == 0) {

        frag->segments[0].seg_addr.pval = (frag + 1);
        frag->segments[0].seg_len = reserve;
        frag->base.des_src_cnt = 1;

    } else if(ompi_convertor_need_buffers(convertor)) {

        if (max_data + reserve > frag->size) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)(frag+1)) + reserve);

        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( rc < 0 ) {
            mca_btl_tcp_free(btl, &frag->base);
            return NULL;
        }

        frag->segments[0].seg_addr.pval = (frag + 1);
        frag->segments[0].seg_len = max_data + reserve;
        frag->base.des_src_cnt = 1;

    } else {

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( rc < 0 ) {
            mca_btl_tcp_free(btl, &frag->base);
            return NULL;
        }

        frag->segments[0].seg_addr.pval = frag+1;
        frag->segments[0].seg_len = reserve;
        frag->segments[1].seg_addr.pval = iov.iov_base;
        frag->segments[1].seg_len = max_data;
        frag->base.des_src_cnt = 2;
    }

    frag->base.des_src = frag->segments;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
    *size = max_data;
    return &frag->base;
}


/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */

mca_btl_base_descriptor_t* mca_btl_tcp_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_tcp_frag_t* frag;
    ptrdiff_t lb;
    int rc;

    MCA_BTL_TCP_FRAG_ALLOC_USER(frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segments->seg_len = *size;
    frag->segments->seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted;

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = frag->segments;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
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

int mca_btl_tcp_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t* frag = (mca_btl_tcp_frag_t*)descriptor; 
    size_t i;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->iov_cnt = 1;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->hdr.size = 0;
    for(i=0; i<frag->base.des_src_cnt; i++) {
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


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_tcp_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t* frag = (mca_btl_tcp_frag_t*)descriptor; 
    size_t i;

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->hdr.size = 0;
    frag->iov_cnt = 2;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE*)frag->base.des_dst;
    frag->iov[1].iov_len = frag->base.des_dst_cnt * sizeof(mca_btl_base_segment_t);
    for(i=0; i<frag->base.des_src_cnt; i++) {
        frag->hdr.size += frag->segments[i].seg_len;
        frag->iov[i+2].iov_len = frag->segments[i].seg_len;
        frag->iov[i+2].iov_base = (IOVBASE_TYPE*)frag->segments[i].seg_addr.pval;
        frag->iov_cnt++;
    }
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_PUT;
    frag->hdr.count = frag->base.des_dst_cnt;
    if (endpoint->endpoint_nbo) MCA_BTL_TCP_HDR_HTON(frag->hdr);
    return mca_btl_tcp_endpoint_send(endpoint,frag);
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_tcp_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_tcp_module_t* tcp_btl = (mca_btl_tcp_module_t*) btl; 
    mca_btl_tcp_frag_t* frag = (mca_btl_tcp_frag_t*)descriptor; 

    frag->btl = tcp_btl;
    frag->endpoint = endpoint;
    frag->rc = 0;
    frag->iov_idx = 0;
    frag->hdr.size = 0;
    frag->iov_cnt = 2;
    frag->iov_ptr = frag->iov;
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;
    frag->iov[0].iov_len = sizeof(frag->hdr);
    frag->iov[1].iov_base = (IOVBASE_TYPE*)frag->base.des_src;
    frag->iov[1].iov_len = frag->base.des_src_cnt * sizeof(mca_btl_base_segment_t);
    frag->hdr.base.tag = MCA_BTL_TAG_BTL;
    frag->hdr.type = MCA_BTL_TCP_HDR_TYPE_GET;
    frag->hdr.count = frag->base.des_src_cnt;
    if (endpoint->endpoint_nbo) MCA_BTL_TCP_HDR_HTON(frag->hdr);
    return mca_btl_tcp_endpoint_send(endpoint,frag);
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
        opal_progress_event_decrement();
    }
    free(tcp_btl);
    return OMPI_SUCCESS;
}

