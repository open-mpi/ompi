/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "gm_config.h"
#include <string.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "btl_gm.h"
#include "btl_gm_frag.h" 
#include "btl_gm_proc.h"
#include "btl_gm_endpoint.h"
#include "datatype/convertor.h" 
#include "mca/mpool/base/base.h" 
#include "mca/mpool/mpool.h" 


mca_btl_gm_module_t mca_btl_gm_module = {
    {
        &mca_btl_gm_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_gm_add_procs,
        mca_btl_gm_del_procs,
        mca_btl_gm_register, 
        mca_btl_gm_finalize,
        mca_btl_gm_alloc, 
        mca_btl_gm_free, 
        mca_btl_gm_prepare_src,
        mca_btl_gm_prepare_dst,
        mca_btl_gm_send,
        mca_btl_gm_put,
        NULL /* get */ 
    }
};


/**
 *
 */

int mca_btl_gm_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_gm_proc_t* gm_proc;
        mca_btl_base_endpoint_t* gm_endpoint;

        if(ompi_proc == ompi_proc_local()) 
            continue;

        if(NULL == (gm_proc = mca_btl_gm_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&gm_proc->proc_lock);

        /* The btl_proc datastructure is shared by all GM PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        gm_endpoint = OBJ_NEW(mca_btl_gm_endpoint_t);
        if(NULL == gm_endpoint) {
            OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        gm_endpoint->endpoint_btl = gm_btl;
        rc = mca_btl_gm_proc_insert(gm_proc, gm_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(gm_endpoint);
            OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
            continue;
        }
        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = gm_endpoint;
    }
    return OMPI_SUCCESS;
}

int mca_btl_gm_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OMPI_SUCCESS;
}


/**
 * Register callback function to support send/recv semantics
 */

int mca_btl_gm_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
    gm_btl->gm_reg[tag].cbfunc = cbfunc; 
    gm_btl->gm_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_gm_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
    mca_btl_gm_frag_t* frag;
    int rc;
    
    if(size <= btl->btl_eager_limit) { 
        MCA_BTL_GM_FRAG_ALLOC_EAGER(gm_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_eager_limit ? 
            size : btl->btl_eager_limit ; 
    } else { 
        MCA_BTL_GM_FRAG_ALLOC_MAX(gm_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_max_send_size ? 
            size : btl->btl_max_send_size ; 
    }
    
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0; 
    return &frag->base;
}


/**
 * Return a segment
 */

int mca_btl_gm_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)des; 
    if(frag->size == 0) {
        OBJ_RELEASE(frag->registration);
        MCA_BTL_GM_FRAG_RETURN_USER(btl, frag); 
    } else if(frag->size == mca_btl_gm_component.gm_eager_frag_size) {
        MCA_BTL_GM_FRAG_RETURN_EAGER(btl, frag); 
    } else if(frag->size == mca_btl_gm_component.gm_max_frag_size) {
        MCA_BTL_GM_FRAG_RETURN_MAX(btl, frag); 
    }  else {
        opal_output(0, "[%s:%d] mca_btl_gm_free: invalid descriptor\n", __FILE__,__LINE__);
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS; 
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_gm_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*)btl;
    mca_btl_gm_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

#if OMPI_MCA_BTL_GM_SUPPORT_REGISTERING && \
    (OMPI_MCA_BTL_GM_HAVE_RDMA_GET || OMPI_MCA_BTL_GM_HAVE_RDMA_PUT)
    /*
     * If the data has already been pinned and is contigous than we can
     * use it in place.
    */
    if (NULL != registration && 0 == ompi_convertor_need_buffers(convertor)) {

        size_t reg_len;
        MCA_BTL_GM_FRAG_ALLOC_USER(gm_btl, frag, rc);
        if(NULL == frag){
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
                                                                                                    
        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;

        reg_len = (unsigned char*)registration->bound - (unsigned char*)iov.iov_base + 1;
        if(frag->segment.seg_len > reg_len) {
                                                                                                    
            mca_mpool_base_module_t* mpool = gm_btl->gm_mpool;
            size_t new_len = (unsigned char*)iov.iov_base - registration->base + max_data;
            void* base_addr = registration->base;

            /* remove old registration from tree and decrement reference count */
            mca_mpool_base_remove(base_addr);
            OBJ_RELEASE(registration);

            /* re-register at new size */
            rc = mpool->mpool_register(
                mpool,
                base_addr,
                new_len,
                &registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                return NULL;
            }

            /* re-insert into tree with new registration */
            rc = mca_mpool_base_insert(
                base_addr,
                new_len,
                mpool,
                btl,
                registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
        } 

        /* bump reference count as so that the registration
         * doesn't go away when the operation completes
         */
        OBJ_RETAIN(registration);
        frag->registration = registration;

    /*
     * if the data is not already pinned - but the leave pinned option is set,
     * then go ahead and pin contigous data. however, if a reserve is required 
     * then we must allocated a fragment w/ buffer space
    */
    } else if ((mca_btl_gm_component.leave_pinned || max_data > btl->btl_max_send_size) && 
               ompi_convertor_need_buffers(convertor) == 0 &&
               reserve == 0) {

        mca_mpool_base_module_t* mpool = gm_btl->gm_mpool;
        MCA_BTL_GM_FRAG_ALLOC_USER(gm_btl, frag, rc);
        if(NULL == frag){
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
                                                                                                
        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;

        rc = mpool->mpool_register(
            mpool,
            iov.iov_base,
            max_data,
            &registration);
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }

        if(mca_btl_gm_component.leave_pinned) {
            /*
             * insert the registration into the tree and bump the reference
             * count so that it doesn't go away on completion.
            */
            rc = mca_mpool_base_insert(
                iov.iov_base,
                iov.iov_len,
                mpool,
                btl,
                registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
            OBJ_RETAIN(registration);
        } 
        frag->registration = registration;
    } 

    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
    */
    else 
#endif

    if (max_data+reserve <= btl->btl_eager_limit) {
                                                                                                    
        MCA_BTL_GM_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
                                                                                                    
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_GM_FRAG_RETURN_EAGER(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    /* 
     * otherwise pack as much data as we can into a fragment
     * that is the max send size.
     */
    else {
                                                                                                    
        MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        if(max_data + reserve > btl->btl_max_send_size){
            max_data = btl->btl_max_send_size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        *size  = max_data;
                                                                                                    
        if( rc < 0 ) {
            MCA_BTL_GM_FRAG_RETURN_MAX(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
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

mca_btl_base_descriptor_t* mca_btl_gm_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
#if OMPI_MCA_BTL_GM_SUPPORT_REGISTERING && \
    (OMPI_MCA_BTL_GM_HAVE_RDMA_GET || OMPI_MCA_BTL_GM_HAVE_RDMA_PUT)
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
    mca_btl_gm_frag_t* frag;
    int rc;

    MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    frag->segment.seg_len = *size;
    frag->segment.seg_addr.pval = convertor->pBaseBuf + convertor->bConverted;

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;

    if(NULL != registration) {
        size_t reg_len = (unsigned char*)registration->bound - (unsigned char*)frag->segment.seg_addr.pval + 1;
        if(frag->segment.seg_len > reg_len) {
            mca_mpool_base_module_t* mpool = gm_btl->gm_mpool;
            size_t new_len = (unsigned char*)frag->segment.seg_addr.pval - registration->base + frag->segment.seg_len;
            void* base_addr = registration->base;

            /* remove old registration from tree and decrement reference count */
            mca_mpool_base_remove(base_addr);
            OBJ_RELEASE(registration);

            /* re-register at new size */
            rc = mpool->mpool_register(
                mpool,
                base_addr,
                new_len,
                &registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                return NULL;
            }

            /* re-insert into tree with new registration */
            rc = mca_mpool_base_insert(
                base_addr,
                new_len,
                mpool,
                btl,
                registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
        }

        /* bump reference count as so that the registration
         * doesn't go away when the operation completes
         */
        OBJ_RETAIN(registration);
        frag->registration = registration;

    }  else {

        mca_mpool_base_module_t* mpool = gm_btl->gm_mpool;
        rc = mpool->mpool_register(
            mpool,
            frag->segment.seg_addr.pval,
            frag->segment.seg_len,
            &registration);
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
                                                                                                                   
        if(mca_btl_gm_component.leave_pinned) {
            /*
             * insert the registration into the tree and bump the reference
             * count so that it doesn't go away on completion.
            */
            OBJ_RETAIN(registration);
            rc = mca_mpool_base_insert(
                frag->segment.seg_addr.pval,
                frag->segment.seg_len,
                mpool,
                btl,
                registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_GM_FRAG_RETURN_USER(btl,frag);
                /* release twice */
                OBJ_RELEASE(registration); 
                OBJ_RELEASE(registration);
                return NULL;
            }
        }
        frag->registration = registration;
    }
    return &frag->base;
#else
    return NULL;
#endif
}


/**
 *
 */

static void mca_btl_gm_drop_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_btl_gm_module_t* btl = (mca_btl_gm_module_t*)context;
    OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );
}

static void mca_btl_gm_send_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)context;
    mca_btl_gm_module_t* btl = frag->btl;

    switch(status) {
        case GM_TRY_AGAIN:
        case GM_SEND_TIMED_OUT:
        case GM_TIMED_OUT:
            /* drop all sends to this destination port */
            gm_drop_sends(
                btl->port,
                (frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY) ? GM_HIGH_PRIORITY : GM_LOW_PRIORITY,
                frag->endpoint->endpoint_addr.node_id,
                frag->endpoint->endpoint_addr.port_id,
                mca_btl_gm_drop_callback,
                btl
            );

            /* retry the failed fragment */
            mca_btl_gm_send(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);
            break;
        case GM_SEND_DROPPED:
            /* release the send token */
            OPAL_THREAD_ADD32(&btl->gm_num_send_tokens, 1);

            /* retry the dropped fragment */
            mca_btl_gm_send(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);
            break;
        case GM_SUCCESS:
            /* release the send token */
            OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );

            /* call the completion callback */
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);

            /* check for pending fragments */
            if(opal_list_get_size(&btl->gm_pending)) {
                OPAL_THREAD_LOCK(&btl->gm_lock);
                frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&btl->gm_pending);
                OPAL_THREAD_UNLOCK(&btl->gm_lock);
                mca_btl_gm_send(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);
            }
            break;
 
        default:
            /* error condition can't deal with */
            opal_output(0, "[%s:%d] send completed with unhandled gm error %d\n", __FILE__,__LINE__,status);

            /* release the send token */
            OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );

            /* call the completion callback */
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_ERROR);
            break;
    }
}


static void mca_btl_gm_rdma_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)context;
    mca_btl_gm_module_t* btl = frag->btl;

    /* call the completion callback */
    switch(status) {
        case GM_SUCCESS:
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);
            break;
        default:
            opal_output(0, "[%s:%d] gm rdma operation failed with status %d\n", __FILE__, __LINE__, status);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_ERROR);
            break;
    }
}


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_gm_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl;
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)des; 

    frag->btl = gm_btl;
    frag->endpoint = endpoint; 
    frag->hdr->tag = tag;

    /* queue the descriptor if there are no send tokens */
    if(OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, -1) < 0) {
        OPAL_THREAD_LOCK(&gm_btl->gm_lock);
        opal_list_append(&gm_btl->gm_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&gm_btl->gm_lock);
        OPAL_THREAD_ADD32(&gm_btl->gm_num_send_tokens, 1);
        return OMPI_SUCCESS;
    }
    
    /* post the send request */
    gm_send_with_callback(
        gm_btl->port,
        frag->hdr,
        frag->size,
        frag->segment.seg_len + sizeof(mca_btl_base_header_t),
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_send_callback,
        frag);

    if(opal_list_get_size(&gm_btl->gm_repost)) {
        mca_btl_gm_frag_t* frag;
        OPAL_THREAD_LOCK(&btl->gm_lock);
        while(NULL != (frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&gm_btl->gm_repost))) {
            gm_provide_receive_buffer(gm_btl->port, frag->hdr, frag->size, frag->priority);
        }
        OPAL_THREAD_UNLOCK(&btl->gm_lock);
    }
    return OMPI_SUCCESS;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_gm_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
#if OMPI_MCA_BTL_GM_HAVE_RDMA_PUT
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl;
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*) des; 

    frag->btl = gm_btl;
    frag->endpoint = endpoint;

    gm_put(gm_btl->port,
        des->des_src->seg_addr.pval,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_rdma_callback,
        frag);
    return OMPI_SUCCESS; 
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_gm_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
#if OMPI_MCA_BTL_GM_HAVE_RDMA_GET
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl;
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*) des; 

    frag->btl = gm_btl;
    frag->endpoint = endpoint;

    gm_put(gm_btl->port,
        des->des_src->seg_addr.pval,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_rdma_callback,
        frag);
    return OMPI_SUCCESS; 
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_gm_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
    
    if(gm_btl->gm_frag_eager.fl_num_allocated != 
       gm_btl->gm_frag_eager.super.opal_list_length){ 
        opal_output(0, "btl gm_frag_eager: %d allocated %d returned \n", 
                    gm_btl->gm_frag_eager.fl_num_allocated, 
                    gm_btl->gm_frag_eager.super.opal_list_length); 
    }
    if(gm_btl->gm_frag_max.fl_num_allocated != 
      gm_btl->gm_frag_max.super.opal_list_length) { 
        opal_output(0, "btl gm_frag_max: %d allocated %d returned \n", 
                    gm_btl->gm_frag_max.fl_num_allocated, 
                    gm_btl->gm_frag_max.super.opal_list_length); 
    }
    if(gm_btl->gm_frag_user.fl_num_allocated != 
       gm_btl->gm_frag_user.super.opal_list_length){ 
        opal_output(0, "btl gm_frag_user: %d allocated %d returned \n", 
                    gm_btl->gm_frag_user.fl_num_allocated, 
                    gm_btl->gm_frag_user.super.opal_list_length); 
    }

    OBJ_DESTRUCT(&gm_btl->gm_lock);
    OBJ_DESTRUCT(&gm_btl->gm_frag_eager);
    OBJ_DESTRUCT(&gm_btl->gm_frag_max);
    OBJ_DESTRUCT(&gm_btl->gm_frag_user);
    free(gm_btl);
    return OMPI_SUCCESS;
}

