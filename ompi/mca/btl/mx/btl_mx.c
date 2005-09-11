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
#include <string.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "btl_mx.h"
#include "btl_mx_frag.h" 
#include "btl_mx_proc.h"
#include "btl_mx_endpoint.h"
#include "datatype/convertor.h" 
#include "mca/mpool/base/base.h" 
#include "mca/mpool/mpool.h" 


mca_btl_mx_module_t mca_btl_mx_module = {
    {
        &mca_btl_mx_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND_INPLACE | MCA_BTL_FLAGS_PUT, /* flags */
        mca_btl_mx_add_procs,
        mca_btl_mx_del_procs,
        mca_btl_mx_register, 
        mca_btl_mx_finalize,
        mca_btl_mx_alloc, 
        mca_btl_mx_free, 
        mca_btl_mx_prepare_src,
        mca_btl_mx_prepare_dst,
        mca_btl_mx_send,
        mca_btl_mx_put,
        NULL /* get */ 
    }
};

/**
 *
 */

int mca_btl_mx_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_mx_proc_t* mx_proc;
        mca_btl_base_endpoint_t* mx_endpoint;

        if( ompi_procs[i] == ompi_proc_local_proc ) {
            /* Do not alllow to connect to ourselfs ... */
            continue;
        }

        if(NULL == (mx_proc = mca_btl_mx_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&mx_proc->proc_lock);

        /* The btl_proc datastructure is shared by all MX BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        mx_endpoint = OBJ_NEW(mca_btl_mx_endpoint_t);
        if(NULL == mx_endpoint) {
            OPAL_THREAD_UNLOCK(&mx_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        mx_endpoint->endpoint_btl = mx_btl;
        rc = mca_btl_mx_proc_insert(mx_proc, mx_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(mx_endpoint);
            OBJ_RELEASE(mx_proc);
            OPAL_THREAD_UNLOCK(&mx_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&mx_proc->proc_lock);
        peers[i] = mx_endpoint;
    }

    return OMPI_SUCCESS;
}

int mca_btl_mx_del_procs(struct mca_btl_base_module_t* btl, 
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

int mca_btl_mx_register( struct mca_btl_base_module_t* btl, 
                         mca_btl_base_tag_t tag, 
                         mca_btl_base_module_recv_cb_fn_t cbfunc, 
                         void* cbdata )
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; 
    mca_btl_mx_frag_t* frag;
    mx_return_t mx_return;
    mx_segment_t mx_segment;
    int i, rc;

    mx_btl->mx_reg[tag].cbfunc = cbfunc; 
    mx_btl->mx_reg[tag].cbdata = cbdata; 
    /*
     * Post the receives
     */
    for( i = 0; i < mca_btl_mx_component.mx_max_posted_recv; i++ ) {
        MCA_BTL_MX_FRAG_ALLOC_EAGER( mx_btl, frag, rc );
        if( NULL == frag ) {
            if( 0 == i ) {
                return OMPI_ERROR;
            }
        }
        frag->base.des_dst     = frag->segment;
        frag->base.des_dst_cnt = 1;
        frag->base.des_src     = NULL;
        frag->base.des_src_cnt = 0;
        frag->mx_frag_list     = NULL;
        frag->tag              = tag;

        mx_segment.segment_ptr = frag->base.des_dst->seg_addr.pval;
        mx_segment.segment_length = frag->base.des_dst->seg_len;
        mx_return = mx_irecv( mx_btl->mx_endpoint, &mx_segment, 1, (uint64_t)tag, 0xffffffffffffffff,
                              frag, &(frag->mx_request) );
        if( MX_SUCCESS != mx_return ) {
            return OMPI_ERROR;
        }
    }

    extern int mx_debug;
    if( mx_debug ) {
#include <unistd.h>
        sleep(20);
    }

    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_mx_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; 
    mca_btl_mx_frag_t* frag;
    int rc;
    
#if 0
    if(size <= mx_btl->super.btl_eager_limit) { 
        MCA_BTL_MX_FRAG_ALLOC_EAGER(mx_btl, frag, rc);
        frag->segment[0].seg_len = 
            size <= mx_btl->super.btl_eager_limit ? 
            size : mx_btl->super.btl_eager_limit ; 
    } else { 
        MCA_BTL_MX_FRAG_ALLOC_USER(mx_btl, frag, rc);
        frag->segment[0].seg_len = 
            size <= mx_btl->super.btl_max_send_size ? 
            size : mx_btl->super.btl_max_send_size ; 
    }
#endif
    MCA_BTL_MX_FRAG_ALLOC_EAGER(mx_btl, frag, rc);
    frag->segment[0].seg_len = 
        size <= mx_btl->super.btl_eager_limit ? 
        size : mx_btl->super.btl_eager_limit ; 
    frag->base.des_src = frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
xo */

int mca_btl_mx_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_mx_frag_t* frag = (mca_btl_mx_frag_t*)des; 
#if MCA_BTL_HAS_MPOOL
    if(frag->size == 0) {
        OBJ_RELEASE(frag->registration);
    }
#endif
    if( 0 == frag->base.des_dst_cnt ) {  /* send fragment */
        MCA_BTL_MX_FRAG_RETURN(btl, frag);
    } else {  /* receive fragment */
        opal_output( 0, "BARFFFFFFF   return send frag\n" );
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
mca_btl_base_descriptor_t* mca_btl_mx_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*)btl;
    mca_btl_mx_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

#if MCA_BTL_HAS_MPOOL
    /*
     * If the data has already been pinned and is contigous than we can
     * use it in place.
    */
    if (NULL != registration && 0 == ompi_convertor_need_buffers(convertor)) {

        size_t reg_len;
        MCA_BTL_MX_FRAG_ALLOC_USER(mx_btl, frag, rc);
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
                                                                                                    
            mca_mpool_base_module_t* mpool = mx_btl->mx_mpool;
            size_t new_len = (unsigned char*)iov.iov_base - 
                (unsigned char *) registration->base + max_data;
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
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
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
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
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
    } else if ((mca_btl_mx_component.leave_pinned || max_data > btl->btl_max_send_size) && 
               ompi_convertor_need_buffers(convertor) == 0 &&
               reserve == 0) {

        mca_mpool_base_module_t* mpool = mx_btl->mx_mpool;
        MCA_BTL_MX_FRAG_ALLOC_USER(mx_btl, frag, rc);
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
            MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }

        if(mca_btl_mx_component.leave_pinned) {
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
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
            OBJ_RETAIN(registration);
        } 
        frag->registration = registration;

    } else
#endif
        /* If the data is contiguous we can user directly the pointer
         * to the user memory.
         */
        if( 0 == ompi_convertor_need_buffers(convertor) ) {
            MCA_BTL_MX_FRAG_ALLOC_USER(btl, frag, rc);
            if( NULL == frag ) {
                return NULL;
            }
            
            if( (max_data + reserve) > btl->btl_eager_limit ) {
                max_data = btl->btl_eager_limit - reserve;
            }
            /* let the convertor figure out the correct pointer depending on the data layout */
            iov.iov_base = NULL;
            iov.iov_len  = max_data;
            frag->base.des_src_cnt = 2;
            frag->segment[0].seg_len = reserve;
        } else {
            MCA_BTL_MX_FRAG_ALLOC_EAGER( mx_btl, frag, rc );
            if( NULL == frag ) {
                return NULL;
            }
                
            if( (max_data + reserve) <= btl->btl_eager_limit ) {
                iov.iov_len = max_data;
            } else {
                iov.iov_len = mca_btl_mx_module.super.btl_eager_limit - reserve;
                max_data = iov.iov_len;  /* let the PML establish the pipeline */
            }
            iov.iov_base = (unsigned char*)frag->segment[0].seg_addr.pval + reserve;
            frag->segment[0].seg_len = reserve;
            frag->base.des_src_cnt = 1;
        }

    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
    *size  = max_data;
    if( rc < 0 ) {
        MCA_BTL_MX_FRAG_RETURN( mx_btl, frag );
        return NULL;
    }
    if( 1 == frag->base.des_src_cnt ) {
        frag->segment[0].seg_len += max_data;
    } else {
        frag->segment[1].seg_addr.pval = iov.iov_base;
        frag->segment[1].seg_len       = max_data;
    }
    frag->base.des_src = frag->segment;
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

mca_btl_base_descriptor_t* mca_btl_mx_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; 
    mca_btl_mx_frag_t* frag;
    int rc;

    MCA_BTL_MX_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    frag->segment[0].seg_len = *size;
    frag->segment[0].seg_addr.pval = convertor->pBaseBuf + convertor->bConverted;

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;

#if MCA_BTL_HAS_MPOOL
    if(NULL != registration) {
        size_t reg_len = (unsigned char*)registration->bound - (unsigned char*)frag->segment.seg_addr.pval + 1;
        if(frag->segment.seg_len > reg_len) {
            mca_mpool_base_module_t* mpool = mx_btl->mx_mpool;
            size_t new_len = (unsigned char*)frag->segment.seg_addr.pval - 
                (unsigned char*) registration->base + 
                frag->segment.seg_len;
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
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
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
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
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

        mca_mpool_base_module_t* mpool = mx_btl->mx_mpool;
        rc = mpool->mpool_register(
            mpool,
            frag->segment.seg_addr.pval,
            frag->segment.seg_len,
            &registration);
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
                                                                                                                   
        if(mca_btl_mx_component.leave_pinned) {
            /*
             * insert the registration into the tree and bump the reference
             * count so that it doesn't go away on completion.
            */
            rc = mca_mpool_base_insert(
                frag->segment.seg_addr.pval,
                frag->segment.seg_len,
                mpool,
                btl,
                registration);
            if(rc != OMPI_SUCCESS) {
                MCA_BTL_MX_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
            OBJ_RETAIN(registration);
        }
        frag->registration = registration;
    }
#endif
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

int mca_btl_mx_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl;
    mca_btl_mx_frag_t* frag = (mca_btl_mx_frag_t*)descriptor;
    mx_segment_t mx_segment[2];
    mx_return_t mx_return;
    uint64_t total_length;

    frag->endpoint = endpoint;
    frag->tag      = tag;
    mx_segment[0].segment_ptr    = descriptor->des_src[0].seg_addr.pval;
    mx_segment[0].segment_length = descriptor->des_src[0].seg_len;
    total_length = mx_segment[0].segment_length;
    if( 2 == descriptor->des_src_cnt ) {
        mx_segment[1].segment_ptr    = descriptor->des_src[1].seg_addr.pval;
        mx_segment[1].segment_length = descriptor->des_src[1].seg_len;
        total_length += mx_segment[1].segment_length;
    }
    mx_return = mx_isend( mx_btl->mx_endpoint, mx_segment, descriptor->des_src_cnt, endpoint->mx_peer_addr,
                          (uint64_t)tag, frag, &frag->mx_request );
    if( MX_SUCCESS != mx_return ) {
        opal_output( 0, "mx_isend fails with error %s\n", mx_strerror(mx_return) );
        return OMPI_ERROR;
    }
#if 0
    if( 4096 > total_length ) {
        mx_status_t mx_status;
        uint32_t mx_result;

        /* let's check for completness */
        mx_return = mx_test( mx_btl->mx_endpoint, &(frag->mx_request), &mx_status, &mx_result );
        if( MX_SUCCESS != mx_return ) 
            return OMPI_SUCCESS;
        /* call the completion callback */
        frag->base.des_cbfunc( &(mx_btl->super), frag->endpoint, &(frag->base), OMPI_SUCCESS);
    }
#endif
    return OMPI_SUCCESS;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_mx_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    /* mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; */
    mca_btl_mx_frag_t* frag = (mca_btl_mx_frag_t*) descriptor; 
    frag->endpoint = endpoint;
    /* TODO */
    return OMPI_ERR_NOT_IMPLEMENTED; 
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_mx_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    /* mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; */
    mca_btl_mx_frag_t* frag = (mca_btl_mx_frag_t*) descriptor; 
    frag->endpoint = endpoint;
    /* TODO */
    return OMPI_ERR_NOT_IMPLEMENTED; 
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_mx_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*) btl; 
    
    OBJ_DESTRUCT( &mx_btl->mx_lock );
    OBJ_DESTRUCT( &mx_btl->mx_peers );
    free(mx_btl);
    return OMPI_SUCCESS;
}

