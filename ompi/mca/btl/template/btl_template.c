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
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "btl_template.h"
#include "btl_template_frag.h" 
#include "btl_template_proc.h"
#include "btl_template_endpoint.h"
#include "datatype/convertor.h" 
#include "mca/mpool/base/base.h" 
#include "mca/mpool/mpool.h" 


mca_btl_template_module_t mca_btl_template_module = {
    {
        &mca_btl_template_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_template_add_procs,
        mca_btl_template_del_procs,
        mca_btl_template_register, 
        mca_btl_template_finalize,
        mca_btl_template_alloc, 
        mca_btl_template_free, 
        mca_btl_template_prepare_src,
        mca_btl_template_prepare_dst,
        mca_btl_template_send,
        mca_btl_template_put,
        NULL /* get */ 
    }
};

/**
 *
 */

int mca_btl_template_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_template_proc_t* template_proc;
        mca_btl_base_endpoint_t* template_endpoint;

        if(NULL == (template_proc = mca_btl_template_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&template_proc->proc_lock);

        /* The btl_proc datastructure is shared by all TEMPLATE PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        template_endpoint = OBJ_NEW(mca_btl_template_endpoint_t);
        if(NULL == template_endpoint) {
            OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        template_endpoint->endpoint_btl = template_btl;
        rc = mca_btl_template_proc_insert(template_proc, template_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(template_endpoint);
            OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = template_endpoint;
    }

    return OMPI_SUCCESS;
}

int mca_btl_template_del_procs(struct mca_btl_base_module_t* btl, 
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

int mca_btl_template_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    template_btl->template_reg[tag].cbfunc = cbfunc; 
    template_btl->template_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_template_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    mca_btl_template_frag_t* frag;
    int rc;
    
    if(size <= btl->btl_eager_limit){ 
        MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(template_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_eager_limit ? 
            size : btl->btl_eager_limit ; 
    } else { 
        MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(template_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_max_send_size ? 
            size : btl->btl_max_send_size ; 
    }
    
    frag->base.des_flags = 0; 
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
 */

int mca_btl_template_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_template_frag_t* frag = (mca_btl_template_frag_t*)des; 
    if(frag->size == 0) {
#if MCA_BTL_HAS_MPOOL
        OBJ_RELEASE(frag->registration);
#endif
        MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl, frag); 
    } else if(frag->size == btl->btl_eager_limit){ 
        MCA_BTL_TEMPLATE_FRAG_RETURN_EAGER(btl, frag); 
    } else if(frag->size == btl->btl_max_send_size) {
        MCA_BTL_TEMPLATE_FRAG_RETURN_EAGER(btl, frag); 
    }  else {
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
mca_btl_base_descriptor_t* mca_btl_template_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*)btl;
    mca_btl_template_frag_t* frag;
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
        MCA_BTL_TEMPLATE_FRAG_ALLOC_USER(template_btl, frag, rc);
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
                                                                                                    
            mca_mpool_base_module_t* mpool = template_btl->template_mpool;
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
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
    } else if ((mca_btl_template_component.leave_pinned || max_data > btl->btl_max_send_size) && 
               ompi_convertor_need_buffers(convertor) == 0 &&
               reserve == 0) {

        mca_mpool_base_module_t* mpool = template_btl->template_mpool;
        MCA_BTL_TEMPLATE_FRAG_ALLOC_USER(template_btl, frag, rc);
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
            MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }

        if(mca_btl_template_component.leave_pinned) {
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
                OBJ_RELEASE(registration);
                return NULL;
            }
            OBJ_RETAIN(registration);
        } 
        frag->registration = registration;

    } else 
#endif

    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
    */
    if (max_data+reserve <= btl->btl_eager_limit) {
                                                                                                    
        MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
                                                                                                    
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_TEMPLATE_FRAG_RETURN_EAGER(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    /* 
     * otherwise pack as much data as we can into a fragment
     * that is the max send size.
     */
    else {
                                                                                                    
        MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        if(max_data + reserve > frag->size){
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        *size  = max_data;
                                                                                                    
        if( rc < 0 ) {
            MCA_BTL_TEMPLATE_FRAG_RETURN_MAX(btl, frag);
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

mca_btl_base_descriptor_t* mca_btl_template_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    mca_btl_template_frag_t* frag;
    int rc;

    MCA_BTL_TEMPLATE_FRAG_ALLOC_USER(btl, frag, rc);
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

#if MCA_BTL_HAS_MPOOL
    if(NULL != registration) {
        size_t reg_len = (unsigned char*)registration->bound - (unsigned char*)frag->segment.seg_addr.pval + 1;
        if(frag->segment.seg_len > reg_len) {
            mca_mpool_base_module_t* mpool = template_btl->template_mpool;
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
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

        mca_mpool_base_module_t* mpool = template_btl->template_mpool;
        rc = mpool->mpool_register(
            mpool,
            frag->segment.seg_addr.pval,
            frag->segment.seg_len,
            &registration);
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
                                                                                                                   
        if(mca_btl_template_component.leave_pinned) {
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
                MCA_BTL_TEMPLATE_FRAG_RETURN_USER(btl,frag);
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

int mca_btl_template_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
   
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    mca_btl_template_frag_t* frag = (mca_btl_template_frag_t*)descriptor; 
    frag->endpoint = endpoint; 
    /* TODO */
    return OMPI_ERR_NOT_IMPLEMENTED;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_template_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    mca_btl_template_frag_t* frag = (mca_btl_template_frag_t*) descriptor; 
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

int mca_btl_template_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    mca_btl_template_frag_t* frag = (mca_btl_template_frag_t*) descriptor; 
    frag->endpoint = endpoint;
    /* TODO */
    return OMPI_ERR_NOT_IMPLEMENTED; 
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_template_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    
    if(template_btl->template_frag_eager.fl_num_allocated != 
       template_btl->template_frag_eager.super.opal_list_length){ 
        opal_output(0, "btl template_frag_eager: %d allocated %d returned \n", 
                    template_btl->template_frag_eager.fl_num_allocated, 
                    template_btl->template_frag_eager.super.opal_list_length); 
    }
    if(template_btl->template_frag_max.fl_num_allocated != 
      template_btl->template_frag_max.super.opal_list_length) { 
        opal_output(0, "btl template_frag_max: %d allocated %d returned \n", 
                    template_btl->template_frag_max.fl_num_allocated, 
                    template_btl->template_frag_max.super.opal_list_length); 
    }
    if(template_btl->template_frag_user.fl_num_allocated != 
       template_btl->template_frag_user.super.opal_list_length){ 
        opal_output(0, "btl template_frag_user: %d allocated %d returned \n", 
                    template_btl->template_frag_user.fl_num_allocated, 
                    template_btl->template_frag_user.super.opal_list_length); 
    }

    OBJ_DESTRUCT(&template_btl->template_lock);
    OBJ_DESTRUCT(&template_btl->template_frag_eager);
    OBJ_DESTRUCT(&template_btl->template_frag_max);
    OBJ_DESTRUCT(&template_btl->template_frag_user);
    free(template_btl);
    return OMPI_SUCCESS;
}

