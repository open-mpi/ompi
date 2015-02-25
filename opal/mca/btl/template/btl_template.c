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
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "btl_template.h"
#include "btl_template_frag.h" 
#include "btl_template_proc.h"
#include "btl_template_endpoint.h"


mca_btl_template_module_t mca_btl_template_module = {
    .super = {
        .btl_component = &mca_btl_template_component.super,
        .btl_add_procs = mca_btl_template_add_procs,
        .btl_del_procs = mca_btl_template_del_procs,
        .btl_register = mca_btl_template_register,
        .btl_finalize = mca_btl_template_finalize,
        .btl_alloc = mca_btl_template_alloc,
        .btl_free = mca_btl_template_free,
        .btl_prepare_src = mca_btl_template_prepare_src,
        .btl_send = mca_btl_template_send,
        .btl_put = mca_btl_template_put,
        .btl_get = mca_btl_template_get,
        .btl_register_mem = mca_btl_template_register_mem,
        .btl_deregister_mem = mca_btl_template_deregister_mem,
        .btl_ft_event = mca_btl_template_ft_event
    }
};

/**
 *
 */

int mca_btl_template_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct opal_proc_t **opal_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    opal_bitmap_t* reachable)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {

        struct opal_proc_t* opal_proc = opal_procs[i];
        mca_btl_template_proc_t* template_proc;
        mca_btl_base_endpoint_t* template_endpoint;

#if 0
        /* If the BTL doesn't support heterogeneous operations, be
           sure to say we can't reach that proc */
        if (opal_proc_local()->proc_arch != opal_proc->proc_arch) {
            continue;
        }
#endif

        if(NULL == (template_proc = mca_btl_template_proc_create(opal_proc))) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this BTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&template_proc->proc_lock);

        /* The btl_proc datastructure is shared by all TEMPLATE BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        template_endpoint = OBJ_NEW(mca_btl_template_endpoint_t);
        if(NULL == template_endpoint) {
            OPAL_THREAD_UNLOCK(&template_proc->proc_lock);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        template_endpoint->endpoint_btl = template_btl;
        rc = mca_btl_template_proc_insert(template_proc, template_endpoint);
        if(rc != OPAL_SUCCESS) {
            OBJ_RELEASE(template_endpoint);
            OPAL_THREAD_UNLOCK(&template_proc->proc_lock);
            continue;
        }

        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&template_proc->proc_lock);
        peers[i] = template_endpoint;
    }

    return OPAL_SUCCESS;
}

int mca_btl_template_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct opal_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OPAL_SUCCESS;
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
    return OPAL_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_template_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    mca_btl_template_frag_t* frag = NULL;
    
    if(size <= btl->btl_eager_limit){ 
        MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(template_btl, frag); 
    } else { 
        MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(template_btl, frag); 
    }
    if( OPAL_UNLIKELY(NULL != frag) ) {
        return NULL;
    }
    
    frag->segment.seg_len = size;
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
        return OPAL_ERR_BAD_PARAM;
    }
    return OPAL_SUCCESS; 
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
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
)
{
    mca_btl_template_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;
                                                                                                    

    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
    */
    if (max_data+reserve <= btl->btl_eager_limit) {

        MCA_BTL_TEMPLATE_FRAG_ALLOC_EAGER(btl, frag);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
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
                                                                                                    
        MCA_BTL_TEMPLATE_FRAG_ALLOC_MAX(btl, frag);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }
        if(max_data + reserve > frag->size){
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        *size  = max_data;
                                                                                                    
        if( rc < 0 ) {
            MCA_BTL_TEMPLATE_FRAG_RETURN_MAX(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    frag->base.des_segments = &frag->segment;
    frag->base.des_segment_count = 1;
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
    return OPAL_ERR_NOT_IMPLEMENTED;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_template_put (struct mca_btl_base_module_t *btl,
    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
    uint64_t remote_address, struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    /* TODO */
    return OPAL_ERR_NOT_IMPLEMENTED; 
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_template_get (struct mca_btl_base_module_t *btl,
    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
    uint64_t remote_address, struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    /* TODO */
    return OPAL_ERR_NOT_IMPLEMENTED; 
}

/**
 * @brief Register a memory region for put/get/atomic operations.
 *
 * @param btl (IN)         BTL module
 * @param endpoint(IN)     BTL addressing information (or NULL for all endpoints)
 * @param base (IN)        Pointer to start of region
 * @param size (IN)        Size of region
 * @param flags (IN)       Flags indicating what operation will be performed. Valid
 *                         values are MCA_BTL_DES_FLAGS_PUT, MCA_BTL_DES_FLAGS_GET,
 *                         and MCA_BTL_DES_FLAGS_ATOMIC
 *
 * @returns a memory registration handle valid for both local and remote operations
 * @returns NULL if the region could not be registered
 *
 * This function registers the specified region with the hardware for use with
 * the btl_put, btl_get, btl_atomic_cas, btl_atomic_op, and btl_atomic_fop
 * functions. Care should be taken to not hold an excessive number of registrations
 * as they may use limited system/NIC resources.
 */
struct mca_btl_base_registration_handle_t *mca_btl_template_register_mem (
    struct mca_btl_base_module_t* btl, struct mca_btl_base_endpoint_t *endpoint, void *base,
    size_t size, uint32_t flags)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    /* TODO */
    return NULL;
}

/**
 * @brief Deregister a memory region
 *
 * @param btl (IN)         BTL module region was registered with
 * @param handle (IN)      BTL registration handle to deregister
 *
 * This function deregisters the memory region associated with the specified handle. Care
 * should be taken to not perform any RDMA or atomic operation on this memory region
 * after it is deregistered. It is erroneous to specify a memory handle associated with
 * a remote node.
 */
int mca_btl_template_deregister_mem (struct mca_btl_base_module_t* btl,
				     struct mca_btl_base_registration_handle_t *handle)
{
    /* mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; */
    /* TODO */
    return OPAL_ERR_NOT_IMPLEMENTED;
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_template_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_template_module_t* template_btl = (mca_btl_template_module_t*) btl; 
    OBJ_DESTRUCT(&template_btl->template_lock);
    OBJ_DESTRUCT(&template_btl->template_frag_eager);
    OBJ_DESTRUCT(&template_btl->template_frag_max);
    OBJ_DESTRUCT(&template_btl->template_frag_user);
    free(template_btl);
    return OPAL_SUCCESS;
}

int mca_btl_template_ft_event(int state) {
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
