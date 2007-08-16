/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "btl_gm.h"
#include "btl_gm_frag.h" 
#include "btl_gm_proc.h"
#include "btl_gm_endpoint.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/proc/proc.h"


/**
 * Non-locking versions of public interfaces.
 */

static int mca_btl_gm_send_nl( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des, 
    mca_btl_base_tag_t tag);

static int mca_btl_gm_get_nl( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des);

static int mca_btl_gm_put_nl( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des);


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
#if OMPI_ENABLE_MPI_THREADS || OMPI_ENABLE_PROGRESS_THREADS
        mca_btl_gm_send,
        mca_btl_gm_put,
        mca_btl_gm_get,
#else
        mca_btl_gm_send_nl,
        mca_btl_gm_put_nl,
        mca_btl_gm_get_nl,
#endif
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_gm_register_error_cb
        
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
            continue;
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
            OPAL_THREAD_UNLOCK(&gm_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        gm_endpoint->endpoint_btl = gm_btl;
        rc = mca_btl_gm_proc_insert(gm_proc, gm_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(gm_endpoint);
            OPAL_THREAD_UNLOCK(&gm_proc->proc_lock);
            continue;
        }
        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&gm_proc->proc_lock);
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


/* 
 *Register callback function for error handling..
 */ 
int mca_btl_gm_register_error_cb(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_module_error_cb_fn_t cbfunc)
{
    
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
    gm_btl->error_cb = cbfunc; /* stash for later */
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
        if(NULL == frag) { 
            return NULL;
        }
        frag->type=MCA_BTL_GM_EAGER;
        frag->segment.seg_len = size;
        
    } else if(size <=  btl->btl_max_send_size) { 
        MCA_BTL_GM_FRAG_ALLOC_MAX(gm_btl, frag, rc); 
        if(NULL == frag) { 
            return NULL;
        }
        frag->type=MCA_BTL_GM_SEND;
        frag->segment.seg_len = size; 
    } else { 
        return NULL;
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

int mca_btl_gm_free( struct mca_btl_base_module_t* btl, 
		     mca_btl_base_descriptor_t* des )
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)des;

    if( NULL != frag->registration ) {
        btl->btl_mpool->mpool_deregister(btl->btl_mpool, (mca_mpool_base_registration_t*) frag->registration);
        frag->registration = NULL;
    }

    MCA_BTL_GM_FRAG_RETURN(btl, frag); 
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
    mca_btl_gm_frag_t *frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

#if (OMPI_MCA_BTL_GM_HAVE_RDMA_GET || OMPI_MCA_BTL_GM_HAVE_RDMA_PUT)
    if(ompi_convertor_need_buffers(convertor) == false && 0 == reserve) {
        if(registration != NULL || max_data > btl->btl_max_send_size) {
            MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc);
            if(NULL == frag) {
                return NULL;
            }

            /*
             * just assign it something..
             * we will assign the real value in put/get
             */
            frag->type = MCA_BTL_GM_PUT;
            iov.iov_len = max_data;
            iov.iov_base = NULL;


            ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);

            *size = max_data;

            if(NULL == registration) {
                rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                        iov.iov_base, max_data, 0, &registration);
                if(OMPI_SUCCESS != rc || NULL == registration) {
                    MCA_BTL_GM_FRAG_RETURN(btl, frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                frag->registration = registration;
            }

            frag->segment.seg_len = max_data;
            frag->segment.seg_addr.pval = iov.iov_base;

            frag->base.des_src = &frag->segment;
            frag->base.des_src_cnt = 1;
            frag->base.des_dst = NULL;
            frag->base.des_dst_cnt = 0;
            frag->base.des_flags = 0;

            return &frag->base;
        }
    }
#endif

    if (max_data + reserve <= btl->btl_eager_limit) {
        /* the data is small enough to fit in the eager frag and
         * memory is not prepinned */
        MCA_BTL_GM_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(frag != NULL) {
            frag->type = MCA_BTL_GM_EAGER;
        }
    }

    if(NULL == frag) {
        /* the data doesn't fit into eager frag or eger frag is
         * not available */
        MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        frag->type = MCA_BTL_GM_SEND;
        if(max_data + reserve > btl->btl_max_send_size) {
            max_data = btl->btl_max_send_size - reserve;
        }
    }

    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
    if(rc < 0) {
        MCA_BTL_GM_FRAG_RETURN(btl, frag);
        return NULL;
    }
    *size  = max_data;
    frag->segment.seg_len = max_data + reserve;
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
#if (OMPI_MCA_BTL_GM_HAVE_RDMA_GET || OMPI_MCA_BTL_GM_HAVE_RDMA_PUT)
    mca_btl_gm_frag_t* frag;
    mca_mpool_base_module_t* mpool = btl->btl_mpool;
    ptrdiff_t lb;
    int rc;

    MCA_BTL_GM_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }
    /*
     * just assign it something.. 
     *  we will assign the real value in put/get 
     */
    frag->type = MCA_BTL_GM_PUT;
    
    ompi_ddt_type_lb(convertor->pDesc, &lb);
    /* 
     *  we don't know that this is for a PUT,
     *  but it doesn't matter.. they belong 
     *  on the same list eventually anyway
     */      
    frag->type = MCA_BTL_GM_PUT; 
    
    frag->segment.seg_len = *size;
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted;

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
    if(NULL == registration) {
        rc = mpool->mpool_register( mpool,
				    frag->segment.seg_addr.pval,
				    frag->segment.seg_len,
				    0,
				    &registration );
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_GM_FRAG_RETURN(btl,frag);
            return NULL;
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

/**
 * Callback on send completion and/or error.
 * Called with mca_btl_gm_component.gm_lock held.
 */

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
            mca_btl_gm_send_nl(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);
            break;
        case GM_SEND_DROPPED:
            /* release the send token */
            OPAL_THREAD_ADD32(&btl->gm_num_send_tokens, 1);

            /* retry the dropped fragment */
            mca_btl_gm_send_nl(&btl->super, frag->endpoint, &frag->base, frag->hdr->tag);
            break;
        case GM_SUCCESS:
            /* call the completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);

            /* return the send token and deque pending fragments */
            MCA_BTL_GM_RETURN_TOKEN(btl);
            break;
 
        default:
            /* error condition can't deal with */
            opal_output(0, "[%s:%d] send completed with unhandled gm error %d\n", __FILE__,__LINE__,status);

            /* release the send token */
            OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );

            /* call the completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_ERROR);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
            break;
    }
}


/**
 * Initiate an asynchronous send. Do NOT acquire gm lock, must already be held,
 * or in an unthreaded environment.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */


static int mca_btl_gm_send_nl( 
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
    MCA_BTL_GM_ACQUIRE_TOKEN_NL(gm_btl, frag);

    /* post the send descriptor */
    if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&  
       frag->size == mca_btl_gm_component.gm_eager_frag_size) {
        gm_send_with_callback( gm_btl->port,
			       frag->hdr,
			       mca_btl_gm_component.gm_eager_frag_size,
			       frag->segment.seg_len + sizeof(mca_btl_base_header_t),
			       GM_HIGH_PRIORITY,
			       endpoint->endpoint_addr.node_id,
			       endpoint->endpoint_addr.port_id,
			       mca_btl_gm_send_callback,
			       frag );
    } else {
        gm_send_with_callback( gm_btl->port,
			       frag->hdr,
			       mca_btl_gm_component.gm_max_frag_size,
			       frag->segment.seg_len + sizeof(mca_btl_base_header_t),
			       GM_LOW_PRIORITY,
			       endpoint->endpoint_addr.node_id,
			       endpoint->endpoint_addr.port_id,
			       mca_btl_gm_send_callback,
			       frag );
    }
    if(opal_list_get_size(&gm_btl->gm_repost)) {
        mca_btl_gm_frag_t* frag;
        while(NULL != (frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&gm_btl->gm_repost))) {
            gm_provide_receive_buffer(gm_btl->port, frag->hdr, frag->size, frag->priority);
        }
    }

    return OMPI_SUCCESS;
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
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    MCA_BTL_GM_ACQUIRE_TOKEN(gm_btl, frag);
    
    /* post the send descriptor */
    if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&  
       frag->size == mca_btl_gm_component.gm_eager_frag_size) {
        gm_send_with_callback( gm_btl->port,
			       frag->hdr,
			       mca_btl_gm_component.gm_eager_frag_size,
			       frag->segment.seg_len + sizeof(mca_btl_base_header_t),
			       GM_HIGH_PRIORITY,
			       endpoint->endpoint_addr.node_id,
			       endpoint->endpoint_addr.port_id,
			       mca_btl_gm_send_callback,
			       frag );
    } else {
        gm_send_with_callback( gm_btl->port,
			       frag->hdr,
			       mca_btl_gm_component.gm_max_frag_size,
			       frag->segment.seg_len + sizeof(mca_btl_base_header_t),
			       GM_LOW_PRIORITY,
			       endpoint->endpoint_addr.node_id,
			       endpoint->endpoint_addr.port_id,
			       mca_btl_gm_send_callback,
			       frag );
    }

    if(opal_list_get_size(&gm_btl->gm_repost)) {
        mca_btl_gm_frag_t* frag;
        while(NULL != (frag = (mca_btl_gm_frag_t*)opal_list_remove_first(&gm_btl->gm_repost))) {
            gm_provide_receive_buffer(gm_btl->port, frag->hdr, frag->size, frag->priority);
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
    return OMPI_SUCCESS;
}



/**
 * Callback on put completion and/or error.
 * Called with mca_btl_gm_component.gm_lock held.
 */

static void mca_btl_gm_put_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)context;
    mca_btl_gm_module_t* btl = frag->btl;

    /* call the completion callback */
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
            mca_btl_gm_put_nl(&btl->super, frag->endpoint, &frag->base);
            break;
        case GM_SEND_DROPPED:
            /* release the send token */
            OPAL_THREAD_ADD32(&btl->gm_num_send_tokens, 1);

            /* retry the dropped fragment */
            mca_btl_gm_put_nl(&btl->super, frag->endpoint, &frag->base);
            break;
        case GM_SUCCESS:
            /* call completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);

            /* return the send token and deque pending fragments */
            MCA_BTL_GM_RETURN_TOKEN(btl);
            break;
        default:
            /* error condition can't deal with */
            opal_output(0, "[%s:%d] gm_put operation failed with status %d\n", __FILE__, __LINE__, status);

            /* release the send token */
            OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );

            /* call the completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_ERROR);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
            break;
    }
}


/**
 * Initiate an asynchronous put. Do not acquire lock.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

static int mca_btl_gm_put_nl( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
#if OMPI_MCA_BTL_GM_HAVE_RDMA_PUT
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl;
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*) des; 

    frag->btl = gm_btl;
    frag->endpoint = endpoint;
    frag->type = MCA_BTL_GM_PUT;

    /* queue the descriptor if there are no send tokens */
    MCA_BTL_GM_ACQUIRE_TOKEN_NL(gm_btl, frag);

    /* post the put descriptor */
    gm_put(gm_btl->port,
        des->des_src->seg_addr.pval,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_put_callback,
        frag);
    return OMPI_SUCCESS; 
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
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
    frag->type = MCA_BTL_GM_PUT;

    /* queue the descriptor if there are no send tokens */
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    MCA_BTL_GM_ACQUIRE_TOKEN(gm_btl, frag);

    /* post the put descriptor */
    gm_put(gm_btl->port,
        des->des_src->seg_addr.pval,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_put_callback,
        frag);
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
    return OMPI_SUCCESS; 
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}



/**
 * Callback on get completion and/or error. 
 * Called with mca_btl_gm_component.gm_lock held.
 */

static void mca_btl_gm_get_callback( struct gm_port* port, void* context, gm_status_t status )
{
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)context;
    mca_btl_gm_module_t* btl = frag->btl;

    /* call the completion callback */
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
            mca_btl_gm_get_nl(&btl->super, frag->endpoint, &frag->base);
            break;
        case GM_SEND_DROPPED:
            /* release the send token */
            OPAL_THREAD_ADD32(&btl->gm_num_send_tokens, 1);

            /* retry the dropped fragment */
            mca_btl_gm_get_nl(&btl->super, frag->endpoint, &frag->base);
            break;
        case GM_SUCCESS:
            /* call completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_SUCCESS);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);

            /* return the send token and deque pending fragments */
            MCA_BTL_GM_RETURN_TOKEN(btl);
            break;
        default:
            /* error condition can't deal with */
            opal_output(0, "[%s:%d] gm_get operation failed with status %d\n", __FILE__, __LINE__, status);

            /* release the send token */
            OPAL_THREAD_ADD32( &btl->gm_num_send_tokens, 1 );

            /* call the completion callback */
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            frag->base.des_cbfunc(&btl->super, frag->endpoint, &frag->base, OMPI_ERROR);
            OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
            break;
    }
}


/**
 * Initiate an asynchronous get. No locking.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

static int mca_btl_gm_get_nl( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
#if OMPI_MCA_BTL_GM_HAVE_RDMA_GET
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl;
    mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*) des; 

    frag->btl = gm_btl;
    frag->endpoint = endpoint;
    frag->type = MCA_BTL_GM_GET;

    /* queue the descriptor if there are no send tokens */
    MCA_BTL_GM_ACQUIRE_TOKEN_NL(gm_btl, frag);

    /* post get put descriptor */
    gm_get(gm_btl->port,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_addr.pval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_get_callback,
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
    frag->type = MCA_BTL_GM_GET;

    /* queue the descriptor if there are no send tokens */
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    MCA_BTL_GM_ACQUIRE_TOKEN(gm_btl, frag);

    /* post get put descriptor */
    gm_get(gm_btl->port,
        des->des_dst->seg_addr.lval,
        des->des_src->seg_addr.pval,
        des->des_src->seg_len,
        GM_LOW_PRIORITY,
        endpoint->endpoint_addr.node_id,
        endpoint->endpoint_addr.port_id,
        mca_btl_gm_get_callback,
        frag);
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
    return OMPI_SUCCESS; 
#else
    return OMPI_ERR_NOT_IMPLEMENTED;
#endif
}


/*
 * Cleanup/release module resources.
 */

#if OMPI_ENABLE_PROGRESS_THREADS
static void mca_btl_gm_alarm(void* arg) {}
#endif

int mca_btl_gm_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_gm_module_t* gm_btl = (mca_btl_gm_module_t*) btl; 
#if OMPI_ENABLE_PROGRESS_THREADS
    gm_alarm_t alarm;
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    gm_btl->gm_progress = false;
    gm_initialize_alarm(&alarm);
    gm_set_alarm(gm_btl->port, &alarm, 10, mca_btl_gm_alarm, NULL);
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
    opal_thread_join(&gm_btl->gm_thread, NULL);
#endif

    OBJ_DESTRUCT(&gm_btl->gm_frag_eager);
    OBJ_DESTRUCT(&gm_btl->gm_frag_max);
    OBJ_DESTRUCT(&gm_btl->gm_frag_user);
    gm_close(gm_btl->port);
    free(gm_btl);
    return OMPI_SUCCESS;
}

