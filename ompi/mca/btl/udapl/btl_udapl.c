/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include "btl_udapl.h"
#include "btl_udapl_frag.h" 
#include "btl_udapl_proc.h"
#include "btl_udapl_endpoint.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/udapl/mpool_udapl.h"
#include "ompi/proc/proc.h"


mca_btl_udapl_module_t mca_btl_udapl_module = {
    {
        &mca_btl_udapl_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND,
        mca_btl_udapl_add_procs,
        mca_btl_udapl_del_procs,
        mca_btl_udapl_register, 
        mca_btl_udapl_finalize,
        mca_btl_udapl_alloc, 
        mca_btl_udapl_free, 
        mca_btl_udapl_prepare_src,
        NULL, /* prepare_dst */
        mca_btl_udapl_send,
        NULL, /* put */
        NULL, /* get */ 
        mca_btl_base_dump
    }
};


/**
 * Initialize module module resources.
 */

int
mca_btl_udapl_init(DAT_NAME_PTR ia_name, mca_btl_udapl_module_t* btl)
{
    mca_mpool_base_resources_t res;
    DAT_CONN_QUAL port;
    DAT_IA_ATTR attr;
    DAT_RETURN rc;

    /* open the uDAPL interface */
    btl->udapl_evd_async = DAT_HANDLE_NULL;
    rc = dat_ia_open(ia_name, mca_btl_udapl_component.udapl_evd_qlen,
            &btl->udapl_evd_async, &btl->udapl_ia);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ia_open");
        return OMPI_ERROR;
    }

    /* create a protection zone */
    rc = dat_pz_create(btl->udapl_ia, &btl->udapl_pz);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_pz_create");
        goto failure;
    }

    /* query to get address information */
    /* TODO - we only get the address, but there's other useful stuff here */
    rc = dat_ia_query(btl->udapl_ia, &btl->udapl_evd_async,
            DAT_IA_FIELD_IA_ADDRESS_PTR, &attr, DAT_IA_FIELD_NONE, NULL);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ia_query");
        goto failure;
    }

    memcpy(&btl->udapl_addr.addr, attr.ia_address_ptr, sizeof(DAT_SOCK_ADDR));

    /* set up evd's */
    rc = dat_evd_create(btl->udapl_ia,
            mca_btl_udapl_component.udapl_evd_qlen, DAT_HANDLE_NULL,
            DAT_EVD_DTO_FLAG | DAT_EVD_RMR_BIND_FLAG, &btl->udapl_evd_dto);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_evd_create (dto)");
        goto failure;
    }

    rc = dat_evd_create(btl->udapl_ia,
            mca_btl_udapl_component.udapl_evd_qlen, DAT_HANDLE_NULL,
            DAT_EVD_CR_FLAG | DAT_EVD_CONNECTION_FLAG, &btl->udapl_evd_conn);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_evd_create (conn)");
        goto failure;
    }

    /* create our public service point */
    /* We have to specify a port, so we go through a range until we
       find a port that works */
    for(port = mca_btl_udapl_component.udapl_port_low;
            port <= mca_btl_udapl_component.udapl_port_high; port++) {

        rc = dat_psp_create(btl->udapl_ia, port, btl->udapl_evd_conn,
                DAT_PSP_CONSUMER_FLAG, &btl->udapl_psp);
        if(DAT_SUCCESS == rc) {
            break;
        } else if(DAT_CONN_QUAL_IN_USE != rc) {
            mca_btl_udapl_error(rc, "dat_psp_create");
            goto failure;
        }
    }

    if(port == mca_btl_udapl_component.udapl_port_high) {
        goto failure;
    }

    /* Save the port with the address information */
    btl->udapl_addr.port = port;

    /* initialize the memory pool */
    res.udapl_ia = btl->udapl_ia;
    res.udapl_pz = btl->udapl_pz;

    btl->super.btl_mpool = mca_mpool_base_module_create(
            mca_btl_udapl_component.udapl_mpool_name, &btl->super, &res);

    /* initialize objects */
    OBJ_CONSTRUCT(&btl->udapl_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_frag_recv, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->udapl_pending, opal_list_t);
    OBJ_CONSTRUCT(&btl->udapl_repost, opal_list_t);
    OBJ_CONSTRUCT(&btl->udapl_mru_reg, opal_list_t);
    OBJ_CONSTRUCT(&btl->udapl_lock, opal_mutex_t);
    
    /* initialize free lists */
    ompi_free_list_init(&btl->udapl_frag_eager,
          sizeof(mca_btl_udapl_frag_eager_t) +
                 mca_btl_udapl_module.super.btl_eager_limit,
          OBJ_CLASS(mca_btl_udapl_frag_eager_t),
          mca_btl_udapl_component.udapl_free_list_num,
          mca_btl_udapl_component.udapl_free_list_max,
          mca_btl_udapl_component.udapl_free_list_inc,
          btl->super.btl_mpool);

    ompi_free_list_init(&btl->udapl_frag_max,
          sizeof(mca_btl_udapl_frag_max_t) +
                 mca_btl_udapl_module.super.btl_max_send_size,
          OBJ_CLASS(mca_btl_udapl_frag_max_t),
          mca_btl_udapl_component.udapl_free_list_num,
          mca_btl_udapl_component.udapl_free_list_max,
          mca_btl_udapl_component.udapl_free_list_inc,
          btl->super.btl_mpool);

    ompi_free_list_init(&btl->udapl_frag_user,
          sizeof(mca_btl_udapl_frag_user_t),
          OBJ_CLASS(mca_btl_udapl_frag_user_t),
          mca_btl_udapl_component.udapl_free_list_num,
          mca_btl_udapl_component.udapl_free_list_max,
          mca_btl_udapl_component.udapl_free_list_inc,
          NULL);

    /* TODO - post receives */
    /* TODO - can I always use SRQ, or just on new enough uDAPLs? */
    return OMPI_SUCCESS;

failure:
    dat_ia_close(btl->udapl_ia, DAT_CLOSE_ABRUPT_FLAG);
    return OMPI_ERROR;
}


/*
 * Cleanup/release module resources.
 */

int mca_btl_udapl_finalize(struct mca_btl_base_module_t* base_btl)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) base_btl; 

    OPAL_OUTPUT((0, "udapl_finalize\n"));

    /* release uDAPL resources */
    dat_evd_free(udapl_btl->udapl_evd_dto);
    dat_evd_free(udapl_btl->udapl_evd_conn);
    dat_pz_free(udapl_btl->udapl_pz);
    dat_ia_close(udapl_btl->udapl_ia, DAT_CLOSE_GRACEFUL_FLAG);

    /* destroy objects */
    OBJ_DESTRUCT(&udapl_btl->udapl_lock);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_eager);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_max);
    OBJ_DESTRUCT(&udapl_btl->udapl_frag_user);

    free(udapl_btl);
    return OMPI_SUCCESS;
}


/**
 *
 */

int mca_btl_udapl_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*)btl;
    int i, rc;

    OPAL_OUTPUT((0, "udapl_add_procs\n"));

    for(i = 0; i < (int) nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_udapl_proc_t* udapl_proc;
        mca_btl_base_endpoint_t* udapl_endpoint;

        if(ompi_proc == ompi_proc_local()) 
            continue;

        if(NULL == (udapl_proc = mca_btl_udapl_proc_create(ompi_proc))) {
            continue;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this BTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&udapl_proc->proc_lock);

        /* The btl_proc datastructure is shared by all uDAPL BTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        udapl_endpoint = OBJ_NEW(mca_btl_udapl_endpoint_t);
        if(NULL == udapl_endpoint) {
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        udapl_endpoint->endpoint_btl = udapl_btl;
        rc = mca_btl_udapl_proc_insert(udapl_proc, udapl_endpoint);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(udapl_endpoint);
            OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&udapl_proc->proc_lock);
        peers[i] = udapl_endpoint;
    }

    return OMPI_SUCCESS;
}

int mca_btl_udapl_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    OPAL_OUTPUT((0, "udapl_del_procs\n"));
    /* TODO */
    return OMPI_SUCCESS;
}


/**
 * Register callback function to support send/recv semantics
 */

int mca_btl_udapl_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    udapl_btl->udapl_reg[tag].cbfunc = cbfunc; 
    udapl_btl->udapl_reg[tag].cbdata = cbdata; 

    OPAL_OUTPUT((0, "udapl_register\n"));
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_udapl_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*) btl; 
    mca_btl_udapl_frag_t* frag;
    int rc;

    OPAL_OUTPUT((0, "udapl_alloc\n"));
    
    if(size <= btl->btl_eager_limit) { 
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(udapl_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_eager_limit ? 
            size : btl->btl_eager_limit; 
    } else { 
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(udapl_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_max_send_size ? 
            size : btl->btl_max_send_size; 
    }

    /* Set up the LMR triplet from the frag segment */
    /* Note that this triplet defines a sub-region of a registered LMR */
    frag->triplet.lmr_context = frag->segment.seg_key.key32[0];
    frag->triplet.virtual_address = (DAT_VADDR)frag->segment.seg_addr.pval;
    frag->triplet.segment_length = frag->segment.seg_len;
    
    frag->btl = udapl_btl;
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

int mca_btl_udapl_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    OPAL_OUTPUT((0, "udapl_free\n"));

    if(frag->size == 0) {
        btl->btl_mpool->mpool_release(btl->btl_mpool, frag->registration);
        MCA_BTL_UDAPL_FRAG_RETURN_USER(btl, frag); 
    } else if(frag->size == mca_btl_udapl_component.udapl_eager_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag); 
    } else if(frag->size == mca_btl_udapl_component.udapl_max_frag_size) {
        MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag); 
    }  else {
        OPAL_OUTPUT((0, "[%s:%d] mca_btl_udapl_free: invalid descriptor\n", __FILE__,__LINE__));
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
mca_btl_base_descriptor_t* mca_btl_udapl_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_udapl_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int32_t free_after;
    int rc;

    OPAL_OUTPUT((0, "udapl_prepare_src\n"));

    /*
     * If the data has already been pinned and is contigous than we can
     * use it in place.
    */
    if (NULL != registration && 0 == ompi_convertor_need_buffers(convertor)) {
        size_t reg_len;
        MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
        if(NULL == frag){
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor, &iov,
                &iov_count, &max_data, &free_after);

        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;

        reg_len = (unsigned char*)registration->bound -
                (unsigned char*)iov.iov_base + 1;
        
        /* bump reference count as so that the registration
         * doesn't go away when the operation completes
         */
        btl->btl_mpool->mpool_retain(btl->btl_mpool, registration);
        frag->registration = registration;

    /*
     * if the data is not already pinned - but the leave pinned option is set,
     * then go ahead and pin contigous data. however, if a reserve is required 
     * then we must allocated a fragment w/ buffer space
    */
    } else if (max_data > btl->btl_max_send_size && 
               ompi_convertor_need_buffers(convertor) == 0 &&
               reserve == 0) {

        mca_mpool_base_module_t* mpool = btl->btl_mpool;
        MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
        if(NULL == frag){
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        ompi_convertor_pack(convertor, &iov,
                &iov_count, &max_data, &free_after);

        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;

        rc = mpool->mpool_register(
                                   mpool,
                                   iov.iov_base,
                                   max_data,
                                   0,
                                   &registration);

        if(rc != OMPI_SUCCESS) {
            MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }

        frag->registration = registration;
    } 

    /*
     * if we aren't pinning the data and the requested size is less
     * than the eager limit pack into a fragment from the eager pool
    */
    else if (max_data+reserve <= btl->btl_eager_limit) {
                                                                                                    
        MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
                                                                                                    
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after);
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    /* 
     * otherwise pack as much data as we can into a fragment
     * that is the max send size.
     */
    else {
                                                                                                    
        MCA_BTL_UDAPL_FRAG_ALLOC_MAX(btl, frag, rc);
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
            MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag);
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

mca_btl_base_descriptor_t* mca_btl_udapl_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_udapl_frag_t* frag;
    mca_mpool_base_module_t* mpool = btl->btl_mpool;
    long lb;
    int rc;

    OPAL_OUTPUT((0, "udapl_prepare_dst\n"));

    MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segment.seg_len = *size;
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted;

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
    if(NULL != registration) {
        /* bump reference count as so that the registration
         * doesn't go away when the operation completes
         */
        
        mpool->mpool_retain(mpool, 
                           (mca_mpool_base_registration_t*) registration); 
                
        frag->registration = registration;

    }  else {

        rc = mpool->mpool_register(
                                   mpool,
                                   frag->segment.seg_addr.pval,
                                   frag->segment.seg_len,
                                   0,
                                   &registration);
        if(rc != OMPI_SUCCESS) {
            MCA_BTL_UDAPL_FRAG_RETURN_USER(btl,frag);
            return NULL;
        }
        
        frag->registration = registration;
    }
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

int mca_btl_udapl_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* des, 
    mca_btl_base_tag_t tag)
   
{
    mca_btl_udapl_module_t* udapl_btl = (mca_btl_udapl_module_t*)btl;
    mca_btl_udapl_frag_t* frag = (mca_btl_udapl_frag_t*)des;

    OPAL_OUTPUT((0, "udapl_send\n"));
    
    frag->btl = udapl_btl;
    frag->endpoint = endpoint;
    frag->hdr->tag = tag;
    frag->type = MCA_BTL_UDAPL_SEND;

    return mca_btl_udapl_endpoint_send(endpoint, frag);
}



/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_udapl_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    OPAL_OUTPUT((0, "udapl_put\n"));
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

int mca_btl_udapl_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* des)
{
    OPAL_OUTPUT((0, "udapl_get\n"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

