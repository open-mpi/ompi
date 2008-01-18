/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/communicator/communicator.h"
#include "btl_elan.h"
#include "btl_elan_frag.h" 
#include "btl_elan_proc.h"
#include "btl_elan_endpoint.h"

#include "ompi/datatype/convertor.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/runtime/ompi_module_exchange.h"
#include "orte/class/orte_proc_table.h" 
#include "opal/class/opal_hash_table.h"   

#include "stdio.h"
#include "elan/elan.h"
#include "opal/util/os_path.h"
#include "opal/util/opal_environ.h"
#include "orte/util/proc_info.h"

/**
 *
 */
extern char** environ;

/**
 * PML->BTL notification of change in the process list.
 * 
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */
static int mca_btl_elan_add_procs( struct mca_btl_base_module_t* btl, 
                                   size_t nprocs, 
                                   struct ompi_proc_t **ompi_procs, 
                                   struct mca_btl_base_endpoint_t** peers, 
                                   ompi_bitmap_t* reachable )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*)btl;
    mca_btl_elan_frag_t* frag;
    int i, rc;
    FILE* file;
    char* filename;
    ELAN_BASE    *base;
    ELAN_STATE   *state;
    ELAN_QUEUE   *q = NULL;
    ELAN_TPORT   *p = NULL;

    /* Create the mapid file in the temporary storage */
    filename = opal_os_path( false, orte_process_info.proc_session_dir, "ELAN_ID", NULL );
    file = fopen( filename, "w" );
    for( i = 0; i < (int)nprocs; i++ ) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        fprintf( file, "%s %d\n", ompi_proc->proc_hostname, i );
    }
    fclose( file );

    /* Set the environment before firing up the Elan library */
    opal_setenv( "LIBELAN_MACHINES_FILE", filename, true, &environ );
    opal_setenv( "MPIRUN_ELANIDMAP_FILE", mca_btl_elan_component.elanidmap_file,
                 false, &environ ); 

    base = elan_baseInit(0);  
    if( NULL == base )  
        return OMPI_ERR_OUT_OF_RESOURCE;
    state = base->state;   
    if( NULL == state ) {  
        mca_btl_base_error_no_nics( "ELAN", "Quadrics" ); 
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Create the global queue (it's a synchronization point) */
    if( (q = elan_gallocQueue(base, base->allGroup)) == NULL ) {  
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if( !(p = elan_tportInit(base->state,
                             q,
                             base->tport_nslots,
                             base->tport_smallmsg,
                             mca_btl_elan_module.super.btl_eager_limit, /*base->tport_bigmsg,*/
                             base->tport_stripemsg, 
                             ELAN_POLL_EVENT,  
                             base->retryCount, 
                             &base->shm_key,  
                             base->shm_fifodepth, 
                             base->shm_fragsize,  
                             ELAN_TPORT_SHM_DISABLE | ELAN_TPORT_USERCOPY_DISABLE))) { 
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    elan_btl->base  = base; 
    elan_btl->state = state; 
    elan_btl->queue = q;  
    elan_btl->tport = p;  
    elan_btl->elan_vp = state->vp;  
    elan_btl->elan_nvp = state->nvp; 

    for(i = 0; i < (int) nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_elan_proc_t* elan_proc;
        mca_btl_base_endpoint_t* elan_endpoint;

        /* Don't use Elan for local communications */
        if( ompi_proc_local_proc == ompi_proc )
            continue;

        if(NULL == (elan_proc = mca_btl_elan_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        elan_endpoint = OBJ_NEW(mca_btl_elan_endpoint_t);
        if(NULL == elan_endpoint) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        elan_endpoint->endpoint_btl = elan_btl;

        OPAL_THREAD_LOCK(&elan_proc->proc_lock);
        rc = mca_btl_elan_proc_insert(elan_proc, elan_endpoint);
        OPAL_THREAD_UNLOCK(&elan_proc->proc_lock);

        if( OMPI_SUCCESS != rc ) {
            OBJ_RELEASE(elan_endpoint);
            OBJ_RELEASE(elan_proc);
            continue;
        }
        ompi_bitmap_set_bit(reachable, i);
        peers[i] = elan_endpoint;
    }

    /* Post some receives */
    for( i = 0; i < mca_btl_elan_component.elan_max_posted_recv; i++ ) {
        /* Post the receives if there is no unexpected handler */
        MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc );
        if( NULL == frag ) {
            return OMPI_ERROR; 
        }
        frag->base.des_dst     = &(frag->segment);
        frag->base.des_dst_cnt = 1;
        frag->base.des_src     = NULL;
        frag->base.des_src_cnt = 0;
        frag->type             = MCA_BTL_ELAN_HDR_TYPE_RECV;
        frag->elan_event = elan_tportRxStart( elan_btl->tport, 0/*ELAN_TPORT_RXANY*/, 0, 0, 0, 0,
                                              frag->base.des_dst->seg_addr.pval,
                                              mca_btl_elan_module.super.btl_eager_limit );
        /* Add the fragment to the pending RDMA list */
        opal_list_append( &(elan_btl->recv_list), (opal_list_item_t*)frag );
    }
    return OMPI_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
static int mca_btl_elan_del_procs( struct mca_btl_base_module_t* btl, 
                                   size_t nprocs, 
                                   struct ompi_proc_t **procs, 
                                   struct mca_btl_base_endpoint_t ** endpoints )
{
    return OMPI_SUCCESS;
}

/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
static mca_btl_base_descriptor_t*
mca_btl_elan_alloc( struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* peer,
                    uint8_t order,
                    size_t size,
                    uint32_t flags )
{
    mca_btl_elan_frag_t* frag;
    int rc;

    if( size <= btl->btl_eager_limit ) { 
        MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc); 
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }
        frag->segment.seg_len = size;
    } else if( size <= btl->btl_max_send_size ) {
        MCA_BTL_ELAN_FRAG_ALLOC_MAX(frag, rc); 
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }
        frag->segment.seg_len = size;
    } else {
        return NULL;
    }
    frag->segment.seg_addr.pval = (void*)(frag+1);
    frag->base.des_src = &(frag->segment);
    frag->base.des_src_cnt = 1;   
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0; 
    frag->btl = (mca_btl_elan_module_t*)btl;
    frag->endpoint = peer;
    frag->base.order = MCA_BTL_NO_ORDER;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */
static int mca_btl_elan_free( struct mca_btl_base_module_t* btl, 
                              mca_btl_base_descriptor_t* des ) 
{
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)des;
    MCA_BTL_ELAN_FRAG_RETURN(frag); 
    return OMPI_SUCCESS; 
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
static mca_btl_base_descriptor_t*
mca_btl_elan_prepare_src( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct mca_mpool_base_registration_t* registration,
                          struct ompi_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags)
{
    mca_btl_elan_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if( OPAL_UNLIKELY(max_data > UINT32_MAX) ) {  
        max_data = (size_t)UINT32_MAX;
    }
    if( max_data+reserve <= btl->btl_eager_limit ) {
        MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc);
        if( NULL == frag ) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = (void*)((unsigned char*) frag->segment.seg_addr.pval + reserve);
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_ELAN_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = frag+1;
        frag->segment.seg_len = max_data + reserve;
    }
    else if( max_data+reserve <= btl->btl_max_send_size ) {
        MCA_BTL_ELAN_FRAG_ALLOC_MAX(frag, rc);
        if( NULL == frag ) {
            return NULL;
        }
                                                                                                   
        if(max_data + reserve > btl->btl_max_send_size){
            max_data = btl->btl_max_send_size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
                                                                                                    
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );                                                                                                    
        if( rc < 0 ) {
            MCA_BTL_ELAN_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = frag+1;
        *size  = max_data;
        frag->segment.seg_len = max_data + reserve;
    } else {
        MCA_BTL_ELAN_FRAG_ALLOC_USER(frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        frag->type = MCA_BTL_ELAN_HDR_TYPE_PUT;
        iov.iov_len = max_data;
        iov.iov_base = NULL;
        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data);
        *size = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = max_data;
    }
    frag->base.des_src = &(frag->segment);
    frag->base.des_src_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
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

static mca_btl_base_descriptor_t*
mca_btl_elan_prepare_dst( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct mca_mpool_base_registration_t* registration,
                          struct ompi_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags )
{
    mca_btl_elan_frag_t* frag;
    int rc;

    if( OPAL_UNLIKELY((*size) > UINT32_MAX) ) {  
        *size = (size_t)UINT32_MAX;
    }
    MCA_BTL_ELAN_FRAG_ALLOC_USER(frag, rc);
    if( NULL == frag ) {
        return NULL;
    }
    ompi_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );
    frag->segment.seg_len = *size;
    frag->segment.seg_key.key64 = (uint64_t)(intptr_t)convertor;
    /*frag->segment.seg_addr.pval = convertor->pBaseBuf + convertor->bConverted;*/
    frag->type         = MCA_BTL_ELAN_HDR_TYPE_PUT;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_flags = 0;	
    frag->base.des_dst = &(frag->segment);
    frag->base.des_dst_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
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

static int mca_btl_elan_send( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              struct mca_btl_base_descriptor_t* descriptor, 
                              mca_btl_base_tag_t tag )
   
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)descriptor; 
    int peer, proc, send_len;
    void* sbuf = NULL;

    frag->btl = elan_btl;
    frag->endpoint = endpoint;
    frag->tag = tag;
    frag->type = MCA_BTL_ELAN_HDR_TYPE_SEND;
    peer = endpoint->elan_vp;
    proc = elan_btl->elan_vp;
    sbuf     = (void*)frag->base.des_src->seg_addr.pval;
    send_len = frag->base.des_src->seg_len; 
    frag->elan_event = elan_tportTxStart( elan_btl->tport, 0, peer, proc, frag->tag,
                                          sbuf, send_len) ;
    if( elan_tportTxDone(frag->elan_event) ) {
        elan_tportTxWait(frag->elan_event);
        frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint,
                               &(frag->base), OMPI_SUCCESS );
        return OMPI_SUCCESS;
    }

    /* Add the fragment to the pending send list */
    opal_list_append( &(elan_btl->send_list), (opal_list_item_t*)frag );
    return OMPI_SUCCESS;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
static int mca_btl_elan_put( mca_btl_base_module_t* btl,
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_base_descriptor_t* des )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*) des; 
    int     peer = endpoint->elan_vp;
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    unsigned char* src_addr = (unsigned char*)src->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = (unsigned char*)ompi_ptr_ltop(dst->seg_addr.lval);
    frag->endpoint = endpoint;
    frag->btl = elan_btl;
    frag->type = MCA_BTL_ELAN_HDR_TYPE_PUT;
    /* opal_output(0, "put from %p to %d peer , %d\n", src_addr, peer, src_len); */
    frag->elan_event = elan_put(elan_btl->state, src_addr, dst_addr, src_len, peer);
    /* Add the fragment to the pending RDMA list */
    opal_list_append( &(elan_btl->rdma_list), (opal_list_item_t*)frag );
    return OMPI_SUCCESS;
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

static int mca_btl_elan_get( mca_btl_base_module_t* btl,
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_base_descriptor_t* des )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*) des; 
    int     peer = endpoint->elan_vp;
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    unsigned char* src_addr = (unsigned char*)src->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = (unsigned char*)ompi_ptr_ltop(dst->seg_addr.lval);
    /*size_t dst_len = dst->seg_len;*/
    frag->endpoint = endpoint;
    frag->btl = elan_btl;
    frag->type = MCA_BTL_ELAN_HDR_TYPE_GET;
    /*opal_output(0, "get from %p to %d peer , %d\n", src_addr, peer, src_len); */
    frag->elan_event = elan_get(elan_btl->state, src_addr, dst_addr, src_len, peer);
    /* Add the fragment to the pending RDMA list */
    opal_list_append( &(elan_btl->rdma_list), (opal_list_item_t*)frag );
    return OMPI_SUCCESS;
}

int mca_btl_elan_finalize( struct mca_btl_base_module_t* btl )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl; 
    int i, num_btls;

    /* First find the correct BTL in the list attached to the component */
    num_btls = mca_btl_elan_component.elan_num_btls;
    for( i = 0; i < num_btls; i++ ) {
        if( elan_btl == mca_btl_elan_component.elan_btls[i] ) {
            /* Get rid of the BTL */
            if( i == (num_btls-1) ) {
                mca_btl_elan_component.elan_btls[i] = NULL;
            } else {
                mca_btl_elan_component.elan_btls[i] =  mca_btl_elan_component.elan_btls[num_btls-1];
            }
            /* disable the network */
            elan_disable_network( elan_btl->state );
#if 0
            /* Cancel all pending receives */
            {
                mca_btl_elan_frag_t* frag;
    
                while( NULL != (frag = (mca_btl_elan_frag_t*)opal_list_remove_first(&elan_btl->recv_list)) ) {
                    if( elan_tportRxCancel(frag->elan_event) ) {
                        MCA_BTL_ELAN_FRAG_RETURN(frag);
                    }
                    frag = (mca_btl_elan_frag_t*)opal_list_remove_first(&(elan_btl->recv_list));
                }
            }
#endif
            /* Release the mutex */
            OBJ_DESTRUCT(&elan_btl->elan_lock);
            OBJ_DESTRUCT(&elan_btl->recv_list);
            OBJ_DESTRUCT(&elan_btl->send_list);
            OBJ_DESTRUCT(&elan_btl->rdma_list);
            /* The BTL is clean, remove it */
            free(elan_btl);

            return OMPI_SUCCESS;
        }
    }
    /* This BTL is not present in the list attached to the communicator */
    return OMPI_ERROR;
}

int mca_btl_elan_ft_event(int state)
{
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

    return OMPI_SUCCESS;
}

static void mca_btl_elan_dump( struct mca_btl_base_module_t* btl,
                                  struct mca_btl_base_endpoint_t* endpoint,
                                  int verbose )
{
}

static int
mca_btl_elan_register_error( struct mca_btl_base_module_t* btl, 
                             mca_btl_base_module_error_cb_fn_t cbfunc )
{
    return OMPI_SUCCESS;
}

mca_btl_elan_module_t mca_btl_elan_module = {
    {
        &mca_btl_elan_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* btl_rdma_pipeline_offset */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */				
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_elan_add_procs,
        mca_btl_elan_del_procs,
        NULL,  /* btl_register */
        mca_btl_elan_finalize,
        mca_btl_elan_alloc, 
        mca_btl_elan_free, 
        mca_btl_elan_prepare_src,
        mca_btl_elan_prepare_dst,
        mca_btl_elan_send,
        mca_btl_elan_put,
        mca_btl_elan_get,
        mca_btl_elan_dump,
        NULL, /* mpool */
        mca_btl_elan_register_error, /* register error cb */
        mca_btl_elan_ft_event /* mca_btl_elan_ft_event*/
    }
};
