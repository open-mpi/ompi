/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ptl_mx.h"
#include "include/constants.h"
#include "util/output.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_proc.h"
#include "ptl_mx_module.h"


static mca_ptl_mx_module_t* mca_ptl_mx_create(uint64_t addr);

static void* mca_ptl_mx_mem_alloc( size_t* size )
{
    return malloc(*size);
}


/**
 * Initialize MX PTL modules
 */

int mca_ptl_mx_module_init(void)
{
    size_t size;
    uint32_t i;
    int rc;
    uint64_t *nic_addrs;
    mx_endpoint_addr_t *endpoint_addrs;
    mx_return_t status;

    /* intialize MX library */
    if(MX_SUCCESS != (status = mx_init())) {
        ompi_output(0, "mca_ptl_mx_init: mx_init() failed with status=%d\n", status);
        return OMPI_ERR_INIT;
    }

    /* determine the number of NICs */
    if((status = mx_get_info(
        NULL,
        MX_NIC_COUNT,
        &mca_ptl_mx_component.mx_num_ptls,
        sizeof(uint32_t))) != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_init: mx_get_info(MX_NIC_COUNT) failed with status=%d\n", status);
        return OMPI_ERR_INIT;
    }

    /* determine the NIC ids */
    size = sizeof(uint64_t) * (mca_ptl_mx_component.mx_num_ptls+1);
    if(NULL == (nic_addrs = (uint64_t*)malloc(size)))
        return OMPI_ERR_OUT_OF_RESOURCE;
    if((status = mx_get_info(
        NULL,
        MX_NIC_IDS,
        nic_addrs,
        size)) != MX_SUCCESS) {
        free(nic_addrs);
        return OMPI_ERR_INIT;
    }

    /* check for limit on number of ptls */
    if(mca_ptl_mx_component.mx_num_ptls > mca_ptl_mx_component.mx_max_ptls)
       mca_ptl_mx_component.mx_num_ptls = mca_ptl_mx_component.mx_max_ptls;

    /* allocate an array of pointers to ptls */
    mca_ptl_mx_component.mx_ptls = (mca_ptl_mx_module_t**)malloc(
        sizeof(mca_ptl_mx_module_t*) * mca_ptl_mx_component.mx_num_ptls);
    if(NULL == mca_ptl_mx_component.mx_ptls) {
        ompi_output(0, "mca_ptl_mx_init: malloc() failed\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* create a ptl for each NIC */
    for(i=0; i<mca_ptl_mx_component.mx_num_ptls; i++) {
        mca_ptl_mx_module_t* ptl = mca_ptl_mx_create(nic_addrs[i]);
        if(NULL == ptl) {
            return OMPI_ERR_INIT;
        }
        mca_ptl_mx_component.mx_ptls[i] = ptl;
    }
    free(nic_addrs);

    /* post local endpoint addresses */
    size = mca_ptl_mx_component.mx_num_ptls * sizeof(mx_endpoint_addr_t);
    endpoint_addrs = (mx_endpoint_addr_t*)malloc(size);
    if(NULL == endpoint_addrs) {
        ompi_output(0, "mca_ptl_mx_module_init: malloc() failed\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for(i=0; i<mca_ptl_mx_component.mx_num_ptls; i++) {
        mca_ptl_mx_module_t* ptl = mca_ptl_mx_component.mx_ptls[i];
        endpoint_addrs[i] = ptl->mx_endpoint_addr;
    }
    if((rc = mca_base_modex_send(
        &mca_ptl_mx_component.super.ptlm_version, 
        endpoint_addrs, 
        mca_ptl_mx_component.mx_num_ptls * sizeof(mx_endpoint_addr_t))) != OMPI_SUCCESS)
        return rc;
    return OMPI_SUCCESS;
}


/**
 * Thread to progress outstanding requests.
 */

#if OMPI_HAVE_THREADS

static void* mca_ptl_mx_thread(ompi_object_t *arg)
{
    ompi_thread_t* thr = (ompi_thread_t*)arg;
    mca_ptl_mx_module_t* ptl = thr->t_arg;
    while(ptl->mx_enabled) {
        mx_request_t mx_request;
        mx_return_t mx_return;
        mx_status_t mx_status;
        uint32_t mx_result;

        /* block waiting for status */
        mx_return = mx_peek(
            ptl->mx_endpoint, 
            UINT_MAX,
            &mx_request,
            &mx_result);
        if(mx_return == MX_TIMEOUT)
            continue;
        else if(ptl->mx_enabled == false)
            break;
        else if(mx_return != MX_SUCCESS) {
            ompi_output(0, "mca_ptl_mx_thread: mx_probe() failed with status %d\n", 
                mx_return);
            break;
        }
        
        mx_return = mx_test(
            ptl->mx_endpoint,
            &mx_request,
            &mx_status,
            &mx_result);
        if(mx_return == MX_SUCCESS) {
            MCA_PTL_MX_PROGRESS(ptl, mx_status);
        } else {
            ompi_output(0, "mca_ptl_mx_progress: mx_test() failed with status=%dn",
                mx_return);
        }
    }
    return NULL;
}

#endif
                                        
/*
 * Callback on a match.
 *
 */

static void mca_ptl_mx_match(void* context, uint64_t match_value, int size)
{
    mca_ptl_mx_module_t* ptl = (mca_ptl_mx_module_t*)context;
	mca_pml_base_recv_request_t* request;
    mca_ptl_mx_recv_frag_t *frag;
    mx_return_t mx_return;
    ompi_ptr_t match;
    uint32_t offset;
    ompi_proc_t* proc;
    ompi_convertor_t* convertor;
    int rc;

    /* use of the sizeof of the header as the match value */
    if(match_value <= MCA_PTL_HDR_TYPE_MAX)
        return;

    /* otherwise extract request pointer and offset */
    match.lval = match_value;
    request = (mca_pml_base_recv_request_t*)match.sval.uval;
    offset = match.sval.lval;
    proc = ompi_comm_peer_lookup(request->req_base.req_comm,
    	request->req_base.req_ompi.req_status.MPI_SOURCE);

    /* allocate a fragment for receive */
    MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc); 
    if(rc != OMPI_SUCCESS) { 
        ompi_output(0, "mca_ptl_mx_match: unable to allocate resources.\n");
        return;
    }

    frag->frag_size = size;
    frag->frag_recv.frag_request = request;
    frag->frag_recv.frag_base.frag_peer = NULL; 
    frag->frag_recv.frag_base.frag_owner = &ptl->super; 
    frag->frag_recv.frag_base.frag_size = frag->frag_size;
    frag->frag_recv.frag_base.frag_header.hdr_common.hdr_type = 
        MCA_PTL_HDR_TYPE_FRAG;
    frag->frag_recv.frag_base.frag_header.hdr_common.hdr_flags = 0;
    convertor = &frag->frag_recv.frag_base.frag_convertor;

    /* initialize convertor */
    ompi_convertor_copy(proc->proc_convertor, convertor);
    ompi_convertor_init_for_recv(
    	convertor,
    	0,                              /* flags */
    	request->req_base.req_datatype, /* datatype */
    	request->req_base.req_count,    /* count elements */
    	request->req_base.req_addr,     /* users buffer */
    	offset,                         /* offset in bytes into packed buffer */
    	mca_ptl_mx_mem_alloc );         /* not allocating memory */
                                                      
    /* non-contiguous - allocate buffer for receive */
    if( 1 == ompi_convertor_need_buffers( convertor )  ||
    	request->req_bytes_packed < offset + frag->frag_size ) {

    	/* TODO - use a fixed fragment size for non-contigous and convert 
    	 * this to a free-list of buffers.
    	*/
    	frag->frag_recv.frag_is_buffered = true;
    	frag->frag_recv.frag_base.frag_addr = malloc(frag->frag_size);
    	if( NULL == frag->frag_recv.frag_base.frag_addr ) {
            ompi_output(0, "mca_ptl_mx_match: unable to allocate buffer (%d)\n", frag->frag_size);
            MCA_PTL_MX_RECV_FRAG_RETURN(frag);
            return;
    	}

    	/* check for sending more than receiving */
    	if( offset > request->req_bytes_packed ) {
            frag->frag_recv.frag_base.frag_size = 0;
    	} else if (offset + frag->frag_size > request->req_bytes_packed ) {
            frag->frag_recv.frag_base.frag_size = request->req_bytes_packed - offset;
    	}
    /* calculate offset into users buffer */
    } else {
    	frag->frag_recv.frag_base.frag_addr = ((unsigned char*)request->req_base.req_addr) + offset;
    	frag->frag_recv.frag_is_buffered = false;
    }

    /* dont receive a header */
    frag->frag_segment_count = 1;
    frag->frag_segments[1].segment_ptr = frag->frag_recv.frag_base.frag_addr;
    frag->frag_segments[1].segment_length = frag->frag_size;

    mx_return = mx_irecv(
        ptl->mx_endpoint,
        frag->frag_segments+1,
        frag->frag_segment_count,
        match_value,
        MX_MATCH_MASK_NONE,
        frag,
        &frag->frag_request);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_match: mx_irecv() failed with status=%dn", mx_return);
        MCA_PTL_MX_RECV_FRAG_RETURN(frag);
    }
}


/*
 * Create and intialize an MX PTL module, where each module
 * represents a specific NIC.
 */

static mca_ptl_mx_module_t* mca_ptl_mx_create(uint64_t addr)
{
    mca_ptl_mx_module_t* ptl = malloc(sizeof(mca_ptl_mx_module_t));
    mx_return_t status;
    if(NULL == ptl)
        return NULL;

    /* copy over default settings */
    memcpy(ptl, &mca_ptl_mx_module, sizeof(mca_ptl_mx_module_t));
    OBJ_CONSTRUCT(&ptl->mx_peers, ompi_list_t);
                        	    
    /* open local endpoint */
    status = mx_open_endpoint(
        addr,
        1,
        mca_ptl_mx_component.mx_filter,
        NULL,
        0,
        &ptl->mx_endpoint);
    if(status != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_init: mx_open_endpoint() failed with status=%d\n", status);
        mca_ptl_mx_finalize(&ptl->super);
        return NULL;
    }
                        	    
    /* query the endpoint address */
    if((status = mx_get_endpoint_addr(
        ptl->mx_endpoint,
        &ptl->mx_endpoint_addr)) != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_init: mx_get_endpoint_addr() failed with status=%d\n", status);
        mca_ptl_mx_finalize(&ptl->super);
        return NULL;
    }

    /* breakup the endpoint address */
    if((status = mx_decompose_endpoint_addr(
        ptl->mx_endpoint_addr,
        &ptl->mx_nic_addr,
        &ptl->mx_endpoint_id,
        &ptl->mx_filter)) != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_init: mx_decompose_endpoint_addr() failed with status=%d\n", status);
        mca_ptl_mx_finalize(&ptl->super);
        return NULL;
    }

    if(mca_ptl_mx_component.mx_debug) {
        ompi_output(0, "mca_ptl_mx_create: opened %08X:%08X:%08X:%08X\n", 
            (uint32_t)(ptl->mx_nic_addr >> 32), 
            (uint32_t)ptl->mx_nic_addr,
            ptl->mx_endpoint_id,
            ptl->mx_filter);
    }

    /* prepost a receive buffer */
    ptl->mx_recvs_posted = 1;
    MCA_PTL_MX_POST(ptl, MCA_PTL_HDR_TYPE_MATCH, sizeof(mca_ptl_base_match_header_t));
    MCA_PTL_MX_POST(ptl, MCA_PTL_HDR_TYPE_RNDV, sizeof(mca_ptl_base_rendezvous_header_t));
    MCA_PTL_MX_POST(ptl, MCA_PTL_HDR_TYPE_ACK, sizeof(mca_ptl_base_ack_header_t));

    /* register a callback function for matching */
    mx_register_match_callback(ptl->mx_endpoint, mca_ptl_mx_match, ptl);
    return ptl;
}

void mca_ptl_mx_enable()
{
    size_t i;
    for(i=0; i<mca_ptl_mx_component.mx_num_ptls; i++) {
        mca_ptl_mx_module_t* ptl = mca_ptl_mx_component.mx_ptls[i];
        ptl->mx_enabled = true;
#if OMPI_HAVE_THREADS
        /* create a thread to progress requests */
        OBJ_CONSTRUCT(&ptl->mx_thread, ompi_thread_t);
        ptl->mx_thread.t_run = mca_ptl_mx_thread;
        ptl->mx_thread.t_arg = ptl; 
        if(ompi_thread_start(&ptl->mx_thread) != OMPI_SUCCESS) {
            ompi_output(0, "mca_ptl_mx_create: unable to start progress thread.\n");
            return;
        }
#endif
    }
}

void mca_ptl_mx_disable(void)
{
    size_t i;
    for(i=0; i<mca_ptl_mx_component.mx_num_ptls; i++) {
        mca_ptl_mx_module_t* ptl = mca_ptl_mx_component.mx_ptls[i];
        ptl->mx_enabled = false;
#if OMPI_HAVE_THREADS
        mx_wakeup(ptl->mx_endpoint);
        ompi_thread_join(&ptl->mx_thread, NULL);
#endif
    }
}

/*
 * Cleanup PTL resources.
 */

int mca_ptl_mx_finalize(struct mca_ptl_base_module_t* ptl)
{
    mca_ptl_mx_module_t* mx_ptl = (mca_ptl_mx_module_t*)ptl;
    mx_ptl->mx_enabled = false;
#if OMPI_HAVE_THREADS
    mx_wakeup(mx_ptl->mx_endpoint);
    ompi_thread_join(&mx_ptl->mx_thread, NULL);
#endif
    mx_close_endpoint(mx_ptl->mx_endpoint);
    free(mx_ptl);
    return OMPI_SUCCESS;
}
                                                                                                                    
/**
 * PML->PTL notification of addition to the process list.
 *
 * @param ptl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this PTL.
 * @return                OMPI_SUCCESS or error status on failure.
 *
 */
                                                                                                                    
int mca_ptl_mx_add_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_ptl_base_peer_t** peers,
    ompi_bitmap_t* reachable)
{ 
    ompi_proc_t* proc_self = ompi_proc_local();
    mca_ptl_mx_module_t* ptl_mx = (mca_ptl_mx_module_t*)ptl;
    size_t n;
    for( n = 0; n < nprocs; n++ ) {
        int rc;
        mca_ptl_mx_proc_t *ptl_proc = mca_ptl_mx_proc_create(procs[n]);
        mca_ptl_mx_peer_t* ptl_peer;

        if(ptl_proc == NULL)
            return OMPI_ERR_OUT_OF_RESOURCE;

        /* peer doesn't export enough addresses */
        OMPI_THREAD_LOCK(&ptl_proc->proc_lock);
        if(ptl_proc->proc_peer_count == ptl_proc->proc_addr_count) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            continue;
        }

        /* The ptl_proc datastructure is shared by all MX PTL instances that are trying
         * to reach this destination. Cache the peer instance on the proc.
         */
        ptl_peer = OBJ_NEW(mca_ptl_mx_peer_t);
        if(NULL == ptl_peer) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        ptl_peer->peer_ptl = ptl_mx;
        rc = mca_ptl_mx_proc_insert(ptl_proc, ptl_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ptl_peer);
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return rc;
        }
        /* do we need to convert to/from network byte order */
        if(procs[n]->proc_arch != proc_self->proc_arch) {
            ptl_peer->peer_nbo = true;
        }

        /* set peer as reachable */
        ompi_bitmap_set_bit(reachable, n);
        OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
        peers[n] = ptl_peer;
        ompi_list_append(&ptl_mx->mx_peers, (ompi_list_item_t*)ptl_peer);
    }
    return OMPI_SUCCESS;
}
                                                                                                                    
                                                                                                                    
/**
 * PML->PTL notification of change in the process list.
 *
 * @param ptl (IN)     PTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
                                                                                                                    
int mca_ptl_mx_del_procs(
    struct mca_ptl_base_module_t* ptl,
    size_t nprocs,
    struct ompi_proc_t **proc,
    struct mca_ptl_base_peer_t** ptl_peer)
{
    return OMPI_SUCCESS;
}


