#include "ptl_mx.h"
#include "include/constants.h"
#include "util/output.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_proc.h"
#include "ptl_mx_recvfrag.h"
#include "ptl_mx_sendfrag.h"


static mca_ptl_mx_module_t* mca_ptl_mx_create(uint64_t addr);


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
 * Prepost recv buffers
 */

static inline int mca_ptl_mx_post(mca_ptl_mx_module_t* ptl)
{
    mca_ptl_mx_recv_frag_t* frag;
    mx_return_t status;
    int rc;
    /* post an additional recv */
    MCA_PTL_MX_RECV_FRAG_ALLOC(frag, rc);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_thread: unable to allocate recv frag\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_ptl_mx_recv_frag_init(frag, ptl);
    status = mx_irecv(
        ptl->mx_endpoint,
        frag->frag_segments,
        frag->frag_segment_count,
        1,
        MX_MATCH_MASK_NONE,
        frag,
        &frag->frag_request);
    if(status != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_post: mx_irecv() failed with status=%d\n", status);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


/**
 *  Routine to process complete request(s).
 */

static inline void mca_ptl_mx_progress(mca_ptl_mx_module_t* ptl, mx_request_t mx_request)
{
    mx_return_t mx_return;
    mx_status_t mx_status;
    uint32_t mx_result;
    mca_ptl_base_frag_t* frag;

    mx_return = mx_test(
        ptl->mx_endpoint, 
        &mx_request, 
        &mx_status,
        &mx_result);
    if(mx_return != MX_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_progress: mx_test() failed with status=%d\n", mx_return);
        return;
    }

    frag = (mca_ptl_base_frag_t*)mx_status.context;
    switch(frag->frag_type) {
        case MCA_PTL_FRAGMENT_SEND: 
        {
            mca_ptl_mx_send_frag_handler((mca_ptl_mx_send_frag_t*)frag, ptl);
            break;
        }
        case MCA_PTL_FRAGMENT_RECV:
        {
            mca_ptl_mx_recv_frag_handler((mca_ptl_mx_recv_frag_t*)frag, ptl);
            mca_ptl_mx_post(ptl);
            break;
        }
        default:
        {
            ompi_output(0, "mca_ptl_mx_progress: invalid request type: %d\n", frag->frag_type);
            break;
        }
    }
}

/**
 * Thread to progress outstanding requests.
 */

#if OMPI_HAVE_THREADS

static void* mca_ptl_mx_thread(ompi_object_t *arg)
{
    ompi_thread_t* thr = (ompi_thread_t*)arg;
    mca_ptl_mx_module_t* ptl = thr->t_arg;
    while(ptl->mx_thread_run) {
        mx_request_t mx_request;
        mx_return_t mx_return;
        uint32_t mx_result;

        /* block waiting for status */
        mx_return = mx_peek(
            ptl->mx_endpoint, 
            UINT_MAX,
            &mx_request,
            &mx_result);
        if(mx_return == MX_TIMEOUT)
            continue;
        if(mx_return != MX_SUCCESS) {
            ompi_output(0, "mca_ptl_mx_thread: mx_probe() failed with status %d\n", 
                mx_return);
            break;
        }

        /* process the pending request */
        mca_ptl_mx_progress(ptl, mx_request);
    }
    return NULL;
}

#endif
                                        
/*
 * Create and intialize an MX PTL module, where each module
 * represents a specific NIC.
 */

static mca_ptl_mx_module_t* mca_ptl_mx_create(uint64_t addr)
{
    mca_ptl_mx_module_t* ptl = malloc(sizeof(mca_ptl_mx_module_t));
    mx_return_t status;
    int i;
    if(NULL == ptl)
        return NULL;

    /* copy over default settings */
    memcpy(ptl, &mca_ptl_mx_module, sizeof(mca_ptl_mx_module_t));
    OBJ_CONSTRUCT(&ptl->mx_peers, ompi_list_t);
													    
    /* open local endpoint */
    status = mx_open_endpoint(
        addr,
        MX_ANY_ENDPOINT,
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

    /* pre-post receive buffers */
    for(i=0; i<mca_ptl_mx_component.mx_prepost; i++) {
        if(mca_ptl_mx_post(ptl) != OMPI_SUCCESS) {
            mca_ptl_mx_finalize(&ptl->super);
            return NULL;
        }
    }

#if OMPI_HAVE_THREADS
    /* create a thread to progress requests */
    OBJ_CONSTRUCT(&ptl->mx_thread, ompi_thread_t);
    ptl->mx_thread.t_run = mca_ptl_mx_thread;
    ptl->mx_thread.t_arg = ptl; 
    ptl->mx_thread_run = true;
    if(ompi_thread_start(&ptl->mx_thread) != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_mx_create: unable to start progress thread.\n");
        free(ptl);
        return NULL;
    }
#endif
    return ptl;
}


/*
 * Cleanup PTL resources.
 */

int mca_ptl_mx_finalize(struct mca_ptl_base_module_t* ptl)
{
    mca_ptl_mx_module_t* ptl_mx = (mca_ptl_mx_module_t*)ptl;
#if OMPI_HAVE_THREADS
    ptl_mx->mx_thread_run = false;
    ompi_thread_join(&ptl_mx->mx_thread, NULL);
#endif
    free(ptl_mx);
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
        if(procs[n]->proc_arch != proc_self->proc_arch)
            ptl_peer->peer_byte_swap = true;

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


