/*
 * $HEADER$
 */

#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_sm.h"


mca_ptl_sm_t mca_ptl_sm = {
    {
    &mca_ptl_sm_module.super,
    0, /* ptl_exclusivity */
    0, /* ptl_latency */
    0, /* ptl_andwidth */
    0, /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    0, /* ptl_frag_max_size */
    MCA_PTL_PUT,  /* ptl flags */
    mca_ptl_sm_add_procs,
    mca_ptl_sm_del_procs,
    mca_ptl_sm_finalize,
    mca_ptl_sm_send,
    NULL,
    mca_ptl_sm_matched,
    mca_ptl_sm_request_alloc,
    mca_ptl_sm_request_return
    }
};


int mca_ptl_sm_add_procs(
    struct mca_ptl_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_ptl_base_peer_t **peers,
    ompi_bitmap_t* reachability)
{
    int proc,rc;
    size_t size;
    mca_ptl_sm_exchange_t **sm_proc_info;

    /* allocate array to hold setup shared memory from all
     * other procs */
    sm_proc_info=(mca_ptl_sm_exchange_t **)
        malloc(nprocs*sizeof(mca_ptl_sm_exchange_t *));
    if( NULL == sm_proc_info ){
        rc=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* get unique host identifier for each process in the list */
    for( proc=0 ; proc < nprocs; proc++ ) {
        rc = mca_base_modex_recv(
                &mca_ptl_sm_module.super.ptlm_version, proc,
                (void**)(sm_proc_info+proc), &size);
        if(rc != OMPI_SUCCESS) {
            ompi_output(0, "mca_ptl_sm_add_procs: mca_base_modex_recv: failed with return value=%d", rc);
            goto CLEANUP;
        }
    }

    /* free local memory */
    if(sm_proc_info){
        free(sm_proc_info);
    }

    return OMPI_SUCCESS;

CLEANUP:
    if(sm_proc_info){
        free(sm_proc_info);
    }
}


int mca_ptl_sm_del_procs(
    struct mca_ptl_t* ptl, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_ptl_base_peer_t **peers)
{
    return OMPI_SUCCESS;
}


int mca_ptl_sm_finalize(struct mca_ptl_t* ptl)
{
    return OMPI_SUCCESS;
}


int mca_ptl_sm_request_alloc(struct mca_ptl_t* ptl, struct mca_ptl_base_send_request_t** request)
{
    return OMPI_SUCCESS;
}


void mca_ptl_sm_request_return(struct mca_ptl_t* ptl, struct mca_ptl_base_send_request_t* request)
{
}


/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_ptl_sm_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t* sendreq,
    size_t offset,
    size_t *size,
    int flags)
{
    return OMPI_SUCCESS;
}


/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void mca_ptl_sm_matched(
    mca_ptl_t* ptl,
    mca_ptl_base_recv_frag_t* frag)
{
}


