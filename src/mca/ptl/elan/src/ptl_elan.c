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
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_req.h"
#include "ptl_elan_frag.h"


/* XXX: There must be multiple PTL's. This could be the template */
mca_ptl_elan_t  mca_ptl_elan = {
    {
        &mca_ptl_elan_module.super,
        0,                         /* ptl_exclusivity */
        0,                         /* ptl_latency */
        0,                         /* ptl_bandwidth */
        0,                         /* ptl_frag_first_size */
        0,                         /* ptl_frag_min_size */
        0,                         /* ptl_frag_max_size */
        MCA_PTL_PUT,               /* ptl flags */
        
        /* collection of interfaces */
        mca_ptl_elan_add_proc,
        mca_ptl_elan_del_proc,
        mca_ptl_elan_finalize,
        mca_ptl_elan_put,
        mca_ptl_elan_get,
        mca_ptl_elan_matched,
        mca_ptl_elan_req_alloc,
        mca_ptl_elan_req_return
    }
};

int mca_ptl_elan_add_proc (struct mca_ptl_t *ptl,
	                   size_t nprocs,
                           struct ompi_proc_t **ompi_proc,
                           struct mca_ptl_base_peer_t **peer_ret,
			   ompi_bitmap_t* reachable)
{
    return OMPI_SUCCESS;
}

int mca_ptl_elan_del_proc (struct mca_ptl_t *ptl, 
			   size_t nprocs,
			   struct ompi_proc_t ** procs, 
			   struct mca_ptl_base_peer_t **ptl_peer)
{
    return OMPI_SUCCESS;
}

int mca_ptl_elan_finalize (struct mca_ptl_t *ptl)
{
    int rail_index;
    struct mca_ptl_elan_t * elan_ptl ;

    elan_ptl = (struct mca_ptl_elan_t *) ptl;

    /* XXX: Free all the lists, etc, hanged over PTL */

    /* Free the PTL */
    rail_index = elan_ptl->ptl_ni_local;
    free(elan_ptl);

    /* Record the missing of this entry */
    mca_ptl_elan_module.elan_ptls[rail_index] = NULL;
    mca_ptl_elan_module.elan_num_ptls -- ;

    return OMPI_SUCCESS;
}

int mca_ptl_elan_req_alloc (struct mca_ptl_t *ptl, 
        struct mca_ptl_base_send_request_t **request)
{
    int             rc;
    return rc;
}


void mca_ptl_elan_req_return (struct mca_ptl_t *ptl, 
        struct mca_ptl_base_send_request_t *request)
{
    return;
}


void mca_ptl_elan_recv_frag_return (struct mca_ptl_t *ptl,
                                    struct mca_ptl_elan_recv_frag_t *frag)
{
    return;
}


void mca_ptl_elan_send_frag_return (struct mca_ptl_t *ptl,
                                    struct mca_ptl_elan_send_frag_t *frag)
{
    return;
}

/*
 *  Initiate a put operation. 
 */

int mca_ptl_elan_put (struct mca_ptl_t* ptl, 
		      struct mca_ptl_base_peer_t* ptl_base_peer, 
		      struct mca_ptl_base_send_request_t* request,
		      size_t offset,
		      size_t size,
		      int flags)
{
    return OMPI_SUCCESS;
}

/*
 *  Initiate a get. 
 */

int mca_ptl_elan_get (struct mca_ptl_t* ptl, 
		      struct mca_ptl_base_peer_t* ptl_base_peer, 
		      struct mca_ptl_base_recv_request_t* request,
		      size_t offset,
		      size_t size,
		      int flags)
{
    return OMPI_SUCCESS;
}

/*
 *  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void mca_ptl_elan_matched (mca_ptl_t * ptl,
                           mca_ptl_base_recv_frag_t * frag)
{
    return;
}
