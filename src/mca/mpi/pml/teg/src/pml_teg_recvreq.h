/*
 * $HEADER$
 */

#ifndef LAM_PML_TEG_RECV_REQUEST_H
#define LAM_PML_TEG_RECV_REQUEST_H

#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"


/*
 *  Allocate a recv request. 
 */
static inline mca_ptl_base_recv_request_t* mca_pml_teg_recv_request_alloc(int *rc)
{
    return (mca_ptl_base_recv_request_t*)lam_free_list_get(&mca_pml_teg.teg_recv_requests, rc);
}

static void mca_pml_teg_recv_request_return(mca_ptl_base_recv_request_t* request)
{
    lam_free_list_return(&mca_pml_teg.teg_recv_requests, (lam_list_item_t*)request);
}

/*
 * Progress an initialized request.
 */
int mca_pml_teg_recv_request_start(mca_ptl_base_recv_request_t*);


#endif

