/*
 * $HEADER$
 */

#include "lam/mem/malloc.h"
#include "lam/util/output.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "ptl_tcp.h"


mca_ptl_tcp_t mca_ptl_tcp = {
    {
    &mca_ptl_tcp_module.super,
    0, /* ptl_exclusive */
    0, /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    0, /* ptl_frag_max_size */
    0, /* ptl_latency */
    0, /* ptl_andwidth */
    mca_ptl_tcp_add_procs,
    mca_ptl_tcp_fini,
    mca_ptl_tcp_send,
    mca_ptl_tcp_request_alloc
    }
};


int mca_ptl_tcp_create(int if_index)
{
    mca_ptl_tcp_t* ptl = (mca_ptl_tcp_t*)LAM_MALLOC(sizeof(mca_ptl_tcp_t));
    if(NULL == ptl) {
        lam_output(0,"mca_ptl_tcp: unable to allocate ptl");
        return LAM_ERR_OUT_OF_RESOURCE;
    }
    memcpy(ptl, &mca_ptl_tcp, sizeof(mca_ptl_tcp));
    mca_ptl_tcp_module.tcp_ptls[mca_ptl_tcp_module.tcp_num_ptls++] = ptl;

    /* initialize the ptl */
    ptl->tcp_ifindex = if_index;
    lam_ifindextoaddr(if_index, &ptl->tcp_addr, sizeof(ptl->tcp_addr));

    return LAM_SUCCESS;
}

