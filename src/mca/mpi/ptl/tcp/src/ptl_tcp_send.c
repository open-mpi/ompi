/* 
 * $HEADER$
 */
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_sendfrag.h"

                                                                                                                 
int mca_ptl_tcp_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t* sendreq,
    size_t size,
    bool* complete)
{
    mca_ptl_tcp_send_frag_t* sendfrag;
    if (sendreq->req_frags == 0) {
        sendfrag = (mca_ptl_tcp_send_frag_t*)(sendreq+1);
    } else { 
        int rc;
        sendfrag = (mca_ptl_tcp_send_frag_t*)lam_free_list_get(&mca_ptl_tcp_module.tcp_send_frags, &rc);
        if(sendfrag == 0)
            return rc;
    }
    mca_ptl_tcp_send_frag_reinit(sendfrag, ptl_peer, sendreq, size);
    return mca_ptl_tcp_peer_send(ptl_peer, sendfrag);
}

