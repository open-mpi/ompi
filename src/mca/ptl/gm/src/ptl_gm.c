/*
 * $HEADER$
 */

#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_gm.h"
#include "ptl_gm_addr.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_req.h"
#include "ptl_gm_req.c"
#include "ptl_gm_peer.h"

mca_ptl_gm_module_t mca_ptl_gm_module = {
    {
     &mca_ptl_gm_component.super,
    1, /* max size of request cache */
    sizeof(mca_ptl_gm_send_frag_t), /* bytes required by ptl for a request */
    0, /* max size of first fragment */
    0, /* min fragment size */
    0, /* max fragment size */
    0, /* exclusivity */
    0, /* latency */
    0, /* bandwidth */
    MCA_PTL_PUT,  /* ptl flags */

     /* collection of interfaces */
     mca_ptl_gm_add_procs,
     mca_ptl_gm_del_procs,
     mca_ptl_gm_finalize,
     NULL, /* JMS: Need send here */
     mca_ptl_gm_put,
     mca_ptl_gm_get,
     mca_ptl_gm_matched,
     NULL, /* JMS need request init here */
     NULL, /* JMS need request fini here */
     NULL, /* JMS need match here */
     NULL, /* JMS need send_progress here */
     NULL, /* JMS need recv_progress here */
    }
};


OBJ_CLASS_INSTANCE (mca_ptl_gm_recv_frag_t,
                    mca_ptl_base_recv_frag_t, NULL, NULL);

OBJ_CLASS_INSTANCE (mca_ptl_gm_send_request_t,
                    mca_pml_base_send_request_t, NULL, NULL);


OBJ_CLASS_INSTANCE (mca_ptl_gm_peer_t, ompi_list_item_t, NULL, NULL);




/*
 *
 */
int
mca_ptl_gm_add_procs (struct mca_ptl_base_module_t *ptl,
                      size_t nprocs,
                      struct ompi_proc_t **ompi_procs,
                      struct mca_ptl_base_peer_t **peers,
                      ompi_bitmap_t * reachable)
{
    int         i;
    struct ompi_proc_t *ompi_proc;
    mca_ptl_gm_proc_t *ptl_proc;
    mca_ptl_gm_peer_t *ptl_peer;

    for (i = 0; i < nprocs; i++) {
        ompi_proc = ompi_procs[i];
        ptl_proc =
            mca_ptl_gm_proc_create ((mca_ptl_gm_module_t *) ptl,
                                    ompi_proc);

        if (NULL == ptl_proc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }


        OMPI_THREAD_LOCK (&ptl_proc->proc_lock);
        if (ptl_proc->proc_addr_count == ptl_proc->proc_peer_count) {
            OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
            return OMPI_ERR_UNREACH;
        }

        ptl_peer = OBJ_NEW (mca_ptl_gm_peer_t);
        if (NULL == ptl_peer) {
            OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ptl_peer->peer_ptl = (mca_ptl_gm_module_t *) ptl;
        ptl_peer->peer_proc = ptl_proc;
        ptl_proc->peer_arr[ptl_proc->proc_peer_count] = ptl_peer;
        ptl_proc->proc_peer_count++;

        ptl_peer->peer_addr = ptl_proc->proc_addrs + i;
        ompi_bitmap_set_bit (reachable, i);
        OMPI_THREAD_UNLOCK (&ptl_proc->proc_lock);

        peers[i] = ptl_peer;
    }

    return OMPI_SUCCESS;
}




/*
 *
 */
int
mca_ptl_gm_del_procs (struct mca_ptl_base_module_t *ptl,
                      size_t nprocs,
                      struct ompi_proc_t **procs,
                      struct mca_ptl_base_peer_t **peers)
{
    size_t      i;
    for (i = 0; i < nprocs; i++) {
        OBJ_RELEASE (peers[i]);
    }
    return OMPI_SUCCESS;
}




/*
 *
 */

int
mca_ptl_gm_finalize (struct mca_ptl_base_module_t *ptl)
{
    free (ptl);
    return OMPI_SUCCESS;
}




/*
 *
 */

int
mca_ptl_gm_request_alloc (struct mca_ptl_base_module_t *ptl,
                          struct mca_pml_base_send_request_t **request)
{
    int         rc;
    mca_pml_base_send_request_t *sendreq;
    ompi_list_item_t *item;

#if 0
    OMPI_FREE_LIST_GET (&mca_ptl_gm_module.gm_send_req, item, rc);

    if (NULL != (sendreq = (mca_pml_base_send_request_t *) item))
        sendreq->req_owner = ptl;
    *request = sendreq;         /* the allocated memory must be registered */
#endif
    return rc;
}




/*
 *
 */
void
mca_ptl_gm_request_return (struct mca_ptl_base_module_t *ptl,
                           struct mca_pml_base_send_request_t *request)
{
    /*OMPI_FREE_LIST_RETURN(&mca_ptl_gm_module.gm_send_req,
       (ompi_list_item_t*)request); */
    return;
}





/*
 *  Initiate a put
 */

int
mca_ptl_gm_put (struct mca_ptl_base_module_t *ptl,
                struct mca_ptl_base_peer_t *ptl_peer,
                struct mca_pml_base_send_request_t *sendreq,
                size_t offset, size_t size, int flags)
{
#if 0
    mca_ptl_gm_send_frag_t *sendfrag;
    int         rc;

    if (offset == 0) {
        sendfrag = &((mca_ptl_gm_send_request_t *) sendreq)->req_frag;
    } else {
        ompi_list_item_t *item;
        OMPI_FREE_LIST_GET (&mca_ptl_gm_module.gm_send_frags, item, rc);
        if (NULL == (sendfrag = (mca_ptl_gm_send_frag_t *) item))
            return rc;
    }

    rc = mca_ptl_gm_send_frag_init (sendfrag, ptl_peer, sendreq, offset,
                                    &size, flags);

    if (rc != OMPI_SUCCESS)
        return rc;

    sendreq->req_offset += size;
    return mca_ptl_gm_peer_send (ptl_peer, sendfrag);
#endif
    return OMPI_SUCCESS;
}





/*
 *   initiate a get.
 */

int
mca_ptl_gm_get (struct mca_ptl_base_module_t *ptl,
                struct mca_ptl_base_peer_t *ptl_base_peer,
                struct mca_pml_base_recv_request_t *request,
                size_t offset, size_t size, int flags)
{
    return OMPI_SUCCESS;
}





/*  A posted receive has been matched - if required send an
 *  ack back to the peer and process the fragment.
 */

void
mca_ptl_gm_matched (mca_ptl_base_module_t * ptl,
                    mca_ptl_base_recv_frag_t * frag)
{

/* might need to send an ack back */
#if 0
    mca_ptl_base_header_t *header = &frag->super.frag_header;
    if (header->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK_MATCHED) {
        int         rc;
        mca_ptl_gm_send_frag_t *ack;
        mca_ptl_gm_recv_frag_t *recv_frag =
            (mca_ptl_gm_recv_frag_t *) frag;
        ompi_list_item_t *item;
        MCA_PTL_GM_SEND_FRAG_ALLOC (item, rc);
        ack = (mca_ptl_gm_send_frag_t *) item;

        if (NULL == ack) {
            OMPI_THREAD_LOCK (&mca_ptl_gm_module.gm_lock);
            recv_frag->frag_ack_pending = true;
            ompi_list_append (&mca_ptl_gm_module.gm_pending_acks,
                              (ompi_list_item_t *) frag);
            OMPI_THREAD_UNLOCK (&mca_ptl_gm_module.gm_lock);
        } else {
            mca_ptl_gm_send_frag_init_ack (ack, ptl,
                                           recv_frag->super.super.
                                           frag_peer, recv_frag);
            mca_ptl_gm_peer_send (ack->super.super.frag_peer, ack);
        }

    }
    /* process fragment if complete  */
#endif
}
