
#include <sys/time.h>
#include <time.h>
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ns/base/base.h"
#include "ptl_ib.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_priv.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer);
static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer);

OBJ_CLASS_INSTANCE(mca_ptl_ib_peer_t, 
        ompi_list_item_t, mca_ptl_ib_peer_construct, 
        mca_ptl_ib_peer_destruct);

/*
 * Initialize state of the peer instance.
 */

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer)
{
    module_peer->peer_module = 0;
    module_peer->peer_proc = 0;
    module_peer->peer_addr = 0;
    module_peer->peer_ts = 0.0;
    module_peer->peer_send_frag = 0;
    module_peer->peer_recv_frag = 0;
    module_peer->peer_send_event.ev_flags = 0;
    module_peer->peer_recv_event.ev_flags = 0;
    module_peer->peer_state = MCA_PTL_IB_CLOSED;
    module_peer->peer_retries = 0;
    OBJ_CONSTRUCT(&module_peer->peer_frags, ompi_list_t);
    OBJ_CONSTRUCT(&module_peer->peer_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&module_peer->peer_recv_lock, ompi_mutex_t);
}

static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer)
{
}

static int mca_ptl_ib_peer_check_timeout(mca_ptl_base_peer_t* peer)
{
    return OMPI_SUCCESS;
}

static double mca_ptl_ib_get_us(void)
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return (double) t.tv_sec * (double) 1e6 + (double) t.tv_usec;
}

static int mca_ptl_ib_peer_start_connect(mca_ptl_base_peer_t* peer)
{
    VAPI_ret_t ret;
    VAPI_ud_av_t av;
    VAPI_ud_av_hndl_t av_hndl;
    mca_ptl_ib_module_t* peer_module;
    
    peer_module = peer->peer_module;

    peer_module->ud_send_buf = malloc(sizeof(mca_ptl_ib_ud_buf_t) *
            MAX_UD_PREPOST_DEPTH);

    mca_ptl_ib_prep_ud_bufs(peer_module->nic, peer_module->ud_send_buf, 
            IB_SEND, MAX_UD_PREPOST_DEPTH);

    peer_module->ud_send_buf[0].buf_data->qp_hndl = 101;

    av.dgid[0] = 0;
    av.dlid = peer->peer_proc->proc_addrs[0].lid;
    av.grh_flag = TRUE;
    av.flow_label = DEFAULT_FLOW_LABEL;
    av.hop_limit = DEFAULT_HOP_LIMIT;
    av.sgid_index = 0;
    av.sl = DEFAULT_SERVICE_LEVEL;
    av.port = DEFAULT_PORT;
    av.src_path_bits = DEFAULT_SRC_PATH_BITS;
    av.static_rate = DEFAULT_STATIC_RATE;
    av.traffic_class = DEFAULT_TRAFFIC_CLASS;

    ret = VAPI_create_addr_hndl(peer_module->nic, peer_module->ptag,
            &av, &av_hndl);

    if (VAPI_OK != ret) {
        return OMPI_ERROR;
    }

    peer_module->ud_send_buf[0].desc.sr.remote_qkey = 0;
    peer_module->ud_send_buf[0].desc.sr.remote_qp = peer->peer_proc->proc_addrs[0].ud_qp;
    peer_module->ud_send_buf[0].desc.sr.remote_ah = av_hndl;
    peer_module->ud_send_buf[0].desc.sr.id = (VAPI_virt_addr_t) (MT_virt_addr_t)&peer_module->ud_send_buf[0];
    peer_module->ud_send_buf[0].desc.sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t)
        peer_module->ud_send_buf[0].buf_data;
    peer_module->ud_send_buf[0].desc.sg_entry.len = 4;
    peer_module->ud_send_buf[0].desc.sg_entry.lkey = peer_module->ud_send_buf[0].memhandle.lkey;
    peer_module->ud_send_buf[0].desc.sr.set_se = FALSE;
    peer_module->ud_send_buf[0].desc.sr.fence = FALSE;

    ret = VAPI_post_sr(peer_module->nic, 
            peer_module->ud_qp_hndl, 
            &(peer_module->ud_send_buf[0].desc.sr));

    D_PRINT("Remote QP: %d, Remote LID : %d, Posted sr: %s\n", 
            peer_module->ud_send_buf[0].desc.sr.remote_qp,
            av.dlid, VAPI_strerror(ret));

    if(VAPI_OK != ret) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/*
 * Attempt to send a fragment using a given peer. If the peer is not
 * connected, queue the fragment and start the connection as required.
 */

int mca_ptl_ib_peer_send(mca_ptl_base_peer_t* peer,
        mca_ptl_ib_send_frag_t* frag)
{
    int rc;

    OMPI_THREAD_LOCK(&peer->peer_send_lock);

    switch(peer->peer_state) {
        case MCA_PTL_IB_CONNECTING:

            /* Well, connecting means that I've already sent my UD
             * QP across, but I haven't got any reply, so, I have
             * to check for timeout */
            ompi_list_append(&peer->peer_frags, (ompi_list_item_t*)frag);

            rc = mca_ptl_ib_peer_check_timeout(peer);

            break;
            
        case MCA_PTL_IB_CLOSED:

            ompi_list_append(&peer->peer_frags, (ompi_list_item_t*)frag);

            rc = mca_ptl_ib_peer_start_connect(peer);

            break;

        case MCA_PTL_IB_FAILED:

            rc = OMPI_ERR_UNREACH;
            break;

        case MCA_PTL_IB_CONNECTED:
            /* Fill in this later for the send to work
             *
            if (NULL != ptl_peer->peer_send_frag) {
                ompi_list_append(&ptl_peer->peer_frags, (ompi_list_item_t*)frag);
            } else {
                if(mca_ptl_ib_send_frag_handler(frag, ptl_peer->peer_sd)) {
                    OMPI_THREAD_UNLOCK(&ptl_peer->peer_send_lock);
                    mca_ptl_ib_send_frag_progress(frag);
                    return rc;
                } else {
                    ptl_peer->peer_send_frag = frag;
                    ompi_event_add(&ptl_peer->peer_send_event, 0);
                }
            }
            */
            break;
        default:
            rc = OMPI_ERR_UNREACH;
    }
    OMPI_THREAD_UNLOCK(&peer->peer_send_lock);

    return rc;
}
