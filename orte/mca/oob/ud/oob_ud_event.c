/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "oob_ud_component.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

static int mca_oob_ud_event_send_ack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg);
static int mca_oob_ud_event_send_nack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg);

static int mca_oob_ud_event_handle_ack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer,
                                         mca_oob_ud_msg_hdr_t *msg_hdr);
static int mca_oob_ud_event_handle_nack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer,
                                         mca_oob_ud_msg_hdr_t *msg_hdr);

static int mca_oob_ud_event_handle_completion (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg);
static int mca_oob_ud_event_handle_data_ok (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg);
static int mca_oob_ud_event_handle_req (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr);
static int mca_oob_ud_event_handle_rep (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg);
static int mca_oob_ud_event_handle_end (mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr);

static void *mca_oob_ud_event_dispatch(int fd, int flags, void *context);
static void *mca_oob_ud_complete_dispatch(int fd, int flags, void *context);

static void mca_oob_ud_stop_events(mca_oob_ud_device_t *device);

static inline opal_list_item_t *mca_oob_ud_list_get_first (opal_list_t *list)
{
    return (opal_list_get_size (list) == 0) ? NULL : opal_list_get_first (list);
}

static inline opal_list_item_t *mca_oob_ud_list_get_next (opal_list_t *list, opal_list_item_t *item)
{
    opal_list_item_t *next = opal_list_get_next (item);

    return (opal_list_get_end(list) == next) ? NULL : next;
}

static bool event_started = false;
static bool event_completed_set = false;

void mca_oob_ud_event_start_monitor (mca_oob_ud_device_t *device)
{
    if (!event_started) {
#if !OPAL_ENABLE_PROGRESS_THREADS
        opal_progress_event_users_increment ();
#endif
        opal_event_set (orte_event_base, &device->event, device->ib_channel->fd,
                        OPAL_EV_READ, mca_oob_ud_event_dispatch, (void *) device);
        opal_event_add (&device->event, NULL);
        event_started = true;
    }
}

void mca_oob_ud_event_stop_monitor (mca_oob_ud_device_t *device)
{
    if (event_started) {
#if !OPAL_ENABLE_PROGRESS_THREADS
        opal_progress_event_users_decrement ();
#endif
        opal_event_del (&device->event);
        mca_oob_ud_stop_events (device);
        event_started = false;
    }
}

struct mca_oob_ud_msg_item_t {
    opal_list_item_t      super;

    mca_oob_ud_msg_hdr_t *hdr;
    mca_oob_ud_port_t    *port;
    mca_oob_ud_peer_t    *peer;
    int                   msg_num;
};
typedef struct mca_oob_ud_msg_item_t mca_oob_ud_msg_item_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_msg_item_t);

static void mca_oob_ud_msg_item_construct (mca_oob_ud_msg_item_t *item)
{
    memset ((char *) item + sizeof (item->super), 0, sizeof (*item) - sizeof (item->super));
}

static void mca_oob_ud_msg_item_destruct (mca_oob_ud_msg_item_t *item)
{
    if (item->hdr) {
        /* repost the receive request */
        mca_oob_ud_port_post_one_recv (item->port, item->msg_num);
    }
}

OBJ_CLASS_INSTANCE(mca_oob_ud_msg_item_t, opal_list_item_t,
                   mca_oob_ud_msg_item_construct,
                   mca_oob_ud_msg_item_destruct);

static int mca_oob_ud_msg_item_cmp (opal_list_item_t **a, opal_list_item_t **b)
{
    mca_oob_ud_msg_item_t *aitem = *((mca_oob_ud_msg_item_t **) a);
    mca_oob_ud_msg_item_t *bitem = *((mca_oob_ud_msg_item_t **) b);

    if (aitem->peer == bitem->peer) {
        return (aitem->hdr->msg_id > bitem->hdr->msg_id ? 1 : -1);
    } else {
        return (aitem->peer > bitem->peer) ? 1 : -1;
    }
}

static int mca_oob_ud_process_messages (struct ibv_cq *event_cq, mca_oob_ud_port_t *port)
{
    mca_oob_ud_msg_item_t *msg_item, *next_item;
    opal_list_t *processing_msgs = &mca_oob_ud_component.ud_event_processing_msgs;
    mca_oob_ud_peer_t *peer;
    mca_oob_ud_msg_hdr_t *msg_hdr;
    int msg_num, i, count;
    struct ibv_wc wc[40];
    bool peer_nacked;

    count = ibv_poll_cq (event_cq, 40, wc);
    if (count < 0)
        return count;

    /* acknowlege the events */
    ibv_ack_cq_events (event_cq, count);

    for (i = 0 ; i < count ; ++i) {
        msg_num = (int)(wc[i].wr_id & (~MCA_OOB_UD_RECV_WR));
        msg_hdr = (mca_oob_ud_msg_hdr_t *) (port->msg_buf.ptr + msg_num * port->mtu);

        VALGRIND_MAKE_MEM_DEFINED(msg_hdr, wc[i].byte_len);

        if (!(wc[i].wr_id & MCA_OOB_UD_RECV_WR) || IBV_WC_SUCCESS != wc[i].status) {
            mca_oob_ud_port_post_one_recv (port, msg_num);
            continue;
        }

        peer = mca_oob_ud_get_peer (port, &msg_hdr->ra.name, wc[i].src_qp, msg_hdr->ra.qkey,
                                    wc[i].slid, msg_hdr->ra.port_num);

        if (peer) {
            if (MCA_OOB_UD_MSG_ACK != msg_hdr->msg_type && MCA_OOB_UD_MSG_NACK != msg_hdr->msg_type &&
                MCA_OOB_UD_MSG_END != msg_hdr->msg_type) {
                mca_oob_ud_msg_item_t *msg_item = OBJ_NEW(mca_oob_ud_msg_item_t);

                msg_item->msg_num = msg_num;
                msg_item->hdr     = msg_hdr;
                msg_item->port    = port;
                msg_item->peer    = peer;

                opal_list_append (processing_msgs, (opal_list_item_t *) msg_item);
            } else {
                if (MCA_OOB_UD_MSG_ACK == msg_hdr->msg_type) {
                    (void) mca_oob_ud_event_handle_ack (port, peer, msg_hdr);
                } else if (MCA_OOB_UD_MSG_NACK == msg_hdr->msg_type) {
                    (void) mca_oob_ud_event_handle_nack (port, peer, msg_hdr);
                } else {
                    mca_oob_ud_event_handle_end (peer, msg_hdr);
                }

                mca_oob_ud_port_post_one_recv (port, msg_num);
            }
        } else {
            opal_output_verbose(10, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:process_message got a null peer for message id %"
                                 PRIu64, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr->msg_id);
            mca_oob_ud_port_post_one_recv (port, msg_num);
        }
    }

    /* Sort messages by peer then id */
    opal_list_sort (processing_msgs, mca_oob_ud_msg_item_cmp);

    /* Send ACKs/NACKs and throw away out-of-order messages */
    msg_item = (mca_oob_ud_msg_item_t *) mca_oob_ud_list_get_first (processing_msgs);

    for (peer = NULL, peer_nacked = false ; NULL != msg_item ; msg_item = next_item) {
        if (peer != msg_item->peer) {
            peer_nacked = false;
        }

        peer = msg_item->peer;

        next_item = (mca_oob_ud_msg_item_t *) mca_oob_ud_list_get_next (processing_msgs,
                                                                        (opal_list_item_t *)msg_item);

        if (false == peer_nacked) {
            if (msg_item->hdr->msg_id > peer->peer_expected_id) {
                (void) mca_oob_ud_event_send_nack (msg_item->port, peer, msg_item->hdr);
                peer_nacked = true;
            } else if (NULL == next_item || (next_item->peer != msg_item->peer)) {
                (void) mca_oob_ud_event_send_ack (msg_item->port, msg_item->peer, msg_item->hdr);
            }
        }

        if (msg_item->hdr->msg_id != peer->peer_expected_id) {
            opal_list_remove_item (processing_msgs, (opal_list_item_t *) msg_item);
            OBJ_RELEASE(msg_item);
        } else {
            peer->peer_expected_id++;
        }
    }

    /* Process remaining messages */
    while (NULL !=
           (msg_item = (mca_oob_ud_msg_item_t *) opal_list_remove_first (processing_msgs))) {

        switch (msg_item->hdr->msg_type) {
        case MCA_OOB_UD_MSG_REQUEST:
            mca_oob_ud_event_handle_req (port, msg_item->peer, msg_item->hdr);
            break;
        case MCA_OOB_UD_MSG_REPLY:
            mca_oob_ud_event_handle_rep (port, msg_item->hdr);
            break;
        case MCA_OOB_UD_MSG_COMPLETE:
            mca_oob_ud_event_handle_completion (port, msg_item->hdr);
            break;
        case MCA_OOB_UD_MSG_DATA_OK:
            mca_oob_ud_event_handle_data_ok (port, msg_item->hdr);
            break;
        case MCA_OOB_UD_MSG_END:
            mca_oob_ud_event_handle_end (peer, msg_item->hdr);
            break;
        default:
            /* do nothing */
            break;
        }

        OBJ_RELEASE(msg_item);
    }

    return count;
}

static int mca_oob_ud_event_handle_ack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer,
                                        mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_msg_t *msg;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_handle_ack got ack for msg id %" PRIu64
                         " from peer %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr->msg_id,
                         ORTE_NAME_PRINT(&peer->peer_name));

    OPAL_THREAD_LOCK(&peer->peer_lock);

    mca_oob_ud_peer_stop_timer (peer);

    msg = (mca_oob_ud_msg_t *) mca_oob_ud_list_get_first (&peer->peer_flying_messages);

    while (NULL != (msg = (mca_oob_ud_msg_t *) mca_oob_ud_list_get_first (&peer->peer_flying_messages))) {
        if (msg->hdr->msg_id > msg_hdr->msg_id) {
            break;
        }

        msg = (mca_oob_ud_msg_t *)opal_list_remove_first (&peer->peer_flying_messages);
        (void) mca_oob_ud_msg_status_update (msg, MCA_OOB_UD_MSG_STATUS_COMPLETE);
    }

    mca_oob_ud_peer_start_timer (peer);

    OPAL_THREAD_UNLOCK(&peer->peer_lock);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_handle_nack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer,
                                         mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_msg_t *msg;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_handle_nack got nack for msg id %" PRIu64
                         " from peer %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr->msg_id,
                         ORTE_NAME_PRINT(&peer->peer_name));

    OPAL_THREAD_LOCK(&peer->peer_lock);

    mca_oob_ud_peer_stop_timer (peer);

    while (NULL !=
           (msg = (mca_oob_ud_msg_t *) mca_oob_ud_list_get_first (&peer->peer_flying_messages))) {
        if (msg->hdr->msg_id >= msg_hdr->msg_id) {
            break;
        }

        (void) opal_list_remove_first (&peer->peer_flying_messages);
        (void) mca_oob_ud_msg_status_update (msg, MCA_OOB_UD_MSG_STATUS_COMPLETE);
    }

    /* repost remaining messages */
    mca_oob_ud_peer_post_all (peer);

    /* reset and start the timer */
    mca_oob_ud_peer_reset_timer (peer);
    mca_oob_ud_peer_start_timer (peer);

    OPAL_THREAD_UNLOCK(&peer->peer_lock);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_handle_end (mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_handle_end got end message from peer %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&peer->peer_name));

    mca_oob_ud_peer_lost (peer);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_send_ack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_msg_hdr_t tmp_hdr;
    int rc = ORTE_SUCCESS;
    struct ibv_send_wr wr;
    struct ibv_sge sge;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_send_ack sending ack for message id %"
                         PRIu64 " peer = %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), msg_hdr->msg_id,
                         ORTE_NAME_PRINT(&peer->peer_name));

    /* reuse registered buffer to send ack (just need to change the type/return address) */
    memcpy (&tmp_hdr, msg_hdr, sizeof (tmp_hdr));

    msg_hdr->msg_type = MCA_OOB_UD_MSG_ACK;

    /* set return address */
    msg_hdr->ra.qkey     = 0;
    msg_hdr->ra.name     = *ORTE_PROC_MY_NAME;
    msg_hdr->ra.port_num = port->port_num;

    mca_oob_ud_fill_sge (&sge, msg_hdr, sizeof (*msg_hdr), port->msg_buf.mr->lkey);
    mca_oob_ud_fill_send_wr (&wr, &sge, 1, peer);

    rc = mca_oob_ud_qp_post_send (&port->listen_qp, &wr, 1);
    if (ORTE_SUCCESS != rc) {
        opal_output (0, "oob:ud:event_send_ack error posting ack!");
        return rc;
    }

    memcpy (msg_hdr, &tmp_hdr, sizeof (tmp_hdr));

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_send_nack (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_msg_hdr_t tmp_hdr;
    int rc = ORTE_SUCCESS;
    struct ibv_send_wr wr;
    struct ibv_sge sge;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_send_nack sending nack for message id %"
                         PRIu64 " peer = %s. msg_id = %" PRIu64, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         peer->peer_expected_id, ORTE_NAME_PRINT(&peer->peer_name), msg_hdr->msg_id);

    /* reuse registered buffer to send the nack (just need to change the type/return address) */
    memcpy (&tmp_hdr, msg_hdr, sizeof (tmp_hdr));

    msg_hdr->msg_type = MCA_OOB_UD_MSG_NACK;

    /* set return address */
    msg_hdr->ra.qkey     = 0;
    msg_hdr->ra.name     = *ORTE_PROC_MY_NAME;
    msg_hdr->ra.port_num = port->port_num;

    msg_hdr->msg_id = peer->peer_expected_id;

    mca_oob_ud_fill_sge (&sge, msg_hdr, sizeof (*msg_hdr), port->msg_buf.mr->lkey);
    mca_oob_ud_fill_send_wr (&wr, &sge, 1, peer);

    rc = mca_oob_ud_qp_post_send (&port->listen_qp, &wr, 1);
    if (ORTE_SUCCESS != rc) {
        opal_output (0, "oob:ud:event_send_ack error posting nack!");
        return rc;
    }

    memcpy (msg_hdr, &tmp_hdr, sizeof (tmp_hdr));

    return ORTE_SUCCESS;
}

void mca_oob_ud_event_queue_completed (mca_oob_ud_req_t *req)
{
    struct timeval now = {0, 0};

    mca_oob_ud_req_append_to_list (req, &mca_oob_ud_component.ud_event_queued_reqs);

    if (!(event_completed_set) ||
        !(opal_event_evtimer_pending (&mca_oob_ud_component.ud_complete_event, &now))) {
        event_completed_set = true;
        opal_event_evtimer_set (orte_event_base, &mca_oob_ud_component.ud_complete_event,
                                mca_oob_ud_complete_dispatch, NULL);
        opal_event_add (&mca_oob_ud_component.ud_complete_event, &now);
    }
}

static int mca_oob_ud_event_handle_completion (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_req_t *recv_req = msg_hdr->msg_lcl_ctx;
    bool brc;

    if (NULL == recv_req) {
        opal_output(0, "%s oob:ud:event_handle_completion msg_hdr->msg_lcl_ctx is NULL",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return ORTE_ERROR;
    }

    brc = mca_oob_ud_req_is_in_list (recv_req, &mca_oob_ud_component.ud_active_recvs);
    if (false == brc) {
        /* duplicate completion message? */
        opal_output_verbose(0, orte_oob_base_framework.framework_output,
                             "%s oob:ud:event_handle_completion apparent duplicate completion. "
                             "request %p. req list = %p", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) recv_req,
                             (void *) recv_req->req_list);
        return ORTE_SUCCESS;
    }

    recv_req->state = MCA_OOB_UD_REQ_COMPLETE;
    mca_oob_ud_event_queue_completed (recv_req);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_handle_data_ok (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_req_t *send_req = msg_hdr->msg_lcl_ctx;
    bool brc;

    if (NULL == send_req) {
        /* ack! */
        return ORTE_ERROR;
    }

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_handle_data_ok got data ok message for request %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) send_req);

    brc = mca_oob_ud_req_is_in_list (send_req, &mca_oob_ud_component.ud_active_sends);
    if (false == brc) {
        opal_output_verbose(0, orte_oob_base_framework.framework_output,
                             "%s oob:ud:event_handle_data_ok apparent duplicate data ok. "
                             "request %p. req list = %p", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) send_req,
                             (void *) send_req->req_list);
        /* duplicate data ok message? */
        return ORTE_SUCCESS;
    }

    send_req->state = MCA_OOB_UD_REQ_COMPLETE;
    mca_oob_ud_event_queue_completed (send_req);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_event_handle_req (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_req_t *recv_req;
    int rc;

    rc = mca_oob_ud_recv_match_send (port, peer, msg_hdr, &recv_req);
    if (ORTE_SUCCESS == rc) {
        mca_oob_ud_event_queue_completed (recv_req);
    }

    return rc;
}

static int mca_oob_ud_event_handle_rep (mca_oob_ud_port_t *port, mca_oob_ud_msg_hdr_t *msg_hdr)
{
    mca_oob_ud_req_t *send_req = (mca_oob_ud_req_t *) msg_hdr->msg_lcl_ctx;
    bool brc;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:event_handle_rep got reply for request %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) send_req);

    brc = mca_oob_ud_req_is_in_list (send_req, &mca_oob_ud_component.ud_active_sends);
    if (false == brc) {
        opal_output_verbose(0, orte_oob_base_framework.framework_output,
                             "%s oob:ud:event_handle_rep no send matches reply",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* duplicate reply message? */
        return ORTE_SUCCESS;
    }

    send_req->req_mtu          = min(send_req->req_mtu, msg_hdr->msg_data.rep.mtu);
    send_req->req_rem_data_len = msg_hdr->msg_data.rep.data_len;
    send_req->req_rem_ctx      = msg_hdr->msg_rem_ctx;
    send_req->req_rem_qpn      = msg_hdr->msg_data.rep.qpn;

    mca_oob_ud_event_queue_completed (send_req);

    return ORTE_SUCCESS;
}

static void *mca_oob_ud_event_dispatch(int fd, int flags, void *context)
{
    int rc;
    mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) context;
    mca_oob_ud_port_t *port = NULL;
    struct ibv_cq *event_cq = NULL;
    void *event_context     = NULL;

    do {
        rc = ibv_get_cq_event (device->ib_channel, &event_cq, &event_context);
    } while (rc && errno == EINTR);

    if (NULL == event_cq) {
        /* re-arm the event */
        opal_output (0, "%s oob:ud:event_dispatch re-arm the event",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_event_add (&port->device->event, NULL);

        return NULL;
    }

    port = (mca_oob_ud_port_t *) event_context;

    rc = mca_oob_ud_process_messages (event_cq, port);
    if (rc < 0) {
        opal_output (0, "%s oob:ud:event_dispatch error processing messages",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return NULL;
    }

    if (ibv_req_notify_cq(event_cq, 0)) {
        opal_output (0, "%s oob:ud:event_dispatch error asking for cq notifications",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }

    /* re-arm the event */
    opal_event_add (&port->device->event, NULL);

    return NULL;
}

static void *mca_oob_ud_complete_dispatch(int fd, int flags, void *context)
{
    mca_oob_ud_req_t *req;

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);
    while (NULL !=
           (req = (mca_oob_ud_req_t *) opal_list_remove_first (&mca_oob_ud_component.ud_event_queued_reqs))) {
        OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

        opal_output_verbose(10, orte_oob_base_framework.framework_output,
                             "%s oob:ud:event_process processing request %p",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) req);

        req->req_list = NULL;

        switch (req->type) {
        case MCA_OOB_UD_REQ_RECV:
            if (req->state == MCA_OOB_UD_REQ_COMPLETE) {
                mca_oob_ud_recv_complete (req);
            } else {
                mca_oob_ud_req_append_to_list (req, &mca_oob_ud_component.ud_active_recvs);
                mca_oob_ud_recv_try (req);
            }
            break;
        case MCA_OOB_UD_REQ_SEND:
            if (req->state == MCA_OOB_UD_REQ_COMPLETE) {
                mca_oob_ud_send_complete (req, ORTE_SUCCESS);
            } else {
                mca_oob_ud_req_append_to_list (req, &mca_oob_ud_component.ud_active_sends);
                mca_oob_ud_send_try (req);
            }
            break;
        default:
            break;
        }

        OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);
    }

    return NULL;
}

static void mca_oob_ud_stop_events (mca_oob_ud_device_t *device)
{
    opal_list_item_t *item;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:stop_events stopping event processing",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    for (item = opal_list_get_first (&device->ports) ;
         item != opal_list_get_end (&device->ports) ;
         item = opal_list_get_next (item)) {
        mca_oob_ud_port_t *port = (mca_oob_ud_port_t *) item;

        /* flush all receives */
        mca_oob_ud_qp_to_reset (&port->listen_qp);
    }

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:stop_events events stopped",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
}
