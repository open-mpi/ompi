/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
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
#include "oob_ud_req.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

static void mca_oob_ud_req_constuct (mca_oob_ud_req_t *req);
static void mca_oob_ud_req_destruct (mca_oob_ud_req_t *req);

OBJ_CLASS_INSTANCE(mca_oob_ud_req_t, opal_list_item_t, mca_oob_ud_req_constuct,
                   mca_oob_ud_req_destruct);

static void mca_oob_ud_msg_destruct (mca_oob_ud_msg_t *msg);
static void mca_oob_ud_msg_construct (mca_oob_ud_msg_t *msg);

OBJ_CLASS_INSTANCE(mca_oob_ud_msg_t, opal_free_list_item_t,
                   mca_oob_ud_msg_construct,
                   mca_oob_ud_msg_destruct);

static void mca_oob_ud_req_constuct (mca_oob_ud_req_t *req)
{
    memset ((char *)req + sizeof (req->super), 0, sizeof (*req) - sizeof (req->super));
}

static void mca_oob_ud_req_destruct (mca_oob_ud_req_t *req)
{
    int i;

    if (req->req_peer) {
        OBJ_RELEASE(req->req_peer);
    }

    if (req->req_wr.send) {
        free (req->req_wr.send);
    }

    if (req->req_grh_mr) {
        (void) ibv_dereg_mr (req->req_grh_mr);
    }

    if (req->req_grh) {
        free (req->req_grh);
    }

    if (req->req_sge) {
        free (req->req_sge);
    }

    MCA_OOB_UD_REQ_DEREG_MR(req);
}

void mca_oob_ud_req_timer_set (mca_oob_ud_req_t *req, const struct timeval *timeout,
                               int max_tries, void (*cb)(evutil_socket_t, short, void *))
{
    opal_event_evtimer_set (orte_event_base, &req->timer.event, cb, (void *) req);
    req->timer.value.tv_sec  = timeout->tv_sec;
    req->timer.value.tv_usec = timeout->tv_usec;
    opal_event_evtimer_add (&req->timer.event, &req->timer.value);
}

int mca_oob_ud_msg_get (struct mca_oob_ud_port_t *port, mca_oob_ud_req_t *req,
                        mca_oob_ud_qp_t *qp, mca_oob_ud_peer_t *peer, bool persist,
                        mca_oob_ud_msg_t **msgp)
{
    opal_free_list_item_t *item;
    opal_free_list_t *list = &port->free_msgs;

    item = opal_free_list_wait_st (list);
    if (NULL == item) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                            "%s oob:ud:msg_get error getting message buffer",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return ORTE_ERROR;
    }

    *msgp = (mca_oob_ud_msg_t *) item;

    (*msgp)->persist = persist;
    (*msgp)->req     = req;
    (*msgp)->peer    = peer;
    (*msgp)->qp      = qp;

    if (NULL != peer) {
        OBJ_RETAIN(peer);
    }

    memset ((*msgp)->hdr, 0, sizeof (*((*msgp)->hdr)));

    mca_oob_ud_fill_sge (&(*msgp)->sge, (*msgp)->hdr, port->mtu, (*msgp)->mr->lkey);
    mca_oob_ud_fill_send_wr (&(*msgp)->wr, &(*msgp)->sge, 1, peer);

    /* set return address */
    (*msgp)->hdr->ra.name     = *ORTE_PROC_MY_NAME;
    (*msgp)->hdr->ra.qkey     = 0;
    (*msgp)->hdr->ra.port_num = port->port_num;

    return ORTE_SUCCESS;
}

int mca_oob_ud_msg_init (opal_free_list_item_t *item, void *context) {
    mca_oob_ud_port_t *port = (mca_oob_ud_port_t *) context;
    int buffer_id = port->send_buffer_index++ + mca_oob_ud_component.ud_recv_buffer_count;
    char *buf = port->msg_buf.ptr + buffer_id * port->mtu;
    mca_oob_ud_msg_t *msg = (mca_oob_ud_msg_t *) item;

    msg->port = port;
    msg->hdr  = (mca_oob_ud_msg_hdr_t *) buf;
    msg->mr   = port->msg_buf.mr;

    return ORTE_SUCCESS;
}

void mca_oob_ud_msg_return (mca_oob_ud_msg_t *msg)
{
    opal_free_list_t *list = &msg->port->free_msgs;

    if (NULL != msg->peer) {
        mca_oob_ud_peer_release (msg->peer);
    }

    msg->peer   = NULL;
    msg->cbfunc = NULL;
    msg->qp     = NULL;
    msg->req    = NULL;

    opal_free_list_return_st (list, &msg->super);
}

static void mca_oob_ud_msg_construct (mca_oob_ud_msg_t *msg)
{
    memset ((char *)msg + sizeof (msg->super), 0, sizeof (*msg) - sizeof (msg->super));

    OBJ_CONSTRUCT(&msg->status_changed, opal_condition_t);
    OBJ_CONSTRUCT(&msg->lock, opal_mutex_t);
}

static void mca_oob_ud_msg_destruct (mca_oob_ud_msg_t *msg)
{
    OBJ_DESTRUCT(&msg->status_changed);
    OBJ_DESTRUCT(&msg->lock);

    if (NULL != msg->peer) {
        mca_oob_ud_peer_release (msg->peer);
    }
}

int mca_oob_ud_msg_post_send (mca_oob_ud_msg_t *msg)
{
    int rc = ORTE_SUCCESS;

    msg->status = MCA_OOB_UD_MSG_STATUS_POSTED;

    OPAL_THREAD_LOCK(&msg->peer->peer_lock);

    if (MCA_OOB_UD_MSG_ACK == msg->hdr->msg_type ||
        MCA_OOB_UD_MSG_NACK == msg->hdr->msg_type) {
        rc = mca_oob_ud_qp_post_send (msg->qp, &msg->wr, 1);
    } else {
        rc = mca_oob_ud_peer_post_msg (msg->peer, msg);
    }

    if (ORTE_SUCCESS != rc && false == msg->persist) {
        msg->status = MCA_OOB_UD_MSG_STATUS_ERROR;
        mca_oob_ud_msg_return (msg);
    }

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:msg_post_send posted send for msg %p with id %" PRIu64,
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) msg, msg->hdr->msg_id);

    OPAL_THREAD_UNLOCK(&msg->peer->peer_lock);

    return rc;
}

int mca_oob_ud_msg_status_update (mca_oob_ud_msg_t *msg, mca_oob_ud_status_t status)
{
    int rc;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:msg_status_update setting status of msg %p to %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) msg, (int) status);

    OPAL_THREAD_LOCK(&msg->lock);

    if (status != msg->status) {
        if (MCA_OOB_UD_MSG_STATUS_COMPLETE == status) {
            opal_output_verbose(10, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:msg_status_update setting peer %s as available",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&msg->peer->peer_name));

            msg->peer->peer_available = true;
        }

        switch (status) {
        case MCA_OOB_UD_MSG_STATUS_TIMEOUT:
            rc = ORTE_ERR_TIMEOUT;
            break;
        case MCA_OOB_UD_MSG_STATUS_COMPLETE:
            rc = ORTE_SUCCESS;
            break;
        case MCA_OOB_UD_MSG_STATUS_ERROR:
        default:
            rc = ORTE_ERROR;
        }

        if (msg->cbfunc) {
            msg->cbfunc (msg, rc);
        }

        /* signal status change */
        msg->status = status;
        opal_condition_signal (&msg->status_changed);

        OPAL_THREAD_UNLOCK(&msg->lock);

        if (false == msg->persist) {
            mca_oob_ud_msg_return (msg);
        }

        return ORTE_SUCCESS;
    }

    OPAL_THREAD_UNLOCK(&msg->lock);

    return ORTE_SUCCESS;
}

static void mca_oob_ud_req_return (mca_oob_ud_req_t *req)
{
    opal_output_verbose(15, orte_oob_base_framework.framework_output,
                         "%s oob:ud:req_return returning req %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) req);

    mca_oob_ud_req_append_to_list (req, NULL);

    if (NULL != req->req_peer) {
        mca_oob_ud_peer_release (req->req_peer);
        req->req_peer = NULL;
    }

    if (NULL != req->req_wr.send) {
        free (req->req_wr.send);
        req->req_wr.send = NULL;
    }

    if (NULL != req->req_sge) {
        free (req->req_sge);
        req->req_sge = NULL;
    }

    OBJ_RELEASE(req);
}

void mca_oob_ud_req_complete (mca_oob_ud_req_t *req, int rc)
{
    int i;
    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:req_complete %s request %p completed with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (req->type == MCA_OOB_UD_REQ_SEND) ? "SEND":"RECV", (void *) req, rc);

    if (NULL != req->req_qp) {
        (void) mca_oob_ud_qp_data_release (req->req_qp);
        req->req_qp = NULL;
    }

    /* deregister memory *before* handing it to the callback */
    MCA_OOB_UD_REQ_DEREG_MR(req);

    switch (req->type) {
    case MCA_OOB_UD_REQ_SEND:
        if (req->req_data_type != MCA_OOB_UD_REQ_TR) {
            req->rml_msg->status = rc;
            ORTE_RML_SEND_COMPLETE(req->rml_msg);
        }
        break;
    case MCA_OOB_UD_REQ_RECV:
        if ((req->req_target.jobid == ORTE_PROC_MY_NAME->jobid) &&
            (req->req_target.vpid == ORTE_PROC_MY_NAME->vpid)) {
            opal_output_verbose(1, orte_oob_base_framework.framework_output,
                "%s DELIVERING TO RML",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            if (MCA_OOB_UD_REQ_IOV == req->req_data_type) {
                char *data = (char *)calloc(req->req_data.iov.count, sizeof(struct iovec));
                int datalen = 0;
                for (i = 0 ; i < req->req_data.iov.count; ++i) {
                    memcpy (&data[datalen], req->req_data.iov.uiov[i].iov_base, req->req_data.iov.uiov[i].iov_len);
                    datalen += req->req_data.iov.uiov[i].iov_len;
                }
                ORTE_RML_POST_MESSAGE(&req->req_origin, req->req_tag, data, datalen);
                free(data);
            } else {
                ORTE_RML_POST_MESSAGE(&req->req_origin, req->req_tag,
                                       req->req_data.buf.p, req->req_data.buf.size);
            }
        } else {
            opal_output_verbose(1, orte_oob_base_framework.framework_output,
                                "%s UD PROMOTING ROUTED MESSAGE FOR %s TO OOB",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&req->req_target));

            orte_rml_send_t *snd = OBJ_NEW(orte_rml_send_t);
            snd->dst = req->req_target;
            snd->origin =  req->req_origin;
            snd->tag = req->req_tag;

            if (MCA_OOB_UD_REQ_IOV == req->req_data_type) {
                char *data = (char *)calloc(req->req_data.iov.count, sizeof(struct iovec));
                int datalen = 0;
                for (i = 0 ; i < req->req_data.iov.count; ++i) {
                    memcpy (&data[datalen], req->req_data.iov.uiov[i].iov_base, req->req_data.iov.uiov[i].iov_len);
                    datalen += req->req_data.iov.uiov[i].iov_len;
                }
                snd->data = data;
                snd->count = datalen;
            } else {
                char *data = (char *)calloc(req->req_data.buf.size, sizeof(char));
                memcpy (data, req->req_data.buf.p, req->req_data.buf.size);
                snd->data = data;
                snd->count = req->req_data.buf.size;
            }
            snd->cbfunc.iov = NULL;
            snd->cbdata = NULL;
            /* activate the OOB send state */
            ORTE_OOB_SEND(snd);
        }
        break;
    default:
        break;
    }

    mca_oob_ud_req_return (req);
}

void mca_oob_ud_req_append_to_list (mca_oob_ud_req_t *req, opal_list_t *list)
{
    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);

    if (NULL != req->req_list) {
        opal_list_remove_item (req->req_list, (opal_list_item_t *) req);
    }

    if (NULL != list) {
        opal_list_append (list, (opal_list_item_t *) req);
    }

    req->req_list = list;

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);
}

bool mca_oob_ud_req_is_in_list (mca_oob_ud_req_t *req, opal_list_t *list)
{
    opal_list_item_t *item;
    bool rc = false;

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);

    for (item = opal_list_get_first (list) ;
         item != opal_list_get_end (list) ;
         item = opal_list_get_next (item)) {
        if (item == (opal_list_item_t *) req) {
            rc = true;
            break;
        }
    }

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

    return rc;
}

void mca_oob_ud_req_abort (mca_oob_ud_req_t *req)
{
    /* caller should have removed this request from any owner list */
    req->req_list = NULL;

    if (NULL != req->req_qp) {
        mca_oob_ud_qp_data_release (req->req_qp);
        req->req_qp = NULL;
    }

    /* free up request resources */
    mca_oob_ud_req_complete (req, ORTE_ERR_INTERUPTED);
}

int mca_oob_ud_msg_wait (mca_oob_ud_msg_t *msg)
{
    OPAL_THREAD_LOCK(&msg->lock);
    /* wait for ack */
    while (MCA_OOB_UD_MSG_STATUS_POSTED == msg->status) {
        opal_condition_wait (&msg->status_changed, &msg->lock);
    }
    OPAL_THREAD_UNLOCK(&msg->lock);

    switch (msg->status) {
    case MCA_OOB_UD_MSG_STATUS_TIMEOUT:
        return ORTE_ERR_TIMEOUT;
    case MCA_OOB_UD_MSG_STATUS_COMPLETE:
        return ORTE_SUCCESS;
    case MCA_OOB_UD_MSG_STATUS_ERROR:
    default:
        return ORTE_ERROR;
    }
}
