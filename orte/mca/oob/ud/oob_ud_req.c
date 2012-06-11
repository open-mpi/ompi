/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "oob_ud.h"

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

    if (req->req_mr) {
        for (i = 0 ; i < req->req_count ; ++i) {
            if (req->req_mr[i]) {
                (void) ibv_dereg_mr (req->req_mr[i]);
            }
        }
        /* these should have already been deregistered */
        free (req->req_mr);
    }
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
    int rc;

    OPAL_FREE_LIST_WAIT(list, item, rc);
    if (OPAL_SUCCESS != rc) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:msg_get error getting message "
                             "buffer. rc = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc));
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

int mca_oob_ud_msg_init (mca_oob_ud_msg_t *msg, struct mca_oob_ud_port_t *port,
                         char *buf, struct ibv_mr *mr)
{
    msg->port = port;
    msg->hdr  = (mca_oob_ud_msg_hdr_t *) buf;
    msg->mr   = mr;

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

    OPAL_FREE_LIST_RETURN(list, msg);
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

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:msg_post_send posted send for msg %p with id %" PRIu64,
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) msg, msg->hdr->msg_id));

    OPAL_THREAD_UNLOCK(&msg->peer->peer_lock);

    return rc;
}

int mca_oob_ud_msg_status_update (mca_oob_ud_msg_t *msg, mca_oob_ud_status_t status)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:msg_status_update setting status of msg %p "
                         "to %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) msg, (int) status));

    OPAL_THREAD_LOCK(&msg->lock);

    if (status != msg->status) {
        if (MCA_OOB_UD_MSG_STATUS_COMPLETE == status) {
            OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:msg_status_update setting peer %s as "
                                 "available", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&msg->peer->peer_name)));

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
    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:req_return returning req %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) req));

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

    if (ORTE_RML_PERSISTENT & req->req_flags) {
        if (ORTE_RML_ALLOC & req->req_flags) {
            int iov_index = req->req_count - 1;

            /* NTH: caller took possesion of the buffer */
            if (req->req_uiov[iov_index].iov_base) {
                req->req_uiov[iov_index].iov_base = NULL;
                req->req_uiov[iov_index].iov_len  = 0;
            }
        }

        mca_oob_ud_recv_match (req);
    } else {
        OBJ_RELEASE(req);
    }
}

void mca_oob_ud_req_complete (mca_oob_ud_req_t *req, int rc)
{
    int size, i;

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:req_complete request %p completed with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) req, rc));

    if (NULL != req->req_qp) {
        (void) mca_oob_ud_qp_data_release (req->req_qp);
        req->req_qp = NULL;
    }

    /* deregister memory *before* handing it to the callback */
    if (req->req_mr) {
        for (i = 0 ; i < req->req_count ; ++i) {
            if (req->req_mr[i]) {
                (void) ibv_dereg_mr (req->req_mr[i]);
                req->req_mr[i] = NULL;
            }
        }
    }

    if (req->req_cbfunc) {
        req->req_rc = rc;

        if ((req->req_flags & ORTE_RML_FLAG_RECURSIVE_CALLBACK) == 0) {
            OPAL_THREAD_LOCK (&mca_oob_ud_component.ud_lock);
            mca_oob_ud_req_append_to_list (req, &mca_oob_ud_component.ud_completed);
            size = opal_list_get_size (&mca_oob_ud_component.ud_completed);
            OPAL_THREAD_UNLOCK (&mca_oob_ud_component.ud_lock);
            if (size > 1) {
                return;
            }
        }

        req->req_cbfunc (req->req_rc, &req->req_target, req->req_uiov, req->req_count,
                         req->req_tag, req->req_cbdata);

        if ((req->req_flags & ORTE_RML_FLAG_RECURSIVE_CALLBACK) == 0) {
            opal_list_item_t* item;

            OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);
            mca_oob_ud_req_return (req);
            while(NULL != 
                  (item = opal_list_remove_first(&mca_oob_ud_component.ud_completed))) {
                req = (mca_oob_ud_req_t *) item;
                req->req_list = NULL;

                OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);
                req->req_cbfunc (req->req_rc, &req->req_target, req->req_uiov, req->req_count,
                                 req->req_tag, req->req_cbdata);
                OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);

                mca_oob_ud_req_return (req);
            }
            OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);
        } else {
            mca_oob_ud_req_return (req);
        }
    } else {
        mca_oob_ud_req_return (req);
    }
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

    /* don't call the callback */
    req->req_cbfunc = NULL;

    /* make sure the request is freed */
    req->req_flags = 0;

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
