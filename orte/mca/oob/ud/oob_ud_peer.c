/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "oob_ud_peer.h"
#include "oob_ud_component.h"

#include "opal/include/opal_stdint.h"

#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"
#include "orte/mca/routed/routed.h"

static void mca_oob_ud_peer_construct (mca_oob_ud_peer_t *peer);
static void mca_oob_ud_peer_destruct (mca_oob_ud_peer_t *peer);

OBJ_CLASS_INSTANCE(mca_oob_ud_peer_t, opal_object_t,
                   mca_oob_ud_peer_construct,
                   mca_oob_ud_peer_destruct);


int mca_oob_ud_peer_lookup (const orte_process_name_t *name, mca_oob_ud_peer_t **peer) {
    int rc;

    *peer = NULL;

    rc = opal_proc_table_get_value(&mca_oob_ud_module.peers,
                                   *name, (void**)peer);
    if (OPAL_SUCCESS != rc) {
        return ORTE_ERR_UNREACH;
    }

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_parse_uri (const char *uri, uint32_t *qp_num,
                                        uint16_t *lid, uint16_t *port_num)
{
    int rc;

    rc = sscanf (uri, "ud://%u.%hu.%hu", qp_num, lid, port_num);
    if (3 != rc) {
        opal_output (0, "%s oob:ud:parse_uri error parsing uri. expected 3 elements. got %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc);
        return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}

int mca_oob_ud_peer_update_with_uri (mca_oob_ud_peer_t *peer, const char *uri)
{
    opal_list_item_t *item;
    struct ibv_ah_attr ah_attr;
    mca_oob_ud_device_t *device;
    uint32_t qp_num;
    /* NTH: port is 16-bit here because C90 does not support hh in sscanf */
    uint16_t lid, port_num;
    int rc;

    rc = mca_oob_ud_parse_uri (uri, &qp_num, &lid, &port_num);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    if (peer->peer_lid != lid || peer->peer_port != port_num) {
        if (NULL != peer->peer_ah) {
            (void) ibv_destroy_ah (peer->peer_ah);
            peer->peer_ah = NULL;
        }
    }

    peer->peer_qpn  = qp_num;
    peer->peer_qkey = 0; /* NTH: todo -- add qkey support if needed */
    peer->peer_lid  = lid;
    peer->peer_port = port_num;

    if (NULL == peer->peer_ah) {
        memset (&ah_attr, 0, sizeof (ah_attr));
        ah_attr.dlid     = lid;
        ah_attr.port_num = port_num;

        for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices);
             item != opal_list_get_end (&mca_oob_ud_component.ud_devices);
             item = opal_list_get_next (item)) {
            device = (mca_oob_ud_device_t *)item;

            /* try to create an address handle using this device */
            peer->peer_ah = ibv_create_ah (device->ib_pd, &ah_attr);
            if (NULL != peer->peer_ah) {
                peer->peer_context = (void *) item;
                break;
            }
        }

        if (NULL == peer->peer_ah) {
            free (peer);
            return ORTE_ERROR;
        }
    }

    return ORTE_SUCCESS;
}

mca_oob_ud_peer_t *mca_oob_ud_get_peer (struct mca_oob_ud_port_t *port,
                                        orte_process_name_t *name,
                                        uint32_t qpn, uint32_t qkey,
                                        uint16_t lid, uint8_t port_num)
{
    struct ibv_ah_attr ah_attr;
    mca_oob_ud_peer_t *peer;
    int rc;

    rc = mca_oob_ud_peer_lookup (name, &peer);
    if (ORTE_SUCCESS == rc) {
        opal_output_verbose(20, orte_oob_base_framework.framework_output,
                             "%s oob:ud:peer_from_msg_hdr using cached peer",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

        return peer;
    }

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:peer_from_msg_hdr creating peer from return address",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    peer = OBJ_NEW(mca_oob_ud_peer_t);
    if (NULL == peer) {
        return NULL;
    }

    peer->peer_qpn  = qpn;
    peer->peer_qkey = qkey;
    peer->peer_name = *name;
    peer->peer_lid  = lid;
    peer->peer_port = port_num;

    memset (&ah_attr, 0, sizeof (ah_attr));
    ah_attr.dlid     = peer->peer_lid;
    ah_attr.port_num = peer->peer_port;

    peer->peer_ah = ibv_create_ah (port->device->ib_pd, &ah_attr);
    if (NULL == peer->peer_ah) {
        free (peer);
        return NULL;
    }

    peer->peer_context = port->device;

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);

    opal_proc_table_set_value(&mca_oob_ud_module.peers,
                              *name, (void *) peer);

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);

    return peer;
}

mca_oob_ud_peer_t *mca_oob_ud_peer_from_uri (const char *uri)
{
    mca_oob_ud_peer_t *peer;
    int rc;

    peer = OBJ_NEW(mca_oob_ud_peer_t);
    if (NULL == peer) {
        return NULL;
    }

    rc = mca_oob_ud_peer_update_with_uri (peer, uri);
    if (ORTE_SUCCESS != rc) {
        OBJ_RELEASE (peer);
        peer = NULL;
    }

    return peer;
}

static void mca_oob_ud_peer_construct (mca_oob_ud_peer_t *peer)
{
    memset ((char *) peer + sizeof (peer->super), 0, sizeof (*peer) - sizeof (peer->super));
    OBJ_CONSTRUCT(&peer->peer_flying_messages, opal_list_t);

    peer->peer_expected_id = 1;
}

void mca_oob_ud_peer_handle_end (mca_oob_ud_peer_t *peer)
{
    mca_oob_ud_port_t *port = NULL;
    mca_oob_ud_msg_t *msg = NULL;
    int rc;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:peer_handle_end telling peer %s i am going away",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer->peer_name));

    do {
        /* tell the peer that we are deleting them */
        if (NULL == peer || NULL == peer->peer_context || false == peer->peer_available ||
            false == peer->needs_notification) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:peer_handle_end don't need to tell %s i am going away",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&peer->peer_name));
            break;
        }

        port = (mca_oob_ud_port_t *) opal_list_get_first (&((mca_oob_ud_device_t *)peer->peer_context)->ports);
        if (NULL == port) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:peer_handle_end can't tell %s i am going away (no port)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&peer->peer_name));
            break;
        }

        rc = mca_oob_ud_msg_get (port, NULL, &port->listen_qp, peer, true, &msg);
        if (ORTE_SUCCESS != rc) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:peer_handle_end can't tell %s i am going away (no message buffer)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&peer->peer_name));
            break;
        }

        peer->peer_timer.tries = 2;
        peer->peer_timer.value.tv_usec = 500000;

        msg->hdr->msg_type = MCA_OOB_UD_MSG_END;

        rc = mca_oob_ud_qp_post_send (&port->listen_qp, &msg->wr, 1);
        if (ORTE_SUCCESS != rc) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:peer_handle_end can't tell %s i am going away (send failed)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&peer->peer_name));
            break;
        }
    } while (0);

    if (NULL != msg) {
        mca_oob_ud_msg_return (msg);
    }
}

void mca_oob_ud_peer_lost (mca_oob_ud_peer_t *peer)
{
    OPAL_THREAD_LOCK(&peer->peer_lock);

    if (true == peer->peer_available) {
        peer->peer_available = false;

        opal_output_verbose(10, orte_oob_base_framework.framework_output,
                             "%s oob:ud:peer_lost lost connectivity to peer %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&peer->peer_name));

        /* inform the ERRMGR framework that we have lost a connection so
         * it can decide if this is important, what to do about it, etc.
         */
        ORTE_ACTIVATE_PROC_STATE(&peer->peer_name, ORTE_PROC_STATE_COMM_FAILED);
    }

    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

void mca_oob_ud_peer_release (mca_oob_ud_peer_t *peer)
{
    OBJ_RELEASE(peer);
}

static void mca_oob_ud_peer_destruct (mca_oob_ud_peer_t *peer)
{

    if (NULL != peer->peer_ah) {
        (void) ibv_destroy_ah (peer->peer_ah);
    }
}

static void mca_oob_ud_peer_msg_timeout (int fd, short event, void *ctx)
{
    mca_oob_ud_peer_t *peer = (mca_oob_ud_peer_t *) ctx;
    mca_oob_ud_msg_t  *msg  = (mca_oob_ud_msg_t *) opal_list_get_first (&peer->peer_flying_messages);

    OPAL_THREAD_LOCK(&peer->peer_lock);

    if (false == peer->peer_timer.active) {
        return;
    }

    peer->peer_timer.active = false;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:peer_msg_timeout timeout sending to peer %s. first message = %" PRIu64 " which has length %d" ,
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer->peer_name), msg->hdr->msg_id, msg->wr.sg_list[0].length);

    if (peer->peer_timer.tries == 0) {
        opal_list_item_t *item;


        while (NULL != (item = opal_list_remove_first (&peer->peer_flying_messages))) {
            msg = (mca_oob_ud_msg_t *) item;

            mca_oob_ud_msg_status_update (msg, MCA_OOB_UD_MSG_STATUS_TIMEOUT);
            if (msg->req) {
                mca_oob_ud_req_complete (msg->req, ORTE_ERR_TIMEOUT);
            }
        }

        OPAL_THREAD_UNLOCK(&peer->peer_lock);
        mca_oob_ud_peer_lost (peer);
        return;
    }

    peer->peer_timer.tries--;
    mca_oob_ud_peer_post_all (peer);
    mca_oob_ud_peer_start_timer (peer);

    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

int mca_oob_ud_peer_post_msg (mca_oob_ud_peer_t *peer, mca_oob_ud_msg_t *msg)
{
    int rc;

    msg->hdr->msg_id   = ++peer->peer_next_id;

    rc = mca_oob_ud_qp_post_send (msg->qp, &msg->wr, 1);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    opal_list_append (&peer->peer_flying_messages, (opal_list_item_t *) msg);

    if (false == peer->peer_timer.active) {
        mca_oob_ud_peer_reset_timer (peer);
        mca_oob_ud_peer_start_timer (peer);
    }

    return ORTE_SUCCESS;
}

void mca_oob_ud_peer_stop_timer (mca_oob_ud_peer_t *peer)
{
    if (peer->peer_timer.active) {
        peer->peer_timer.active = false;
        opal_event_evtimer_del (&peer->peer_timer.event);
    }
}

void mca_oob_ud_peer_reset_timer (mca_oob_ud_peer_t *peer)
{
    peer->peer_timer.tries         = mca_oob_ud_component.ud_max_retries;

    peer->peer_timer.value.tv_sec = mca_oob_ud_component.ud_timeout_usec / 1000000;
    peer->peer_timer.value.tv_usec = mca_oob_ud_component.ud_timeout_usec % 1000000;
}

void mca_oob_ud_peer_start_timer (mca_oob_ud_peer_t *peer)
{
    if (!peer->peer_timer.active && opal_list_get_size (&peer->peer_flying_messages)) {
        peer->peer_timer.active = true;

        opal_event_evtimer_set (orte_event_base, &peer->peer_timer.event,
                                mca_oob_ud_peer_msg_timeout, (void *) peer);
        opal_event_evtimer_add (&peer->peer_timer.event, &peer->peer_timer.value);
    }
}

void mca_oob_ud_peer_post_all (mca_oob_ud_peer_t *peer)
{
    opal_list_item_t *item;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:peer_post_all reposting all messages for peer %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) peer);

    for (item = opal_list_get_first (&peer->peer_flying_messages) ;
         item != opal_list_get_end (&peer->peer_flying_messages) ;
         item = opal_list_get_next (item)) {
        mca_oob_ud_msg_t *msg = (mca_oob_ud_msg_t *) item;
        (void) mca_oob_ud_qp_post_send (msg->qp, &msg->wr, 1);
    }
}
