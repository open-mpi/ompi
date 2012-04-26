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

#include "opal/mca/event/event.h"
#include "opal/opal_socket_errno.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "oob_ud.h"

int mca_oob_ud_ping(const orte_process_name_t* name, const char* uri,
                    const struct timeval *timeout) {
    mca_oob_ud_peer_t *peer;
    mca_oob_ud_port_t *port;
    mca_oob_ud_msg_t *msg = NULL;
    struct timeval half_timeout;
    int rc;

    opal_output (0, "attempting to ping %s with uri %s", ORTE_NAME_PRINT(name), uri);

    peer = mca_oob_ud_peer_from_uri (uri);
    if (NULL == peer) {
        return ORTE_ERROR;
    }

    half_timeout.tv_sec   = timeout->tv_sec / 2;
    half_timeout.tv_usec  = (timeout->tv_usec / 2 + (timeout->tv_sec % 2) * 500000) % 1000000;
    half_timeout.tv_sec  += (timeout->tv_usec / 2 + (timeout->tv_sec % 2) * 500000) / 1000000;

    /* NTH: TODO -- get a random port? */
    port = (mca_oob_ud_port_t *) opal_list_get_first (&((mca_oob_ud_device_t *)peer->peer_context)->ports);

    do {
        rc = mca_oob_ud_msg_get (port, NULL, &port->listen_qp, peer, true, &msg);
        if (ORTE_SUCCESS != rc) {
            break;
        }

        msg->hdr->msg_type = MCA_OOB_UD_MSG_PING;

        rc = mca_oob_ud_msg_post_send (msg);

        /* wait for ack */
        rc = mca_oob_ud_msg_wait (msg);

        opal_output (0, "ping result to %s -> %s: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                     ORTE_NAME_PRINT(name), rc);
    } while (0);

    if (NULL != msg) {
        mca_oob_ud_msg_return(msg);
    }

    mca_oob_ud_peer_release (peer);

    return rc;
}
