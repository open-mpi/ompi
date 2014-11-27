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

#include "opal/mca/event/event.h"
#include "opal/opal_socket_errno.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "oob_ud_ping.h"

int mca_oob_ud_process_ping(int fd, short args, void *cbdata)
{
    mca_oob_ud_ping_t *op = (mca_oob_ud_ping_t*)cbdata;

    orte_process_name_t* name = &op->peer;
    mca_oob_ud_peer_t *peer;
    mca_oob_ud_port_t *port;
    mca_oob_ud_msg_t *msg = NULL;
    int rc;

    opal_output_verbose (2, orte_oob_base_framework.framework_output,
                         "%s oob:ud:ping attempting to ping %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(name));

    rc = mca_oob_ud_peer_lookup(name, &peer);
    if (rc != ORTE_SUCCESS) {
        return rc;
    }

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

        opal_output_verbose (2, orte_oob_base_framework.framework_output,
                     "%s oob:ud:ping result to %s -> %s: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(name), rc);
    } while (0);

    if (NULL != msg) {
        mca_oob_ud_msg_return(msg);
    }

    mca_oob_ud_peer_release (peer);

    return rc;
}
