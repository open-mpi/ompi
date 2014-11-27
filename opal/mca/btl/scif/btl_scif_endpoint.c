/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_scif.h"

#include "btl_scif_endpoint.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/util/sys_limits.h"

static void mca_btl_scif_ep_construct (mca_btl_base_endpoint_t *ep) {
    memset ((char *) ep + sizeof(ep->super), 0, sizeof (*ep) - sizeof (ep->super));
    OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ep->frag_wait_list, opal_list_t);
}

static void mca_btl_scif_ep_destruct (mca_btl_base_endpoint_t *ep) {
    if (ep->send_buffer.buffer) {
        scif_munmap (ep->send_buffer.buffer, mca_btl_scif_component.segment_size);
    }

    if (ep->recv_buffer.buffer) {
        scif_unregister (ep->scif_epd, ep->recv_buffer.scif_offset, mca_btl_scif_component.segment_size);
        free (ep->recv_buffer.buffer);
    }

    if (ep->scif_epd) {
        scif_close (ep->scif_epd);
    }

    OBJ_DESTRUCT(&ep->lock);
    OBJ_DESTRUCT(&ep->frag_wait_list);
}

OBJ_CLASS_INSTANCE(mca_btl_scif_endpoint_t, opal_list_item_t,
                   mca_btl_scif_ep_construct, mca_btl_scif_ep_destruct);

static void mca_btl_scif_ep_free_buffer (mca_btl_base_endpoint_t *ep) {
    if (ep->recv_buffer.buffer) {
        scif_unregister (ep->scif_epd, ep->recv_buffer.scif_offset, mca_btl_scif_component.segment_size);
        free (ep->recv_buffer.buffer);
        ep->recv_buffer.buffer = NULL;
        ep->recv_buffer.scif_offset = (off_t) -1;
    }
}

static inline int mca_btl_scif_ep_get_buffer (mca_btl_base_endpoint_t *ep) {
    int rc;

    rc = posix_memalign ((void **) &ep->recv_buffer.buffer, opal_getpagesize(), mca_btl_scif_component.segment_size);
    if (0 > rc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    memset (ep->recv_buffer.buffer, 0, mca_btl_scif_component.segment_size);

    ep->recv_buffer.scif_offset = scif_register (ep->scif_epd, ep->recv_buffer.buffer,
                                                 mca_btl_scif_component.segment_size, 0,
                                                 SCIF_PROT_READ | SCIF_PROT_WRITE, 0);
    if (SCIF_REGISTER_FAILED == ep->recv_buffer.scif_offset) {
        BTL_VERBOSE(("failed to register a scif buffer of size %d. errno = %d",
                     mca_btl_scif_component.segment_size, errno));
        free (ep->recv_buffer.buffer);
        ep->recv_buffer.buffer = NULL;
        return OPAL_ERROR;
    }

    ep->recv_buffer.startp = (uint32_t *) ep->recv_buffer.buffer;
    ep->recv_buffer.endp   = ep->recv_buffer.startp + 1;

    ep->recv_buffer.startp[0] = ep->recv_buffer.endp[0] = 64;

    BTL_VERBOSE(("allocated buffer of size %d bytes. with scif registration %lu",
                 mca_btl_scif_component.segment_size, (unsigned long) ep->recv_buffer.scif_offset));

    return OPAL_SUCCESS;
}

/* must be called with the endpoint lock held */
static int mca_btl_scif_ep_connect_finish (mca_btl_base_endpoint_t *ep, bool passive) {
    int rc;

    rc = mca_btl_scif_ep_get_buffer (ep);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_VERBOSE(("error allocating buffer for scif peer"));
        return rc;
    }

    if (passive) {
        rc = scif_recv (ep->scif_epd, &ep->send_buffer.scif_offset,
                        sizeof (ep->send_buffer.scif_offset), SCIF_RECV_BLOCK);
        if (OPAL_LIKELY(-1 != rc)) {
            rc = scif_send (ep->scif_epd, &ep->recv_buffer.scif_offset,
                            sizeof (ep->recv_buffer.scif_offset), SCIF_SEND_BLOCK);
        }
    } else {
        rc = scif_send (ep->scif_epd, &ep->recv_buffer.scif_offset,
                        sizeof (ep->recv_buffer.scif_offset), SCIF_SEND_BLOCK);
        if (OPAL_LIKELY(-1 != rc)) {
            rc = scif_recv (ep->scif_epd, &ep->send_buffer.scif_offset,
                            sizeof (ep->send_buffer.scif_offset), SCIF_RECV_BLOCK);
        }
    }

    if (OPAL_UNLIKELY(-1 == rc)) {
        BTL_VERBOSE(("error exchanging connection data with peer %d", ep->peer_proc->proc_name.vpid));
        mca_btl_scif_ep_free_buffer (ep);
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("remote peer %d has scif offset %lu", ep->peer_proc->proc_name.vpid,
                 (unsigned long) ep->send_buffer.scif_offset));

    ep->send_buffer.buffer = scif_mmap (0, mca_btl_scif_component.segment_size,
                                        SCIF_PROT_READ | SCIF_PROT_WRITE,
                                        0, ep->scif_epd, ep->send_buffer.scif_offset);
    if (OPAL_UNLIKELY(NULL == ep->send_buffer.buffer)) {
        BTL_VERBOSE(("error in scif_mmap"));
        mca_btl_scif_ep_free_buffer (ep);
        return OPAL_ERROR;
    }

    opal_memchecker_base_mem_defined (ep->send_buffer.buffer, mca_btl_scif_component.segment_size);

    BTL_VERBOSE(("remote peer %d buffer mapped to local pointer %p", ep->peer_proc->proc_name.vpid,
                 ep->send_buffer.buffer));

    /* setup the circular send buffers */
    ep->send_buffer.start = ep->send_buffer.end = 64;

    ep->send_buffer.startp = (uint32_t *) ep->send_buffer.buffer;
    ep->send_buffer.endp   = ep->send_buffer.startp + 1;

    ep->recv_buffer.start = 64;

    /* connection complete */
    ep->state = MCA_BTL_SCIF_EP_STATE_CONNECTED;

    BTL_VERBOSE(("btl/scif connection to remote peer %d established", ep->peer_proc->proc_name.vpid));

    return OPAL_SUCCESS;
}

int mca_btl_scif_ep_connect_start_passive (void) {
    mca_btl_base_endpoint_t *ep = NULL;
    opal_process_name_t remote_name;
    struct scif_portID port_id;
    unsigned int i;
    scif_epd_t epd;
    int rc;

    /* accept the connection request. if the endpoint is already connecting we
     * may close this endpoint and alloc mca_btl_scif_ep_connect_start_active
     * to finish the connection. */
    rc = scif_accept (mca_btl_scif_module.scif_fd, &port_id, &epd, SCIF_ACCEPT_SYNC);
    if (OPAL_UNLIKELY(0 > rc)) {
        BTL_VERBOSE(("error accepting connecton from scif peer. %d", errno));
        return OPAL_ERROR;
    }

    /* determine which peer sent the connection request */
    rc = scif_recv (epd, &remote_name, sizeof (remote_name), SCIF_RECV_BLOCK);
    if (OPAL_UNLIKELY(-1 == rc)) {
        BTL_VERBOSE(("error in scif_recv"));
        scif_close (epd);
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("got connection request from vpid %d on port %u on node %u",
                 remote_name.vpid, port_id.port, port_id.node));

    for (i = 0 ; i < mca_btl_scif_module.endpoint_count ; ++i) {
        if (mca_btl_scif_module.endpoints[i].peer_proc->proc_name.vpid ==
            remote_name.vpid) {
            ep = mca_btl_scif_module.endpoints + i;
            break;
        }
    }

    /* peer not found */
    if (i == mca_btl_scif_module.endpoint_count) {
        BTL_VERBOSE(("remote peer %d unknown", remote_name.vpid));
        scif_close (epd);
        return OPAL_ERROR;
    }

    /* similtaneous connections (active side) */
    if ((MCA_BTL_SCIF_EP_STATE_CONNECTING == ep->state &&
         ep->port_id.port < mca_btl_scif_module.port_id.port) ||
        MCA_BTL_SCIF_EP_STATE_CONNECTED == ep->state) {
        BTL_VERBOSE(("active connection in progress. connection request from peer %d rejected", remote_name.vpid));
        scif_close (epd);
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&ep->lock);

    if (MCA_BTL_SCIF_EP_STATE_CONNECTED == ep->state) {
        opal_mutex_unlock (&ep->lock);
        scif_close (epd);
        return OPAL_SUCCESS;
    }

    BTL_VERBOSE(("accepted connection from port %d", ep->port_id.port));

    ep->state    = MCA_BTL_SCIF_EP_STATE_CONNECTING;
    ep->scif_epd = epd;

    rc = mca_btl_scif_ep_connect_finish (ep, true);
    if (OPAL_SUCCESS != rc) {
        scif_close (ep->scif_epd);
        ep->scif_epd = -1;
        ep->state = MCA_BTL_SCIF_EP_STATE_INIT;
    }

    opal_mutex_unlock (&ep->lock);

    return rc;
}

static inline int mca_btl_scif_ep_connect_start_active (mca_btl_base_endpoint_t *ep) {
    int rc = OPAL_SUCCESS;

    BTL_VERBOSE(("initiaiting connection to remote peer %d with port: %u on local scif node: %u",
                 ep->peer_proc->proc_name.vpid, ep->port_id.port, ep->port_id.node));

    opal_mutex_lock (&ep->lock);
    do {
        if (MCA_BTL_SCIF_EP_STATE_INIT != ep->state) {
            /* the accept thread has already finished this connection */
            rc = OPAL_SUCCESS;
            break;
        }

        ep->state = MCA_BTL_SCIF_EP_STATE_CONNECTING;

        ep->scif_epd = scif_open ();
        if (OPAL_UNLIKELY(SCIF_OPEN_FAILED == ep->scif_epd)) {
            BTL_VERBOSE(("error creating new scif endpoint"));
            rc = OPAL_ERROR;
            break;
        }

        rc = scif_connect (ep->scif_epd, &ep->port_id);
        if (OPAL_UNLIKELY(-1 == rc)) {
            /* the connection attempt failed. this could mean the peer is currently
             * processing connections. we will to try again later. */
            BTL_VERBOSE(("error connecting to scif peer. %d", errno));
            rc = OPAL_ERR_RESOURCE_BUSY;
            break;
        }

        rc = scif_send (ep->scif_epd, &OPAL_PROC_MY_NAME, sizeof (OPAL_PROC_MY_NAME), SCIF_SEND_BLOCK);
        if (OPAL_UNLIKELY(-1 == rc)) {
            BTL_VERBOSE(("error in scif_send"));
            rc = OPAL_ERROR;
            break;
        }

        /* build connection data */
        rc = mca_btl_scif_ep_connect_finish (ep, false);
    } while (0);

    if (OPAL_SUCCESS != rc) {
        scif_close (ep->scif_epd);
        ep->scif_epd = -1;
        ep->state = MCA_BTL_SCIF_EP_STATE_INIT;
    }

    opal_mutex_unlock (&ep->lock);

    return rc;
}

int mca_btl_scif_ep_connect (mca_btl_base_endpoint_t *ep) {
    int rc;

    if (OPAL_LIKELY(MCA_BTL_SCIF_EP_STATE_CONNECTED == ep->state)) {
        return OPAL_SUCCESS;
    } else if (MCA_BTL_SCIF_EP_STATE_CONNECTING == ep->state) {
        return OPAL_ERR_RESOURCE_BUSY;
    }

    if (MCA_BTL_SCIF_EP_STATE_INIT == ep->state) {
        rc = mca_btl_scif_ep_connect_start_active (ep);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    return OPAL_SUCCESS;
}
