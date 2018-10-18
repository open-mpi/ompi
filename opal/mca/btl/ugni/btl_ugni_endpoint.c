/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011-2013 UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni_endpoint.h"
#include "btl_ugni_smsg.h"
#include "opal/mca/pmix/pmix.h"

static void mca_btl_ugni_ep_construct (mca_btl_base_endpoint_t *ep)
{
    memset ((char *) ep + sizeof(ep->super), 0, sizeof (*ep) - sizeof (ep->super));
    OBJ_CONSTRUCT(&ep->frag_wait_list, opal_list_t);
    OBJ_CONSTRUCT(&ep->lock, opal_recursive_mutex_t);
}

static void mca_btl_ugni_ep_destruct (mca_btl_base_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->frag_wait_list);
    OBJ_DESTRUCT(&ep->lock);
    free (ep->remote_attr);
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_endpoint_t, opal_list_item_t,
                   mca_btl_ugni_ep_construct, mca_btl_ugni_ep_destruct);

static int mca_btl_ugni_endpoint_get_modex (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_modex_t *modex;
    size_t msg_size;
    int rc;

    assert (NULL != ep && NULL != ep->peer_proc);

    /* Receive the modex */
    OPAL_MODEX_RECV(rc, &mca_btl_ugni_component.super.btl_version,
                    &ep->peer_proc->proc_name, (void **)&modex, &msg_size);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_ERROR(("error receiving modex"));
        return rc;
    }

    ep->ep_rem_addr = modex->addr;
    ep->ep_rem_id = modex->id;


    BTL_VERBOSE(("received modex for ep %p. addr: %d, id: %d",  (void*)ep, ep->ep_rem_addr, ep->ep_rem_id));

    free (modex);

    return OPAL_SUCCESS;
}

int mca_btl_ugni_init_ep (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_endpoint_t **ep,
                          mca_btl_ugni_module_t *btl, opal_proc_t *peer_proc)
{
    mca_btl_ugni_endpoint_t *endpoint;
    int rc;

    endpoint = OBJ_NEW(mca_btl_ugni_endpoint_t);
    assert (endpoint != NULL);

    endpoint->smsg_progressing = 0;
    endpoint->state = MCA_BTL_UGNI_EP_STATE_INIT;
    endpoint->peer_proc = peer_proc;

    /* get the modex info for this endpoint and setup a ugni endpoint. this call may lead
     * to re-entry through opal_progress(). */
    rc = mca_btl_ugni_endpoint_get_modex (endpoint);
    if (OPAL_SUCCESS != rc) {
        assert (0);
        return rc;
    }

    /* add this endpoint to the pointer array */
    endpoint->index = opal_pointer_array_add (&ugni_module->endpoints, endpoint);

    *ep = endpoint;

    return OPAL_SUCCESS;
}

void mca_btl_ugni_release_ep (mca_btl_ugni_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    int rc;

    opal_mutex_lock (&ep->lock);

    rc = mca_btl_ugni_ep_disconnect (ep, false);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        BTL_VERBOSE(("btl/ugni error disconnecting endpoint"));
    }

    /* TODO -- Clear space at the end of the endpoint array */
    opal_pointer_array_set_item (&ugni_module->endpoints, ep->index, NULL);

    opal_mutex_unlock (&ep->lock);

    OBJ_RELEASE(ep);
}

static inline int mca_btl_ugni_ep_smsg_get_mbox (mca_btl_base_endpoint_t *ep) {
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    opal_free_list_item_t *mbox;

    assert (NULL == ep->mailbox);

    mbox = opal_free_list_get (&ugni_module->smsg_mboxes);
    if (OPAL_UNLIKELY(NULL == mbox)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    ep->mailbox = (mca_btl_ugni_smsg_mbox_t *) mbox;
    ep->mailbox->attr.index = ep->index;

    /* per ugni spec we need to zero mailbox data before connecting */
    memset ((char *)ep->mailbox->attr.smsg_attr.msg_buffer + ep->mailbox->attr.smsg_attr.mbox_offset, 0,
            ep->mailbox->attr.smsg_attr.buff_size);
    return OPAL_SUCCESS;
}

static int mca_btl_ugni_ep_send_disconnect (mca_btl_base_endpoint_t *ep)
{
    int rc;

    do {
        rc = mca_btl_ugni_endpoint_smsg_send_wtag (ep, NULL, 0, NULL, 0, -1, MCA_BTL_UGNI_TAG_DISCONNECT);
        if (OPAL_LIKELY(GNI_RC_NOT_DONE != rc)) {
            break;
        }

        /* most likely got here because we are out of credits. check the remote CQ to get credit return */
        (void) mca_btl_ugni_progress_remote_smsg (mca_btl_ugni_ep_btl (ep));
    } while (1);

    return mca_btl_rc_ugni_to_opal (rc);
}

int mca_btl_ugni_ep_disconnect (mca_btl_base_endpoint_t *ep, bool send_disconnect)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    mca_btl_ugni_device_t *device;
    int rc;

    if (MCA_BTL_UGNI_EP_STATE_INIT == ep->state) {
        /* nothing to do */
        return OPAL_SUCCESS;
    }

    device = ep->smsg_ep_handle.device;

    while (device->dev_smsg_local_cq.active_operations) {
        /* ensure all sends are complete before removing and procs */
        rc = mca_btl_ugni_progress_local_smsg (ugni_module, device);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state && send_disconnect) {
        rc = mca_btl_ugni_ep_send_disconnect (ep);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_VERBOSE(("could not send disconnect message to peer"));
        }

        /* wait for the disconnect messagse to go */
        do {
            /* ensure all sends are complete before removing and procs */
            rc = mca_btl_ugni_progress_local_smsg (ugni_module, device);
            if (OPAL_SUCCESS != rc) {
                break;
            }
        } while (device->dev_smsg_local_cq.active_operations);

        (void) opal_atomic_add_fetch_32 (&ep->smsg_ep_handle.device->smsg_connections, -1);
    }

    mca_btl_ugni_device_lock (device);

    /* NTH: this call may not need the device lock. seems to work without it but
     * the lock is here to be safe. */
    (void) mca_btl_ugni_ep_handle_cleanup (&ep->smsg_ep_handle);

    mca_btl_ugni_device_unlock (device);

    if (ep->mailbox) {
        opal_free_list_return (&ugni_module->smsg_mboxes, ((opal_free_list_item_t *) ep->mailbox));
        ep->mailbox = NULL;
    }

    ep->state = MCA_BTL_UGNI_EP_STATE_INIT;

    return OPAL_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_start (mca_btl_base_endpoint_t *ep) {
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    mca_btl_ugni_device_t *device = ugni_module->devices;
    int rc;

    /* protect against re-entry from opal_progress */
    if (OPAL_UNLIKELY(MCA_BTL_UGNI_EP_STATE_CONNECTING == ep->state)) {
        return OPAL_ERR_RESOURCE_BUSY;
    }

    ep->state = MCA_BTL_UGNI_EP_STATE_CONNECTING;

    BTL_VERBOSE(("initiating connection to remote peer with address: %u id: %u proc: %p",
                 ep->ep_rem_addr, ep->ep_rem_id, (void *)ep->peer_proc));

    /* bind endpoint to remote address */
    /* we bind two endpoints to seperate out local smsg completion and local fma completion */
    mca_btl_ugni_device_lock (device);
    rc = mca_btl_ugni_ep_handle_init (ep, device->dev_smsg_local_cq.gni_handle, device, &ep->smsg_ep_handle);
    mca_btl_ugni_device_unlock (device);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    /* build connection data */
    rc = mca_btl_ugni_ep_smsg_get_mbox (ep);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    ep->remote_attr = calloc (1, sizeof (*ep->remote_attr));
    if (OPAL_UNLIKELY(NULL == ep->remote_attr)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    BTL_VERBOSE(("btl/ugni connection to remote peer initiated"));

    return OPAL_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_finish (mca_btl_base_endpoint_t *ep) {
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    gni_return_t grc;
    int rc;

    BTL_VERBOSE(("finishing connection. remote attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->remote_attr->smsg_attr.msg_type, ep->remote_attr->smsg_attr.msg_buffer,
                 ep->remote_attr->smsg_attr.buff_size, ep->remote_attr->smsg_attr.mem_hndl.qword1,
                 ep->remote_attr->smsg_attr.mem_hndl.qword2, ep->remote_attr->smsg_attr.mbox_offset,
                 ep->remote_attr->smsg_attr.mbox_maxcredit, ep->remote_attr->smsg_attr.msg_maxsize));

    BTL_VERBOSE(("finishing connection. local attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->mailbox->attr.smsg_attr.msg_type, ep->mailbox->attr.smsg_attr.msg_buffer,
                 ep->mailbox->attr.smsg_attr.buff_size, ep->mailbox->attr.smsg_attr.mem_hndl.qword1,
                 ep->mailbox->attr.smsg_attr.mem_hndl.qword2, ep->mailbox->attr.smsg_attr.mbox_offset,
                 ep->mailbox->attr.smsg_attr.mbox_maxcredit, ep->mailbox->attr.smsg_attr.msg_maxsize));

    grc = GNI_SmsgInit (ep->smsg_ep_handle.gni_handle, &ep->mailbox->attr.smsg_attr,
                        &ep->remote_attr->smsg_attr);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        BTL_ERROR(("error initializing SMSG protocol. rc = %d", grc));

        return mca_btl_rc_ugni_to_opal (grc);
    }

    /* set the local event data to the local index and the remote event data to my
     * index on the remote peer. This makes lookup of endpoints on completion take
     * a single lookup in the endpoints array. we will not be able to change the
     * remote peer's index in the endpoint's array after this point. */
    GNI_EpSetEventData (ep->smsg_ep_handle.gni_handle, ep->index, ep->remote_attr->index);

    ep->rmt_irq_mem_hndl = ep->remote_attr->rmt_irq_mem_hndl;
    ep->state = MCA_BTL_UGNI_EP_STATE_CONNECTED;
    (void) opal_atomic_add_fetch_32 (&ep->smsg_ep_handle.device->smsg_connections, 1);

    /* send all pending messages */
    BTL_VERBOSE(("endpoint connected. posting %u sends", (unsigned int) opal_list_get_size (&ep->frag_wait_list)));

    rc = mca_btl_ugni_progress_send_wait_list (ep);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
        if (false == ep->wait_listed) {
            opal_list_append (&ugni_module->ep_wait_list, &ep->super);
            ep->wait_listed = true;
        }
        OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
    }

    free (ep->remote_attr);
    ep->remote_attr = NULL;

    return OPAL_SUCCESS;
}

static int mca_btl_ugni_directed_ep_post (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    gni_return_t rc;

    BTL_VERBOSE(("posting directed datagram to remote id: %d for endpoint %p", ep->ep_rem_id, (void *)ep));
    /* the irq cq is associated with only the first device */
    ep->mailbox->attr.rmt_irq_mem_hndl = ugni_module->devices->smsg_irq_mhndl;

    rc = GNI_EpPostDataWId (ep->smsg_ep_handle.gni_handle, &ep->mailbox->attr, sizeof (ep->mailbox->attr),
                            ep->remote_attr, sizeof (*ep->remote_attr),
                            MCA_BTL_UGNI_CONNECT_DIRECTED_ID | ep->index);
    if (OPAL_LIKELY(GNI_RC_SUCCESS == rc)) {
        (void) opal_atomic_add_fetch_32 (&ugni_module->active_datagrams, 1);
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

int mca_btl_ugni_wildcard_ep_post (mca_btl_ugni_module_t *ugni_module)
{
    gni_return_t rc;

    BTL_VERBOSE(("posting wildcard datagram"));

    memset (&ugni_module->wc_local_attr, 0, sizeof (ugni_module->wc_local_attr));
    memset (&ugni_module->wc_remote_attr, 0, sizeof (ugni_module->wc_remote_attr));
    rc = GNI_EpPostDataWId (ugni_module->wildcard_ep, &ugni_module->wc_local_attr,
                            sizeof (ugni_module->wc_local_attr), &ugni_module->wc_remote_attr,
                            sizeof (ugni_module->wc_remote_attr), MCA_BTL_UGNI_CONNECT_WILDCARD_ID);

    return mca_btl_rc_ugni_to_opal (rc);
}


int mca_btl_ugni_ep_connect_progress (mca_btl_base_endpoint_t *ep)
{
    int rc;

    BTL_VERBOSE(("progressing connection for endpoint %p with state %d", (void *)ep, ep->state));

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state) {
        return OPAL_SUCCESS;
    }

    if (MCA_BTL_UGNI_EP_STATE_INIT == ep->state) {
        rc = mca_btl_ugni_ep_connect_start (ep);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    BTL_VERBOSE(("ep->remote_attr->smsg_attr = {.msg_type = %d, .msg_buffer = %p}", ep->remote_attr->smsg_attr.msg_type,
                 (void*)ep->remote_attr->smsg_attr.msg_buffer));

    if (GNI_SMSG_TYPE_INVALID == ep->remote_attr->smsg_attr.msg_type) {
        /* use datagram to exchange connection information with the remote peer */
        if (!ep->dg_posted) {
            rc = mca_btl_ugni_directed_ep_post (ep);
            if (OPAL_SUCCESS == rc) {
                ep->dg_posted = true;
                rc = OPAL_ERR_RESOURCE_BUSY;
            }

            return rc;
        }

        return OPAL_SUCCESS;
    }

    return mca_btl_ugni_ep_connect_finish (ep);
}

int mca_btl_ugni_ep_handle_init (mca_btl_ugni_endpoint_t *ep, gni_cq_handle_t cq,
                                 mca_btl_ugni_device_t *device, mca_btl_ugni_endpoint_handle_t *ep_handle)
{
    gni_return_t grc;

    ep_handle->device = device;

    /* create a uGNI endpoint handle and bind it to the remote peer */
    grc = GNI_EpCreate (device->dev_handle, cq, &ep_handle->gni_handle);
    if (OPAL_LIKELY(GNI_RC_SUCCESS == grc)) {
        grc = GNI_EpBind (ep_handle->gni_handle, ep->ep_rem_addr, ep->ep_rem_id);
    }

    return mca_btl_rc_ugni_to_opal (grc);
}

int mca_btl_ugni_ep_handle_cleanup (mca_btl_ugni_endpoint_handle_t *ep_handle)
{
    int rc;

    if (0 == ep_handle->gni_handle) {
        return OPAL_SUCCESS;
    }

    /* TODO: need to fix, may be outstanding tx's, etc. */
    rc = GNI_EpUnbind (ep_handle->gni_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* should warn */
    } else {
        (void) GNI_EpDestroy (ep_handle->gni_handle);
    }

    ep_handle->gni_handle = 0;

    return OPAL_SUCCESS;
}
