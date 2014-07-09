/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011-2013 UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"

#include "btl_ugni_endpoint.h"
#include "btl_ugni_smsg.h"

static void mca_btl_ugni_ep_construct (mca_btl_base_endpoint_t *ep)
{
    memset ((char *) ep + sizeof(ep->super), 0, sizeof (*ep) - sizeof (ep->super));
    OBJ_CONSTRUCT(&ep->frag_wait_list, opal_list_t);
    OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);
}

static void mca_btl_ugni_ep_destruct (mca_btl_base_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->frag_wait_list);
    OBJ_DESTRUCT(&ep->lock);
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_endpoint_t, opal_list_item_t,
                   mca_btl_ugni_ep_construct, mca_btl_ugni_ep_destruct);

static inline int mca_btl_ugni_ep_smsg_get_mbox (mca_btl_base_endpoint_t *ep) {
    mca_btl_ugni_module_t *ugni_module = ep->btl;
    ompi_free_list_item_t *mbox;

    OMPI_FREE_LIST_GET_MT(&ugni_module->smsg_mboxes, mbox);
    if (OPAL_UNLIKELY(NULL == mbox)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ep->mailbox = (mca_btl_ugni_smsg_mbox_t *) mbox;
    ep->mailbox->attr.index = ep->index;

    /* per ugni spec we need to zero mailbox data before connecting */
    memset ((char *)ep->mailbox->attr.smsg_attr.msg_buffer + ep->mailbox->attr.smsg_attr.mbox_offset, 0,
            ep->mailbox->attr.smsg_attr.buff_size);
    return OMPI_SUCCESS;
}

int mca_btl_ugni_ep_disconnect (mca_btl_base_endpoint_t *ep, bool send_disconnect) {
    gni_return_t rc;

    if (MCA_BTL_UGNI_EP_STATE_INIT == ep->state) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state && send_disconnect) {
        rc = GNI_SmsgSendWTag (ep->smsg_ep_handle, NULL, 0, NULL, 0, -1,
                               MCA_BTL_UGNI_TAG_DISCONNECT);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni could not send close message"));
        }

        /* we might want to wait for local completion here (do we even care) */
    }

    (void) ompi_common_ugni_ep_destroy (&ep->smsg_ep_handle);
    (void) ompi_common_ugni_ep_destroy (&ep->rdma_ep_handle);

    OMPI_FREE_LIST_RETURN_MT(&ep->btl->smsg_mboxes, ((ompi_free_list_item_t *) ep->mailbox));
    ep->mailbox = NULL;

    ep->state = MCA_BTL_UGNI_EP_STATE_INIT;

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_start (mca_btl_base_endpoint_t *ep) {
    int rc;

    /* get the modex info for this endpoint and setup a ugni endpoint */
    rc = ompi_common_ugni_endpoint_for_proc (ep->btl->device, ep->peer_proc, &ep->common);
    if (OMPI_SUCCESS != rc) {
        assert (0);
        return rc;
    }

    BTL_VERBOSE(("initiaiting connection to remote peer with address: %u id: %u proc: %p",
                 ep->common->ep_rem_addr, ep->common->ep_rem_id, ep->peer_proc));

    /* bind endpoint to remote address */
    /* we bind two endpoints to seperate out local smsg completion and local fma completion */
    rc = ompi_common_ugni_ep_create (ep->common, ep->btl->smsg_local_cq, &ep->smsg_ep_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = ompi_common_ugni_ep_create (ep->common, ep->btl->rdma_local_cq, &ep->rdma_ep_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* build connection data */
    rc = mca_btl_ugni_ep_smsg_get_mbox (ep);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    ep->state = MCA_BTL_UGNI_EP_STATE_CONNECTING;

    memset (&ep->remote_attr, 0, sizeof (ep->remote_attr));

    BTL_VERBOSE(("btl/ugni connection to remote peer initiated"));

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_finish (mca_btl_base_endpoint_t *ep) {
    gni_return_t grc;
    int rc;

    BTL_VERBOSE(("finishing connection. remote attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->remote_attr.smsg_attr.msg_type, ep->remote_attr.smsg_attr.msg_buffer,
                 ep->remote_attr.smsg_attr.buff_size, ep->remote_attr.smsg_attr.mem_hndl.qword1,
                 ep->remote_attr.smsg_attr.mem_hndl.qword2, ep->remote_attr.smsg_attr.mbox_offset,
                 ep->remote_attr.smsg_attr.mbox_maxcredit, ep->remote_attr.smsg_attr.msg_maxsize));

    BTL_VERBOSE(("finishing connection. local attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->mailbox->attr.smsg_attr.msg_type, ep->mailbox->attr.smsg_attr.msg_buffer,
                 ep->mailbox->attr.smsg_attr.buff_size, ep->mailbox->attr.smsg_attr.mem_hndl.qword1,
                 ep->mailbox->attr.smsg_attr.mem_hndl.qword2, ep->mailbox->attr.smsg_attr.mbox_offset,
                 ep->mailbox->attr.smsg_attr.mbox_maxcredit, ep->mailbox->attr.smsg_attr.msg_maxsize));

    grc = GNI_SmsgInit (ep->smsg_ep_handle, &ep->mailbox->attr.smsg_attr, &ep->remote_attr.smsg_attr);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        BTL_ERROR(("error initializing SMSG protocol. rc = %d", grc));

        return ompi_common_rc_ugni_to_ompi (grc);
    }

    /* set the local event data to the local index and the remote event data to my
     * index on the remote peer. This makes lookup of endpoints on completion take
     * a single lookup in the endpoints array. we will not be able to change the
     * remote peer's index in the endpoint's array after this point. */
    GNI_EpSetEventData (ep->rdma_ep_handle, ep->index, ep->remote_attr.index);
    GNI_EpSetEventData (ep->smsg_ep_handle, ep->index, ep->remote_attr.index);

    ep->state = MCA_BTL_UGNI_EP_STATE_CONNECTED;

    /* send all pending messages */
    BTL_VERBOSE(("endpoint connected. posting %u sends", (unsigned int) opal_list_get_size (&ep->frag_wait_list)));

    rc = mca_btl_ugni_progress_send_wait_list (ep);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ep->wait_listed = true;
        opal_list_append (&ep->btl->ep_wait_list, &ep->super);
    }

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_directed_ep_post (mca_btl_base_endpoint_t *ep) {
    gni_return_t rc;

    BTL_VERBOSE(("posting directed datagram to remote id: %d for endpoint %p", ep->common->ep_rem_id, ep));

    rc = GNI_EpPostDataWId (ep->smsg_ep_handle, &ep->mailbox->attr, sizeof (ep->mailbox->attr),
                            &ep->remote_attr, sizeof (ep->remote_attr),
                            MCA_BTL_UGNI_CONNECT_DIRECTED_ID | ep->index);

    return ompi_common_rc_ugni_to_ompi (rc);
}

int mca_btl_ugni_ep_connect_progress (mca_btl_base_endpoint_t *ep) {
    int rc;

    BTL_VERBOSE(("progressing connection for endpoint %p with state %d", ep, ep->state));

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state) {
        return OMPI_SUCCESS;
    }

    if (MCA_BTL_UGNI_EP_STATE_INIT == ep->state) {
        rc = mca_btl_ugni_ep_connect_start (ep);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    if (GNI_SMSG_TYPE_INVALID == ep->remote_attr.smsg_attr.msg_type) {
        /* use datagram to exchange connection information with the remote peer */
        rc = mca_btl_ugni_directed_ep_post (ep);
        if (OMPI_SUCCESS == rc) {
            rc = OMPI_ERR_RESOURCE_BUSY;
        }
        return rc;
    }

    return mca_btl_ugni_ep_connect_finish (ep);
}
