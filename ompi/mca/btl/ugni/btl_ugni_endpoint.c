/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_frag.h"

static void mca_btl_ugni_ep_construct (mca_btl_base_endpoint_t *ep);
static void mca_btl_ugni_ep_destruct (mca_btl_base_endpoint_t *ep);

OBJ_CLASS_INSTANCE(mca_btl_base_endpoint_t, opal_object_t,
                   mca_btl_ugni_ep_construct, mca_btl_ugni_ep_destruct);

static void mca_btl_ugni_ep_construct (mca_btl_base_endpoint_t *ep)
{
    OBJ_CONSTRUCT(&ep->pending_list, opal_list_t);
    ep->smsgs_waiting = false;
}

static void mca_btl_ugni_ep_destruct (mca_btl_base_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->pending_list);
}

static void mca_btl_ugni_smsg_mbox_construct (mca_btl_ugni_smsg_mbox_t *mbox) {
    struct mca_btl_ugni_reg_t *reg =
        (struct mca_btl_ugni_reg_t *) mbox->super.registration;

    mbox->buffer = mbox->super.ptr;

    /* initialize mailbox attributes */
    mbox->smsg_attrib.msg_type       = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
    mbox->smsg_attrib.msg_maxsize    = mca_btl_ugni_component.eager_limit +
        sizeof (mca_btl_ugni_frag_hdr_t);
    mbox->smsg_attrib.mbox_maxcredit = mca_btl_ugni_smsg_max_credits;
    mbox->smsg_attrib.mbox_offset    = 0; /* autoselect */
    mbox->smsg_attrib.msg_buffer     = mbox->buffer;
    mbox->smsg_attrib.buff_size      = mca_btl_ugni_smsg_mbox_size;
    mbox->smsg_attrib.mem_hndl       = reg->memory_hdl;
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_smsg_mbox_t, ompi_free_list_item_t,
                   mca_btl_ugni_smsg_mbox_construct, NULL);

static inline int mca_btl_ugni_ep_smsg_get_mbox (mca_btl_base_endpoint_t *ep) {
    mca_btl_ugni_module_t *ugni_module = ep->btl;
    ompi_free_list_item_t *mbox;
    int rc;

    OMPI_FREE_LIST_GET(&ugni_module->smsg_mboxes, mbox, rc);
    if (OPAL_UNLIKELY(NULL == mbox)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ep->mailbox = (mca_btl_ugni_smsg_mbox_t *) mbox;

    /* per ugni spec we need to zero mailbox data before connecting */
    memset (ep->mailbox->buffer, 0, mca_btl_ugni_smsg_mbox_size);
    return rc;
}

int mca_btl_ugni_ep_disconnect (mca_btl_base_endpoint_t *ep, bool send_disconnect) {
    uint32_t msg_id = ORTE_PROC_MY_NAME->vpid;
    char msg;
    int rc;

    OPAL_THREAD_LOCK(&ep->common->lock);

    do {
        if (OMPI_COMMON_UGNI_INIT == MCA_BTL_UGNI_EP_STATE(ep)) {
            /* nothing to do */
            break;
        }

        if (OMPI_COMMON_UGNI_CONNECTED == MCA_BTL_UGNI_EP_STATE(ep) && send_disconnect) {
            rc = GNI_SmsgSendWTag (ep->common->ep_handle, &msg, 1, NULL, 0, msg_id,
                                   MCA_BTL_UGNI_TAG_DISCONNECT);
            if (GNI_RC_SUCCESS != rc) {
                BTL_VERBOSE(("btl/ugni could not send close message"));
            }

            /* we might want to wait for local completion here (do we even care) */
        }

        ep->common->state = OMPI_COMMON_UGNI_BOUND;

        /* drop the lock before we unbind */
        OPAL_THREAD_UNLOCK(&ep->common->lock); 
        rc = ompi_common_ugni_endpoint_unbind (ep->common);
        OPAL_THREAD_LOCK(&ep->common->lock);
        if (OMPI_SUCCESS != rc) {
            BTL_VERBOSE(("btl/ugni error unbinding ugni endpoint"));
        }

        OMPI_FREE_LIST_RETURN(&ep->btl->smsg_mboxes, ((ompi_free_list_item_t *) ep->mailbox));
        ep->mailbox = NULL;
    } while (0);

    OPAL_THREAD_UNLOCK(&ep->common->lock);

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_start (mca_btl_base_endpoint_t *ep) {
    int rc;

    BTL_VERBOSE(("initiaiting connection to remote peer with address: %u id: %u",
                 ep->common->ep_rem_addr, ep->common->ep_rem_id));

    /* bind endpoint to remote address */
    OPAL_THREAD_UNLOCK(&ep->common->lock); 
    rc = ompi_common_ugni_endpoint_bind (ep->common);
    OPAL_THREAD_LOCK(&ep->common->lock); 
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    MCA_BTL_UGNI_EP_STATE(ep) = OMPI_COMMON_UGNI_CONNECTING;

    /* build connection data */
    rc = mca_btl_ugni_ep_smsg_get_mbox (ep);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    memset (&ep->remote_smsg_attrib, 0, sizeof (ep->remote_smsg_attrib));

    BTL_VERBOSE(("btl/ugni connection to remote peer initiated"));

    return OMPI_SUCCESS;
}

static inline int mca_btl_ugni_ep_connect_finish (mca_btl_base_endpoint_t *ep) {
    opal_list_item_t *item;
    int rc;

    BTL_VERBOSE(("finishing connection. remote attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->remote_smsg_attrib.msg_type, ep->remote_smsg_attrib.msg_buffer,
                 ep->remote_smsg_attrib.buff_size, ep->remote_smsg_attrib.mem_hndl.qword1,
                 ep->remote_smsg_attrib.mem_hndl.qword2, ep->remote_smsg_attrib.mbox_offset,
                 ep->remote_smsg_attrib.mbox_maxcredit, ep->remote_smsg_attrib.msg_maxsize));

    BTL_VERBOSE(("finishing connection. local attributes: msg_type = %d, msg_buffer = %p, buff_size = %d, "
                 "mem_hndl = {qword1 = %" PRIu64 ", qword2 = %" PRIu64 "}, mbox = %d, mbox_maxcredit = %d, "
                 "msg_maxsize = %d", ep->mailbox->smsg_attrib.msg_type, ep->mailbox->smsg_attrib.msg_buffer,
                 ep->mailbox->smsg_attrib.buff_size, ep->mailbox->smsg_attrib.mem_hndl.qword1,
                 ep->mailbox->smsg_attrib.mem_hndl.qword2, ep->mailbox->smsg_attrib.mbox_offset,
                 ep->mailbox->smsg_attrib.mbox_maxcredit, ep->mailbox->smsg_attrib.msg_maxsize));

    rc = GNI_SmsgInit (ep->common->ep_handle, &ep->mailbox->smsg_attrib, &ep->remote_smsg_attrib);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("error initializing SMSG protocol. rc = %d", rc));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    BTL_VERBOSE(("endpoint connected. posting %u sends", (unsigned int) opal_list_get_size (&ep->pending_list)));

    MCA_BTL_UGNI_EP_STATE(ep) = OMPI_COMMON_UGNI_CONNECTED;

    /* post pending sends */
    while (NULL != (item = opal_list_remove_first (&ep->pending_list))) {
        mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) item;

        (void) mca_btl_ugni_send (&ep->btl->super, ep, &frag->base, frag->tag);
    }

    return OMPI_SUCCESS;
}

int mca_btl_ugni_ep_connect_progress (mca_btl_base_endpoint_t *ep) {
    int rc;

    if (OMPI_COMMON_UGNI_CONNECTED == MCA_BTL_UGNI_EP_STATE(ep)) {
        return OMPI_SUCCESS;
    }

    if (OMPI_COMMON_UGNI_CONNECTING > ep->common->state) {
        rc = mca_btl_ugni_ep_connect_start (ep);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    if (GNI_SMSG_TYPE_INVALID == ep->remote_smsg_attrib.msg_type) {
        (void) mca_btl_ugni_directed_ep_post (ep);
        return OMPI_ERR_RESOURCE_BUSY;
    }

    return mca_btl_ugni_ep_connect_finish (ep);
}
