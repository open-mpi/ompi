/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_UGNI_ENDPOINT_H 
#define MCA_BTL_UGNI_ENDPOINT_H

#include "btl_ugni.h"

enum mca_btl_ugni_endpoint_state_t {
    MCA_BTL_UGNI_EP_STATE_INIT = 0,
    MCA_BTL_UGNI_EP_STATE_CONNECTING,
    MCA_BTL_UGNI_EP_STATE_CONNECTED
};
typedef enum mca_btl_ugni_endpoint_state_t mca_btl_ugni_endpoint_state_t;

struct mca_btl_ugni_smsg_mbox_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    ompi_proc_t *peer_proc;

    opal_mutex_t lock;
    mca_btl_ugni_endpoint_state_t state;

    ompi_common_ugni_endpoint_t *common;

    mca_btl_ugni_module_t *btl;

    gni_ep_handle_t smsg_ep_handle;
    gni_ep_handle_t rdma_ep_handle;

    mca_btl_ugni_endpoint_attr_t remote_attr;

    struct mca_btl_ugni_smsg_mbox_t *mailbox;

    opal_list_t frag_wait_list;
    bool wait_listed;

    int32_t smsg_progressing;

    int index;
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t  mca_btl_ugni_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_ugni_endpoint_t);

int mca_btl_ugni_ep_connect_progress (mca_btl_ugni_endpoint_t *ep);
int mca_btl_ugni_ep_disconnect (mca_btl_ugni_endpoint_t *ep, bool send_disconnect);

static inline int mca_btl_ugni_init_ep (mca_btl_ugni_module_t *ugni_module,
                                        mca_btl_ugni_endpoint_t **ep,
                                        mca_btl_ugni_module_t *btl,
                                        ompi_proc_t *peer_proc) {
    mca_btl_ugni_endpoint_t *endpoint;

    endpoint = OBJ_NEW(mca_btl_ugni_endpoint_t);
    assert (endpoint != NULL);

    endpoint->smsg_progressing = 0;
    endpoint->state = MCA_BTL_UGNI_EP_STATE_INIT;

    endpoint->btl = btl;
    endpoint->peer_proc = peer_proc;
    endpoint->common = NULL;
    endpoint->index = opal_pointer_array_add (&ugni_module->endpoints, endpoint);

    *ep = endpoint;

    return OMPI_SUCCESS;
}

static inline void mca_btl_ugni_release_ep (mca_btl_ugni_endpoint_t *ep) {
    int rc;

    if (ep->common) {
        opal_mutex_lock (&ep->lock);

        rc = mca_btl_ugni_ep_disconnect (ep, false);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            BTL_VERBOSE(("btl/ugni error disconnecting endpoint"));
        }

        /* TODO -- Clear space at the end of the endpoint array */
        opal_pointer_array_set_item (&ep->btl->endpoints, ep->index, NULL);

        opal_mutex_unlock (&ep->lock);

        ompi_common_ugni_endpoint_return (ep->common);
    }

    OBJ_RELEASE(ep);
}

static inline int mca_btl_ugni_check_endpoint_state (mca_btl_ugni_endpoint_t *ep) {
    int rc;

    if (OPAL_LIKELY(MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state)) {
        return OMPI_SUCCESS;
    }

    opal_mutex_lock (&ep->lock);

    switch (ep->state) {
    case MCA_BTL_UGNI_EP_STATE_INIT:
        rc = mca_btl_ugni_ep_connect_progress (ep);
        if (OMPI_SUCCESS != rc) {
            break;
        }
    case MCA_BTL_UGNI_EP_STATE_CONNECTING:
        rc = OMPI_ERR_RESOURCE_BUSY;
        break;
    default:
        rc = OMPI_SUCCESS;
    }

    opal_mutex_unlock (&ep->lock);

    return rc;
}

static inline int mca_btl_ugni_wildcard_ep_post (mca_btl_ugni_module_t *ugni_module) {
    gni_return_t rc;

    BTL_VERBOSE(("posting wildcard datagram"));

    memset (&ugni_module->wc_local_attr, 0, sizeof (ugni_module->wc_local_attr));
    memset (&ugni_module->wc_remote_attr, 0, sizeof (ugni_module->wc_remote_attr));
    rc = GNI_EpPostDataWId (ugni_module->wildcard_ep, &ugni_module->wc_local_attr,
                            sizeof (ugni_module->wc_local_attr), &ugni_module->wc_remote_attr,
                            sizeof (ugni_module->wc_remote_attr), MCA_BTL_UGNI_CONNECT_WILDCARD_ID);

    return ompi_common_rc_ugni_to_ompi (rc);
}

#endif /* MCA_BTL_UGNI_ENDPOINT_H */
