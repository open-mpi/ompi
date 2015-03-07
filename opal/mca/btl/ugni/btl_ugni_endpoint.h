/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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
    MCA_BTL_UGNI_EP_STATE_RDMA,
    MCA_BTL_UGNI_EP_STATE_CONNECTING,
    MCA_BTL_UGNI_EP_STATE_CONNECTED
};
typedef enum mca_btl_ugni_endpoint_state_t mca_btl_ugni_endpoint_state_t;

struct mca_btl_ugni_smsg_mbox_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    opal_proc_t *peer_proc;

    opal_mutex_t lock;
    mca_btl_ugni_endpoint_state_t state;

    opal_common_ugni_endpoint_t *common;

    mca_btl_ugni_module_t *btl;

    gni_ep_handle_t smsg_ep_handle;
    gni_ep_handle_t rdma_ep_handle;

    mca_btl_ugni_endpoint_attr_t remote_attr; /* TODO: UGH, remove this */

    struct mca_btl_ugni_smsg_mbox_t *mailbox;
    gni_mem_handle_t  rmt_irq_mem_hndl;


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
                                        opal_proc_t *peer_proc) {
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

    return OPAL_SUCCESS;
}

static inline void mca_btl_ugni_release_ep (mca_btl_ugni_endpoint_t *ep) {
    int rc;

    if (ep->common) {
        opal_mutex_lock (&ep->lock);

        rc = mca_btl_ugni_ep_disconnect (ep, false);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_VERBOSE(("btl/ugni error disconnecting endpoint"));
        }

        /* TODO -- Clear space at the end of the endpoint array */
        opal_pointer_array_set_item (&ep->btl->endpoints, ep->index, NULL);

        opal_mutex_unlock (&ep->lock);

        opal_common_ugni_endpoint_return (ep->common);
    }

    OBJ_RELEASE(ep);
}

static inline int mca_btl_ugni_check_endpoint_state (mca_btl_ugni_endpoint_t *ep) {
    int rc;

    if (OPAL_LIKELY(MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state)) {
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&ep->lock);

    switch (ep->state) {
    case MCA_BTL_UGNI_EP_STATE_INIT:
    case MCA_BTL_UGNI_EP_STATE_RDMA:
        rc = mca_btl_ugni_ep_connect_progress (ep);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    case MCA_BTL_UGNI_EP_STATE_CONNECTING:
        rc = OPAL_ERR_RESOURCE_BUSY;
        break;
    default:
        rc = OPAL_SUCCESS;
    }

    opal_mutex_unlock (&ep->lock);

    return rc;
}

static inline int mca_btl_ugni_ep_connect_rdma (mca_btl_base_endpoint_t *ep) {
    int rc;

    if (ep->state >= MCA_BTL_UGNI_EP_STATE_RDMA) {
        return OPAL_SUCCESS;
    }

    /* get the modex info for this endpoint and setup a ugni endpoint */
    rc = opal_common_ugni_endpoint_for_proc (ep->btl->device, ep->peer_proc, &ep->common);
    if (OPAL_SUCCESS != rc) {
        assert (0);
        return rc;
    }

    /* bind endpoint to remote address */
    rc = opal_common_ugni_ep_create (ep->common, ep->btl->rdma_local_cq, &ep->rdma_ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    ep->state = MCA_BTL_UGNI_EP_STATE_RDMA;

    return OPAL_SUCCESS;
}

static inline int mca_btl_ugni_check_endpoint_state_rdma (mca_btl_base_endpoint_t *ep) {
    int rc;
    if (OPAL_LIKELY(MCA_BTL_UGNI_EP_STATE_INIT < ep->state)) {
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&ep->lock);
    rc = mca_btl_ugni_ep_connect_rdma (ep);
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

    return opal_common_rc_ugni_to_opal (rc);
}

#endif /* MCA_BTL_UGNI_ENDPOINT_H */
