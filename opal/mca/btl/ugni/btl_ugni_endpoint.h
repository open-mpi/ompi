/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
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
    MCA_BTL_UGNI_EP_STATE_CONNECTED,
};
typedef enum mca_btl_ugni_endpoint_state_t mca_btl_ugni_endpoint_state_t;

struct mca_btl_ugni_smsg_mbox_t;

struct mca_btl_ugni_endpoint_handle_t {
    mca_btl_ugni_device_t *device;
    gni_ep_handle_t gni_handle;
};

typedef struct mca_btl_ugni_endpoint_handle_t mca_btl_ugni_endpoint_handle_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    opal_proc_t *peer_proc;

    /** may need to lock recursively as the modex lookup could call opal_progress
     * and hence our progress function. if this changes modify this mutex to not
     * be recursive. also need to update the constructor function. */
    opal_recursive_mutex_t lock;
    mca_btl_ugni_endpoint_state_t state;

    /** Remote NIC address */
    uint32_t ep_rem_addr;

    /** Remote CDM identifier (base) */
    uint32_t ep_rem_id;

    /** endpoint to use for SMSG messages */
    mca_btl_ugni_endpoint_handle_t smsg_ep_handle;

    /** temporary space to store the remote SMSG attributes */
    mca_btl_ugni_endpoint_attr_t *remote_attr;

    /** SMSG mailbox assigned to this endpoint */
    struct mca_btl_ugni_smsg_mbox_t *mailbox;

    /** Remote IRQ handle (for async completion) */
    gni_mem_handle_t rmt_irq_mem_hndl;

    /** frags waiting for SMSG credits */
    opal_list_t frag_wait_list;

    /** endpoint is currently wait-listed for SMSG progress */
    bool wait_listed;

    /** protect against race on connection */
    bool dg_posted;

    /** protect against re-entry to SMSG */
    int32_t smsg_progressing;

    int index;
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t  mca_btl_ugni_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_ugni_endpoint_t);

int mca_btl_ugni_ep_connect_progress (mca_btl_ugni_endpoint_t *ep);
int mca_btl_ugni_ep_disconnect (mca_btl_ugni_endpoint_t *ep, bool send_disconnect);
int mca_btl_ugni_wildcard_ep_post (mca_btl_ugni_module_t *ugni_module);
void mca_btl_ugni_release_ep (mca_btl_ugni_endpoint_t *ep);
int mca_btl_ugni_init_ep (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_endpoint_t **ep,
                          mca_btl_ugni_module_t *btl, opal_proc_t *peer_proc);

static inline int mca_btl_ugni_check_endpoint_state (mca_btl_ugni_endpoint_t *ep) {
    int rc;

    if (OPAL_LIKELY(MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state)) {
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&ep->lock);

    switch (ep->state) {
    case MCA_BTL_UGNI_EP_STATE_INIT:
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

/**
 * Accessor function for endpoint btl
 *
 * @param[in] ep   endpoint to query
 *
 * This helper function exists to make it easy to switch between using a single
 * and multiple ugni modules. Currently there is only one so we just use the
 * pointer in the component structure. This saves 4-8 bytes in the endpoint
 * structure.
 */
static inline mca_btl_ugni_module_t *mca_btl_ugni_ep_btl (mca_btl_ugni_endpoint_t *ep)
{
    /* there is only one ugni module at this time. if that changes add a btl pointer back
     * to the endpoint structure. */
    return mca_btl_ugni_component.modules;
}

/**
 * Initialize and bind an endpoint handle
 *
 * @param[in]  ep                 BTL endpoint
 * @param[in]  cq                 completion queue
 * @param[in]  device             device to bind with
 * @param[in]  ep_handle          endpoint handle to initialize and bind
 */
int mca_btl_ugni_ep_handle_init (mca_btl_ugni_endpoint_t *ep, gni_cq_handle_t cq,
                                 mca_btl_ugni_device_t *device, mca_btl_ugni_endpoint_handle_t *ep_handle);

int mca_btl_ugni_ep_handle_cleanup (mca_btl_ugni_endpoint_handle_t *ep_handle);

#endif /* MCA_BTL_UGNI_ENDPOINT_H */
