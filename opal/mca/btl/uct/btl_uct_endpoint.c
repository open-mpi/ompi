/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_endpoint.h"
#include "btl_uct.h"
#include "btl_uct_am.h"
#include "btl_uct_device_context.h"
#include "btl_uct_modex.h"
#include "opal/mca/timer/base/base.h"
#include "opal/util/proc.h"

static void mca_btl_uct_endpoint_construct(mca_btl_uct_endpoint_t *endpoint)
{
    memset(endpoint->uct_eps, 0,
           sizeof(endpoint->uct_eps[0]) * mca_btl_uct_component.num_contexts_per_module);
    endpoint->conn_ep = NULL;
    OBJ_CONSTRUCT(&endpoint->ep_lock, opal_mutex_t);
}

static void mca_btl_uct_endpoint_destruct(mca_btl_uct_endpoint_t *endpoint)
{
    for (int tl_index = 0; tl_index < 2; ++tl_index) {
        for (int i = 0; i < mca_btl_uct_component.num_contexts_per_module; ++i) {
            if (NULL != endpoint->uct_eps[i][tl_index].uct_ep) {
                uct_ep_destroy(endpoint->uct_eps[i][tl_index].uct_ep);
            }
        }
    }

    OBJ_DESTRUCT(&endpoint->ep_lock);
    if (endpoint->conn_ep) {
        OBJ_RELEASE(endpoint->conn_ep);
    }
}

OBJ_CLASS_INSTANCE(mca_btl_uct_endpoint_t, opal_object_t, mca_btl_uct_endpoint_construct,
                   mca_btl_uct_endpoint_destruct);

mca_btl_base_endpoint_t *mca_btl_uct_endpoint_create(opal_proc_t *proc)
{
    mca_btl_uct_endpoint_t *endpoint = calloc(1, sizeof(*endpoint)
                                                     + sizeof(endpoint->uct_eps[0])
                                                           * mca_btl_uct_component
                                                                 .num_contexts_per_module);

    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    OBJ_CONSTRUCT(endpoint, mca_btl_uct_endpoint_t);
    endpoint->ep_proc = proc;

    return (mca_btl_base_endpoint_t *) endpoint;
}

static inline ucs_status_t mca_btl_uct_ep_create_connected_compat(uct_iface_h iface,
                                                                  uct_device_addr_t *device_addr,
                                                                  uct_iface_addr_t *iface_addr,
                                                                  uct_ep_h *uct_ep)
{
#if UCT_API >= UCT_VERSION(1, 6)
    uct_ep_params_t ep_params = {.field_mask = UCT_EP_PARAM_FIELD_IFACE
                                               | UCT_EP_PARAM_FIELD_DEV_ADDR
                                               | UCT_EP_PARAM_FIELD_IFACE_ADDR,
                                 .iface = iface,
                                 .dev_addr = device_addr,
                                 .iface_addr = iface_addr};
    return uct_ep_create(&ep_params, uct_ep);
#else
    return uct_ep_create_connected(iface, device_addr, iface_addr, uct_ep);
#endif
}

static inline ucs_status_t mca_btl_uct_ep_create_compat(uct_iface_h iface, uct_ep_h *uct_ep)
{
#if UCT_API >= UCT_VERSION(1, 6)
    uct_ep_params_t ep_params = {.field_mask = UCT_EP_PARAM_FIELD_IFACE, .iface = iface};
    return uct_ep_create(&ep_params, uct_ep);
#else
    return uct_ep_create(iface, uct_ep);
#endif
}

static int mca_btl_uct_endpoint_connect_iface(mca_btl_uct_module_t *uct_btl, mca_btl_uct_tl_t *tl,
                                              mca_btl_uct_device_context_t *tl_context,
                                              mca_btl_uct_tl_endpoint_t *tl_endpoint,
                                              uint8_t *tl_data)
{
    uct_device_addr_t *device_addr = NULL;
    uct_iface_addr_t *iface_addr;
    ucs_status_t ucs_status;

    /* easy case. just connect to the interface */
    iface_addr = (uct_iface_addr_t *) tl_data;
    device_addr = (uct_device_addr_t *) ((uintptr_t) iface_addr
                                         + tl->uct_iface_attr
                                               .iface_addr_len);

    BTL_VERBOSE(("connecting endpoint to interface"));

    mca_btl_uct_context_lock(tl_context);
    ucs_status = mca_btl_uct_ep_create_connected_compat(tl_context->uct_iface, device_addr,
                                                        iface_addr, &tl_endpoint->uct_ep);
    tl_endpoint->flags = MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY;
    mca_btl_uct_context_unlock(tl_context);

    return (UCS_OK == ucs_status) ? OPAL_SUCCESS : OPAL_ERROR;
}

struct mca_btl_uct_conn_completion_t {
    uct_completion_t super;
    volatile bool complete;
};
typedef struct mca_btl_uct_conn_completion_t mca_btl_uct_conn_completion_t;

#if UCT_API >= ((1L<<UCT_MAJOR_BIT)|(10L << UCT_MINOR_BIT))
static void mca_btl_uct_endpoint_flush_complete(uct_completion_t *self)
{
    mca_btl_uct_conn_completion_t *completion = (mca_btl_uct_conn_completion_t *) self;
    BTL_VERBOSE(("connection flush complete"));
    completion->complete = true;
}
#else
static void mca_btl_uct_endpoint_flush_complete(uct_completion_t *self, ucs_status_t status)
{
    mca_btl_uct_conn_completion_t *completion = (mca_btl_uct_conn_completion_t *) self;
    (void) status;
    BTL_VERBOSE(("connection flush complete"));
    completion->complete = true;
}
#endif

static void mca_btl_uct_flush_conn_endpoint(mca_btl_uct_connection_ep_t *conn_ep)
{
    mca_btl_uct_device_context_t *conn_tl_context = conn_ep->tl->uct_dev_contexts[0];
    mca_btl_uct_conn_completion_t completion
        = {.super = {.count = 1, .func = mca_btl_uct_endpoint_flush_complete}, .complete = false};
    ucs_status_t ucs_status;
    MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
            ucs_status = uct_ep_flush(conn_ep->uct_ep, 0, &completion.super);
    });
    if (UCS_OK != ucs_status && UCS_INPROGRESS != ucs_status) {
        /* NTH: I don't know if this path is needed. For some networks we must use a completion. */
        do {
            MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
                    ucs_status = uct_ep_flush(conn_ep->uct_ep, 0, NULL);
            });
            mca_btl_uct_context_progress(conn_tl_context);
        } while (UCS_INPROGRESS == ucs_status);
    } else if (UCS_OK != ucs_status) {
        do {
            mca_btl_uct_context_progress(conn_tl_context);
        } while (!completion.complete);
    }
}

static void mca_btl_uct_connection_ep_construct(mca_btl_uct_connection_ep_t *ep)
{
    ep->uct_ep = NULL;
    ep->tl = NULL;
}

static void mca_btl_uct_connection_ep_destruct(mca_btl_uct_connection_ep_t *ep)
{
    if (ep->uct_ep) {
        mca_btl_uct_flush_conn_endpoint(ep);
        uct_ep_destroy(ep->uct_ep);
        ep->uct_ep = NULL;
    }
}

OBJ_CLASS_INSTANCE(mca_btl_uct_connection_ep_t, opal_object_t, mca_btl_uct_connection_ep_construct,
                   mca_btl_uct_connection_ep_destruct);

static int mca_btl_uct_endpoint_send_conn_req(mca_btl_uct_module_t *uct_btl,
                                              mca_btl_base_endpoint_t *endpoint,
                                              mca_btl_uct_conn_req_t *request,
                                              size_t request_length)
{
    mca_btl_uct_device_context_t *conn_tl_context = mca_btl_uct_component.conn_tl->uct_dev_contexts[0];

    BTL_VERBOSE(
        ("sending connection request to peer. context id: %d, type: %d, length: %" PRIsize_t,
         request->context_id, request->type, request_length));

    do {
        ucs_status_t ucs_status;
        MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
            ucs_status = uct_ep_am_short(endpoint->conn_ep->uct_ep, MCA_BTL_UCT_CONNECT_RDMA,
                                         request->type, request, request_length);
        });
        if (OPAL_LIKELY(UCS_OK == ucs_status)) {
            break;
        }

        if (OPAL_UNLIKELY(UCS_ERR_NO_RESOURCE != ucs_status)) {
            return OPAL_ERROR;
        }

        /* need to drop the lock to avoid hold-and-wait */
        opal_mutex_unlock(&endpoint->ep_lock);
        /* some TLs (UD for example) need to be progressed to get resources */
        mca_btl_uct_context_progress(conn_tl_context);
        opal_mutex_lock(&endpoint->ep_lock);
    } while (1);

    return OPAL_SUCCESS;
}

static int mca_btl_uct_endpoint_get_helper_endpoint(mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
                                                    uint8_t *conn_tl_data)
{
    if (NULL != endpoint->conn_ep) {
        BTL_VERBOSE(("re-using existing connection endpoint"));
        OBJ_RETAIN(endpoint->conn_ep);
        return OPAL_SUCCESS;
    }

    mca_btl_uct_tl_t *conn_tl = mca_btl_uct_component.conn_tl;

    BTL_VERBOSE(("creating a temporary endpoint for handling connections to %p",
                 opal_process_name_print(endpoint->ep_proc->proc_name)));

    uct_iface_addr_t *iface_addr = (uct_iface_addr_t *) conn_tl_data;
    uct_device_addr_t *device_addr = (uct_device_addr_t *) ((uintptr_t) conn_tl_data
                                                            + conn_tl->uct_iface_attr.iface_addr_len);

    endpoint->conn_ep = OBJ_NEW(mca_btl_uct_connection_ep_t);
    if (OPAL_UNLIKELY(NULL == endpoint->conn_ep)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    endpoint->conn_ep->tl = conn_tl;

    ucs_status_t ucs_status;
    mca_btl_uct_device_context_t *conn_tl_context = conn_tl->uct_dev_contexts[0];
    /* create a temporary endpoint for setting up the rdma endpoint */
    MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
            ucs_status = mca_btl_uct_ep_create_connected_compat(conn_tl_context->uct_iface,
                                                                device_addr, iface_addr,
                                                                &endpoint->conn_ep->uct_ep);
        });
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(
                    ("could not create an endpoint for forming connection to remote peer. code = %d",
                     ucs_status));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_btl_uct_endpoint_send_connection_data(
    mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
    mca_btl_uct_tl_t *tl, mca_btl_uct_device_context_t *tl_context,
    mca_btl_uct_tl_endpoint_t *tl_endpoint, int request_type, int remote_module_index)
{
    ucs_status_t ucs_status;

    BTL_VERBOSE(("connecting endpoint to remote endpoint"));

    size_t request_length = sizeof(mca_btl_uct_conn_req_t)
                            + tl->uct_iface_attr.ep_addr_len;
    mca_btl_uct_conn_req_t *request = alloca(request_length);

    /* fill in common request parameters */
    request->proc_name = OPAL_PROC_MY_NAME;
    request->context_id = tl_context->context_id;
    request->tl_index = tl->tl_index;
    request->type = request_type;
    request->module_index = remote_module_index;

    /* fill in connection request */
    ucs_status = uct_ep_get_address(tl_endpoint->uct_ep, (uct_ep_addr_t *) request->ep_addr);
    if (UCS_OK != ucs_status) {
        /* this is a fatal a fatal error */
        OBJ_RELEASE(endpoint->conn_ep);
        uct_ep_destroy(tl_endpoint->uct_ep);
        tl_endpoint->uct_ep = NULL;
        return OPAL_ERROR;
    }

    /* let the remote side know that the connection has been established and
     * wait for the message to be sent */
    int rc = mca_btl_uct_endpoint_send_conn_req(uct_btl, endpoint, request,
                                                request_length);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OBJ_RELEASE(endpoint->conn_ep);
        uct_ep_destroy(tl_endpoint->uct_ep);
        tl_endpoint->uct_ep = NULL;
        return OPAL_ERROR;
    }

    tl_endpoint->last_connection_req = opal_timer_base_get_usec();

    return OPAL_SUCCESS;
}

static int mca_btl_uct_endpoint_connect_endpoint(
    mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
    mca_btl_uct_tl_t *tl, mca_btl_uct_device_context_t *tl_context,
    mca_btl_uct_tl_endpoint_t *tl_endpoint, uint8_t *tl_data, void *ep_addr, int remote_module_index)
{
    ucs_status_t ucs_status;

    if (NULL == tl_endpoint->uct_ep) {
        BTL_VERBOSE(("allocating endpoint for peer %s and sending connection data",
                     opal_process_name_print(endpoint->ep_proc->proc_name)));

        MCA_BTL_UCT_CONTEXT_SERIALIZE(tl_context, {
            ucs_status = mca_btl_uct_ep_create_compat(tl_context->uct_iface, &tl_endpoint->uct_ep);
        });
        if (UCS_OK != ucs_status) {
            OBJ_RELEASE(endpoint->conn_ep);
            return OPAL_ERROR;
        }
    }

    if (ep_addr) {
        BTL_VERBOSE(
            ("using remote endpoint address to connect endpoint for tl %s, index %d. ep_addr = %p",
             tl->uct_tl_name, tl_context->context_id, ep_addr));

        /* NTH: there is no need to lock the device context in this case */
        ucs_status = uct_ep_connect_to_ep(tl_endpoint->uct_ep, (uct_device_addr_t *) tl_data,
                                          ep_addr);
        if (UCS_OK != ucs_status) {
            return OPAL_ERROR;
        }
    }

    opal_timer_t now = opal_timer_base_get_usec();
    if ((now - tl_endpoint->last_connection_req) > mca_btl_uct_component.connection_retry_timeout || ep_addr) {
        int rc = mca_btl_uct_endpoint_send_connection_data(uct_btl, endpoint, tl, tl_context, tl_endpoint,
                                                           /*request_type=*/!!ep_addr, remote_module_index);
        if (OPAL_SUCCESS != rc) {
            return rc;
        }
    }

    if (ep_addr) {
        mca_btl_uct_endpoint_set_flag(uct_btl, endpoint, tl_context->context_id, tl_endpoint,
                                      MCA_BTL_UCT_ENDPOINT_FLAG_EP_CONNECTED);
    }

    return OPAL_ERR_OUT_OF_RESOURCE;
}

int mca_btl_uct_endpoint_connect(mca_btl_uct_module_t *uct_btl, mca_btl_uct_endpoint_t *endpoint,
                                 int context_id, void *ep_addr, int tl_index)
{
    mca_btl_uct_tl_endpoint_t *tl_endpoint = endpoint->uct_eps[context_id] + tl_index;
    mca_btl_uct_tl_t *tl = (uct_btl->rdma_tl && tl_index == uct_btl->rdma_tl->tl_index)
                               ? uct_btl->rdma_tl
                               : uct_btl->am_tl;
    mca_btl_uct_device_context_t *tl_context
        = mca_btl_uct_module_get_tl_context_specific(uct_btl, tl, context_id);
    uint8_t *conn_tl_data, *tl_data = NULL;
    mca_btl_uct_modex_t *modex;
    size_t msg_size;
    int rc;

    /* only two types of endpoints at this time */
    assert(tl_index < 2);

    if (OPAL_UNLIKELY(NULL == tl)) {
        return OPAL_ERR_UNREACH;
    }

    BTL_VERBOSE(("checking endpoint %p with context id %d. cached uct ep: %p, ready: %d",
                 (void *) endpoint, context_id, (void *) tl_endpoint->uct_ep,
                 !!(MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY & tl_endpoint->flags)));

    opal_mutex_lock(&endpoint->ep_lock);

    do {
        if (MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY & tl_endpoint->flags) {
            /* nothing more to do. someone else completed the connection */
            rc = OPAL_SUCCESS;
            break;
        }

        /* dumpicate connection request. nothing to do until the endpoint data is received */
        if (NULL != tl_endpoint->uct_ep && NULL == ep_addr) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            break;
        }

        /* read the modex. this is done both to start the connection and to process endpoint data */
        OPAL_MODEX_RECV(rc, &mca_btl_uct_component.super.btl_version, &endpoint->ep_proc->proc_name,
                        (void **) &modex, &msg_size);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("error receiving modex"));
            break;
        }

        BTL_VERBOSE(("received modex of size %lu for proc %s. module count %d",
                     (unsigned long) msg_size, OPAL_NAME_PRINT(endpoint->ep_proc->proc_name),
                     modex->module_count));

        int remote_module_index;
        tl_data = mca_btl_uct_find_modex(modex, tl, &remote_module_index);
        if (OPAL_UNLIKELY(NULL == tl_data)) {
            BTL_ERROR(("could not find modex data for this transport"));
            rc = OPAL_ERR_UNREACH;
            break;
        }

        /* connect the endpoint */
        if (mca_btl_uct_tl_requires_connection_tl(tl)) {
            conn_tl_data = mca_btl_uct_find_modex(modex, mca_btl_uct_component.conn_tl,
                                                  /*remote_module_index=*/NULL);
            if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
                BTL_ERROR(("could not find modex for connection module"));
                break;
            }


            if (NULL == tl_endpoint->uct_ep) {
                /* allocate or retain a connection endpoint */
                rc = mca_btl_uct_endpoint_get_helper_endpoint(uct_btl, endpoint,
                                                              conn_tl_data);
                if (OPAL_SUCCESS != rc) {
                    break;
                }
            }

            rc = mca_btl_uct_endpoint_connect_endpoint(uct_btl, endpoint, tl, tl_context, tl_endpoint,
                                                       tl_data, ep_addr, remote_module_index);
        } else {
            rc = mca_btl_uct_endpoint_connect_iface(uct_btl, tl, tl_context, tl_endpoint, tl_data);
        }
    } while (0);

    opal_mutex_unlock(&endpoint->ep_lock);

    BTL_VERBOSE(("endpoint%s ready for use", (OPAL_ERR_OUT_OF_RESOURCE != rc) ? "" : " not yet"));

    return rc;
}
