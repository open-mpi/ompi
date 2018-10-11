/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct.h"
#include "btl_uct_endpoint.h"
#include "btl_uct_device_context.h"
#include "btl_uct_am.h"
#include "opal/util/proc.h"

static void mca_btl_uct_endpoint_construct (mca_btl_uct_endpoint_t *endpoint)
{
    memset (endpoint->uct_eps, 0, sizeof (endpoint->uct_eps[0]) * mca_btl_uct_component.num_contexts_per_module);
    endpoint->conn_ep = NULL;
    OBJ_CONSTRUCT(&endpoint->ep_lock, opal_recursive_mutex_t);
}

static void mca_btl_uct_endpoint_destruct (mca_btl_uct_endpoint_t *endpoint)
{
    for (int tl_index = 0 ; tl_index < 2 ; ++tl_index) {
        for (int i = 0 ; i < mca_btl_uct_component.num_contexts_per_module ; ++i) {
            if (NULL != endpoint->uct_eps[i][tl_index].uct_ep) {
                uct_ep_destroy (endpoint->uct_eps[i][tl_index].uct_ep);
            }
        }
    }

    OBJ_DESTRUCT(&endpoint->ep_lock);
}

OBJ_CLASS_INSTANCE(mca_btl_uct_endpoint_t, opal_object_t,
                   mca_btl_uct_endpoint_construct,
                   mca_btl_uct_endpoint_destruct);

mca_btl_base_endpoint_t *mca_btl_uct_endpoint_create (opal_proc_t *proc)
{
    mca_btl_uct_endpoint_t *endpoint = calloc (1, sizeof (*endpoint) + sizeof (endpoint->uct_eps[0]) *
                                               mca_btl_uct_component.num_contexts_per_module);

    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    OBJ_CONSTRUCT(endpoint, mca_btl_uct_endpoint_t);
    endpoint->ep_proc = proc;

    return (mca_btl_base_endpoint_t *) endpoint;
}

static unsigned char *mca_btl_uct_process_modex_tl (unsigned char *modex_data)
{
    BTL_VERBOSE(("processing modex for tl %s. size: %u", modex_data, *((uint32_t *) modex_data)));

    /* skip size and name */
    return modex_data + 4 + strlen ((char *) modex_data + 4) + 1;
}

static void mca_btl_uct_process_modex (mca_btl_uct_module_t *uct_btl, unsigned char *modex_data,
                                       unsigned char **rdma_tl_data, unsigned char **am_tl_data,
                                       unsigned char **conn_tl_data)
{
    BTL_VERBOSE(("processing remote modex data"));

    if (uct_btl->rdma_tl) {
        BTL_VERBOSE(("modex contains RDMA data"));
        if (rdma_tl_data) {
            *rdma_tl_data = mca_btl_uct_process_modex_tl (modex_data);
        }
        modex_data += *((uint32_t *) modex_data);
    } else if (rdma_tl_data) {
        *rdma_tl_data = NULL;
    }

    if (uct_btl->am_tl && uct_btl->am_tl != uct_btl->rdma_tl) {
        BTL_VERBOSE(("modex contains active message data"));
        if (am_tl_data) {
            *am_tl_data = mca_btl_uct_process_modex_tl (modex_data);
        }
        modex_data += *((uint32_t *) modex_data);
    } else if (am_tl_data) {
        *am_tl_data = NULL;
    }

    if (uct_btl->conn_tl && uct_btl->conn_tl != uct_btl->rdma_tl && uct_btl->conn_tl != uct_btl->am_tl) {
        BTL_VERBOSE(("modex contains connection data"));
        if (conn_tl_data) {
            *conn_tl_data = mca_btl_uct_process_modex_tl (modex_data);
        }
        modex_data += *((uint32_t *) modex_data);
    } else if (conn_tl_data) {
        *conn_tl_data = NULL;
    }
}

static int mca_btl_uct_endpoint_connect_iface (mca_btl_uct_module_t *uct_btl, mca_btl_uct_tl_t *tl,
                                               mca_btl_uct_device_context_t *tl_context,
                                               mca_btl_uct_tl_endpoint_t *tl_endpoint, uint8_t *tl_data)
{
    uct_device_addr_t *device_addr = NULL;
    uct_iface_addr_t *iface_addr;
    ucs_status_t ucs_status;

    /* easy case. just connect to the interface */
    iface_addr = (uct_iface_addr_t *) tl_data;
    device_addr = (uct_device_addr_t *) ((uintptr_t) iface_addr + tl->uct_iface_attr.iface_addr_len);

    BTL_VERBOSE(("connecting endpoint to interface"));

    mca_btl_uct_context_lock (tl_context);
    ucs_status = uct_ep_create_connected (tl_context->uct_iface, device_addr, iface_addr, &tl_endpoint->uct_ep);
    uct_iface_query(tl_context->uct_iface, &tl_endpoint->iface_attr);
    mca_btl_uct_context_unlock (tl_context);

    tl_endpoint->flags = MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY;

    return (UCS_OK == ucs_status) ? OPAL_SUCCESS : OPAL_ERROR;
}

static void mca_btl_uct_connection_ep_construct (mca_btl_uct_connection_ep_t *ep)
{
    ep->uct_ep = NULL;
}

static void mca_btl_uct_connection_ep_destruct (mca_btl_uct_connection_ep_t *ep)
{
    if (ep->uct_ep) {
        uct_ep_destroy (ep->uct_ep);
        ep->uct_ep = NULL;
    }
}

OBJ_CLASS_INSTANCE(mca_btl_uct_connection_ep_t, opal_object_t, mca_btl_uct_connection_ep_construct,
                   mca_btl_uct_connection_ep_destruct);

static int mca_btl_uct_endpoint_send_conn_req (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
                                               mca_btl_uct_device_context_t *conn_tl_context,
                                               int64_t type, void *request, size_t request_length)
{
    mca_btl_uct_connection_ep_t *conn_ep = endpoint->conn_ep;
    ucs_status_t ucs_status;

    BTL_VERBOSE(("sending connection request to peer. type: %" PRId64 ", length: %" PRIsize_t,
                 type, request_length));

    OBJ_RETAIN(endpoint->conn_ep);

    /* need to drop the lock to avoid hold-and-wait */
    opal_mutex_unlock (&endpoint->ep_lock);

    do {
        MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
                ucs_status = uct_ep_am_short (conn_ep->uct_ep, MCA_BTL_UCT_CONNECT_RDMA, type, request, request_length);
            });
        if (OPAL_LIKELY(UCS_OK == ucs_status)) {
            break;
        }

        if (OPAL_UNLIKELY(UCS_ERR_NO_RESOURCE != ucs_status)) {
            return OPAL_ERROR;
        }

        /* some TLs (UD for example) need to be progressed to get resources */
        mca_btl_uct_context_progress (conn_tl_context);
    } while (1);

    /* for now we just wait for the connection request to complete before continuing */
    MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
            do {
                uct_worker_progress (conn_tl_context->uct_worker);
                ucs_status = uct_ep_flush (conn_ep->uct_ep, 0, NULL);
            } while (UCS_INPROGRESS == ucs_status);
        });

    opal_mutex_lock (&endpoint->ep_lock);

    OBJ_RELEASE(endpoint->conn_ep);

    return OPAL_SUCCESS;
}

static int mca_btl_uct_endpoint_connect_endpoint (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint,
                                                  mca_btl_uct_tl_t *tl, mca_btl_uct_device_context_t *tl_context,
                                                  mca_btl_uct_tl_endpoint_t *tl_endpoint, uint8_t *tl_data,
                                                  uint8_t *conn_tl_data, void *ep_addr)
{
    size_t request_length = sizeof (mca_btl_uct_conn_req_t) + tl->uct_iface_attr.ep_addr_len;
    mca_btl_uct_connection_ep_t *conn_ep = endpoint->conn_ep;
    mca_btl_uct_tl_t *conn_tl = uct_btl->conn_tl;
    mca_btl_uct_device_context_t *conn_tl_context = conn_tl->uct_dev_contexts[0];
    mca_btl_uct_conn_req_t *request = alloca (request_length);
    uct_device_addr_t *device_addr = NULL;
    uct_iface_addr_t *iface_addr;
    ucs_status_t ucs_status;
    int rc;

    assert (NULL != conn_tl);

    BTL_VERBOSE(("connecting endpoint to remote endpoint"));

    if (NULL == conn_ep) {
        BTL_VERBOSE(("creating a temporary endpoint for handling connections to %p",
                     opal_process_name_print (endpoint->ep_proc->proc_name)));

        iface_addr = (uct_iface_addr_t *) conn_tl_data;
        device_addr = (uct_device_addr_t *) ((uintptr_t) conn_tl_data + conn_tl->uct_iface_attr.iface_addr_len);

        endpoint->conn_ep = conn_ep = OBJ_NEW(mca_btl_uct_connection_ep_t);
        if (OPAL_UNLIKELY(NULL == conn_ep)) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* create a temporary endpoint for setting up the rdma endpoint */
        MCA_BTL_UCT_CONTEXT_SERIALIZE(conn_tl_context, {
                ucs_status = uct_ep_create_connected  (conn_tl_context->uct_iface, device_addr, iface_addr,
                                                       &conn_ep->uct_ep);
            });
        if (UCS_OK != ucs_status) {
            BTL_VERBOSE(("could not create an endpoint for forming connection to remote peer. code = %d",
                         ucs_status));
            return OPAL_ERROR;
        }
    } else {
        OBJ_RETAIN(conn_ep);
    }

    /* fill in common request parameters */
    request->proc_name = OPAL_PROC_MY_NAME;
    request->context_id = tl_context->context_id;
    request->tl_index = tl->tl_index;

    if (NULL == tl_endpoint->uct_ep) {
        BTL_VERBOSE(("allocating endpoint for peer %s and sending connection data",
                     opal_process_name_print (endpoint->ep_proc->proc_name)));

        MCA_BTL_UCT_CONTEXT_SERIALIZE(tl_context, {
                ucs_status = uct_ep_create (tl_context->uct_iface, &tl_endpoint->uct_ep);
            });
        if (UCS_OK != ucs_status) {
            OBJ_RELEASE(endpoint->conn_ep);
            return OPAL_ERROR;
        }
        uct_iface_query(tl_context->uct_iface, &tl_endpoint->iface_attr);

        /* fill in connection request */
        ucs_status = uct_ep_get_address (tl_endpoint->uct_ep, (uct_ep_addr_t *) request->ep_addr);
        if (UCS_OK != ucs_status) {
            /* this is a fatal a fatal error */
            OBJ_RELEASE(endpoint->conn_ep);
            uct_ep_destroy (tl_endpoint->uct_ep);
            tl_endpoint->uct_ep = NULL;
            return OPAL_ERROR;
        }

        rc = mca_btl_uct_endpoint_send_conn_req (uct_btl, endpoint, conn_tl_context, 0, request,
                                                 request_length);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            OBJ_RELEASE(endpoint->conn_ep);
            uct_ep_destroy (tl_endpoint->uct_ep);
            tl_endpoint->uct_ep = NULL;
            return OPAL_ERROR;
        }
    }

    if (ep_addr) {
        BTL_VERBOSE(("using remote endpoint address to connect endpoint. ep_addr = %p", ep_addr));

        device_addr = (uct_device_addr_t *) tl_data;

        /* NTH: there is no need to lock the device context in this case */
        ucs_status = uct_ep_connect_to_ep (tl_endpoint->uct_ep, device_addr, ep_addr);
        if (UCS_OK != ucs_status) {
            return OPAL_ERROR;
        }

        /* let the remote side know that the connection has been established and
         * wait for the message to be sent */
        rc = mca_btl_uct_endpoint_send_conn_req (uct_btl, endpoint, conn_tl_context, 1, request,
                                                 sizeof (mca_btl_uct_conn_req_t));
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            OBJ_RELEASE(endpoint->conn_ep);
            uct_ep_destroy (tl_endpoint->uct_ep);
            tl_endpoint->uct_ep = NULL;
            return OPAL_ERROR;
        }
    }

    return (tl_endpoint->flags & MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY) ? OPAL_SUCCESS : OPAL_ERR_OUT_OF_RESOURCE;
}

int mca_btl_uct_endpoint_connect (mca_btl_uct_module_t *uct_btl, mca_btl_uct_endpoint_t *endpoint, int context_id,
                                  void *ep_addr, int tl_index)
{
    mca_btl_uct_tl_endpoint_t *tl_endpoint = endpoint->uct_eps[context_id] + tl_index;
    mca_btl_uct_device_context_t *tl_context = mca_btl_uct_module_get_rdma_context_specific (uct_btl, context_id);
    mca_btl_uct_tl_t *tl = (tl_index == uct_btl->rdma_tl->tl_index) ? uct_btl->rdma_tl : uct_btl->am_tl;
    uint8_t *rdma_tl_data = NULL, *conn_tl_data = NULL, *am_tl_data = NULL, *tl_data;
    mca_btl_uct_connection_ep_t *conn_ep = NULL;
    mca_btl_uct_modex_t *modex;
    uint8_t *modex_data;
    size_t msg_size;
    int rc;

    /* only two types of endpoints at this time */
    assert (tl_index < 2);

    if (OPAL_UNLIKELY(NULL == tl)) {
        return OPAL_ERR_UNREACH;
    }

    BTL_VERBOSE(("checking endpoint %p with context id %d. cached uct ep: %p, ready: %d", (void *) endpoint, context_id,
                 (void *) tl_endpoint->uct_ep, !!(MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY & tl_endpoint->flags)));

    opal_mutex_lock (&endpoint->ep_lock);
    if (MCA_BTL_UCT_ENDPOINT_FLAG_CONN_READY & tl_endpoint->flags) {
        opal_mutex_unlock (&endpoint->ep_lock);
        /* nothing more to do. someone else completed the connection */
        return OPAL_SUCCESS;
    }

    /* dumpicate connection request. nothing to do until the endpoint data is received */
    if (NULL != tl_endpoint->uct_ep && NULL == ep_addr) {
        opal_mutex_unlock (&endpoint->ep_lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    do {
        /* read the modex. this is done both to start the connection and to process endpoint data */
        OPAL_MODEX_RECV(rc, &mca_btl_uct_component.super.btl_version,
                        &endpoint->ep_proc->proc_name, (void **)&modex, &msg_size);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_ERROR(("error receiving modex"));
            break;
        }

        BTL_VERBOSE(("received modex of size %lu for proc %s. module count %d", (unsigned long) msg_size,
                     OPAL_NAME_PRINT(endpoint->ep_proc->proc_name), modex->module_count));
        modex_data = modex->data;

        /* look for matching transport in the modex */
        for (int i = 0 ; i < modex->module_count ; ++i) {
            uint32_t modex_size = *((uint32_t *) modex_data);

            BTL_VERBOSE(("found modex for md %s, searching for %s", modex_data + 4, uct_btl->md_name));

            modex_data += 4;

            if (0 != strcmp ((char *) modex_data, uct_btl->md_name)) {
                /* modex belongs to a different module, skip it and continue */
                modex_data += modex_size - 4;
                continue;
            }

            modex_data += strlen ((char *) modex_data) + 1;

            mca_btl_uct_process_modex (uct_btl, modex_data, &rdma_tl_data, &am_tl_data, &conn_tl_data);
            break;
        }

        tl_data = (tl == uct_btl->rdma_tl) ? rdma_tl_data : am_tl_data;

        if (NULL == tl_data) {
            opal_mutex_unlock (&endpoint->ep_lock);
            return OPAL_ERR_UNREACH;
        }

        /* connect the endpoint */
        if (!mca_btl_uct_tl_requires_connection_tl (tl)) {
            rc = mca_btl_uct_endpoint_connect_iface (uct_btl, tl, tl_context, tl_endpoint, tl_data);
        } else {
            rc = mca_btl_uct_endpoint_connect_endpoint (uct_btl, endpoint, tl, tl_context, tl_endpoint,
                                                        tl_data, conn_tl_data, ep_addr);
        }

    } while (0);

    /* to avoid a possible hold-and wait deadlock. destroy the endpoint after dropping the endpoint lock. */
    if (endpoint->conn_ep && 1 == endpoint->conn_ep->super.obj_reference_count) {
        conn_ep = endpoint->conn_ep;
        endpoint->conn_ep = NULL;
    }

    opal_mutex_unlock (&endpoint->ep_lock);

    if (conn_ep) {
        OBJ_RELEASE(conn_ep);
    }

    BTL_VERBOSE(("endpoint%s ready for use", (OPAL_ERR_OUT_OF_RESOURCE != rc) ? "" : " not yet"));

    return rc;
}
