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

#include "common_ugni.h"

static void ompi_common_ugni_ep_construct (ompi_common_ugni_endpoint_t *ep)
{
    OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);
    ep->state = OMPI_COMMON_UGNI_INIT;
    ep->bind_count = 0;
}

static void ompi_common_ugni_ep_destruct (ompi_common_ugni_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->lock);
    ompi_common_ugni_endpoint_unbind (ep);
    ep->dev->dev_eps[ep->ep_rem_id] = NULL;
}

OBJ_CLASS_INSTANCE(ompi_common_ugni_endpoint_t, opal_object_t,
                   ompi_common_ugni_ep_construct, ompi_common_ugni_ep_destruct);

int ompi_common_ugni_endpoint_for_proc (ompi_common_ugni_device_t *dev, ompi_proc_t *peer_proc,
                                        ompi_common_ugni_endpoint_t **ep)
{
    ompi_common_ugni_endpoint_t *endpoint;
    ompi_common_ugni_modex_t *modex;
    size_t msg_size;
    int rem_id, rc;

    assert (NULL != dev && NULL != ep && peer_proc);

    rem_id = peer_proc->proc_name.vpid;;

    if (NULL == dev->dev_eps[rem_id]) {
        endpoint = OBJ_NEW(ompi_common_ugni_endpoint_t);
        if (OPAL_UNLIKELY(NULL == endpoint)) {
            assert (0);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* Receive the modex */
        rc = ompi_modex_recv(&ompi_common_ugni_component,
                             peer_proc, (void *)&modex, &msg_size);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            OPAL_OUTPUT((-1, "btl/ugni error receiving modex"));
            return rc;
        }

        /* these should be the same */
        assert (rem_id == modex->id);

        endpoint->ep_rem_addr = modex->addr;
        endpoint->ep_rem_id   = modex->id;

        endpoint->dev = dev;

        *ep = endpoint;

        dev->dev_eps[rem_id] = endpoint;
    } else {
        OBJ_RETAIN(dev->dev_eps[rem_id]);
        *ep = dev->dev_eps[rem_id];
    }

    return OMPI_SUCCESS;
}

void ompi_common_ugni_endpoint_return (ompi_common_ugni_endpoint_t *ep)
{
    assert(NULL != ep);

    OBJ_RELEASE(ep);
}

int ompi_common_ugni_endpoint_bind (ompi_common_ugni_endpoint_t *ep)
{
    int rc;

    assert (NULL != ep);
    if (OPAL_UNLIKELY(NULL == ep)) {
        return OPAL_ERR_BAD_PARAM;
    }

    do {
        if (OPAL_LIKELY(OMPI_COMMON_UGNI_BOUND <= ep->state)) {
            return OMPI_SUCCESS;
        }

        OPAL_THREAD_LOCK(&ep->lock);
        /* create a uGNI endpoint handle and bind it to the remote peer */
        rc = GNI_EpCreate (ep->dev->dev_handle, ep->dev->dev_local_cq,
                           &ep->ep_handle);
        if (GNI_RC_SUCCESS != rc) {
            rc = ompi_common_rc_ugni_to_ompi (rc);
            break;
        }

        rc = GNI_EpBind (ep->ep_handle, ep->ep_rem_addr, ep->ep_rem_id);
        if (GNI_RC_SUCCESS != rc) {
            rc = ompi_common_rc_ugni_to_ompi (rc);
            break;
        }

        ep->state = OMPI_COMMON_UGNI_BOUND;
    } while (0);

    OPAL_THREAD_UNLOCK(&ep->lock);

    return rc;
}

int ompi_common_ugni_endpoint_unbind  (ompi_common_ugni_endpoint_t *ep)
{
    int rc;

    if (0 == ep->bind_count) {
        return OMPI_SUCCESS;
    }

    assert (OMPI_COMMON_UGNI_BOUND == ep->state);

    rc = GNI_EpUnbind (ep->ep_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* should warn */
    }

    GNI_EpDestroy (ep->ep_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* should warn */
    }

    ep->state = OMPI_COMMON_UGNI_INIT;
    ep->bind_count--;

    return OMPI_SUCCESS;
}

