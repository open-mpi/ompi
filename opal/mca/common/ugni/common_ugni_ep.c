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

#include "common_ugni.h"

OBJ_CLASS_INSTANCE(opal_common_ugni_endpoint_t, opal_object_t, NULL, NULL);

int opal_common_ugni_endpoint_for_proc (opal_common_ugni_device_t *dev, opal_proc_t *peer_proc,
                                        opal_common_ugni_endpoint_t **ep)
{
    opal_common_ugni_endpoint_t *endpoint;
    opal_common_ugni_modex_t *modex;
    size_t msg_size;
    int rc;

    assert (NULL != dev && NULL != ep && peer_proc);

    endpoint = OBJ_NEW(opal_common_ugni_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        assert (0);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Receive the modex */
    rc = opal_modex_recv(&opal_common_ugni_component, peer_proc,
                         (void *) &modex, &msg_size);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OPAL_OUTPUT((-1, "btl/ugni error receiving modex"));
        return rc;
    }

    endpoint->ep_rem_addr = modex->addr;
    endpoint->ep_rem_id   = modex->id;

    endpoint->dev = dev;

    *ep = endpoint;

    free (modex);

    return OPAL_SUCCESS;
}

void opal_common_ugni_endpoint_return (opal_common_ugni_endpoint_t *ep)
{
    assert(NULL != ep);

    OBJ_RELEASE(ep);
}

int opal_common_ugni_ep_create (opal_common_ugni_endpoint_t *cep, gni_cq_handle_t cq,
                                gni_ep_handle_t *ep_handle)
{
    gni_return_t grc;

    if (OPAL_UNLIKELY(NULL == cep)) {
        assert (0);
        return OPAL_ERR_BAD_PARAM;
    }

    /* create a uGNI endpoint handle and bind it to the remote peer */
    grc = GNI_EpCreate (cep->dev->dev_handle, cq, ep_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
        return opal_common_rc_ugni_to_opal (grc);
    }

    grc = GNI_EpBind (*ep_handle, cep->ep_rem_addr, cep->ep_rem_id);
    if (GNI_RC_SUCCESS != grc) {
        GNI_EpDestroy (*ep_handle);
        return opal_common_rc_ugni_to_opal (grc);
    }

    return OPAL_SUCCESS;
}

int opal_common_ugni_ep_destroy  (gni_ep_handle_t *ep)
{
    int rc;

    if (NULL == ep || 0 == *ep) {
        return OPAL_SUCCESS;
    }

    rc = GNI_EpUnbind (*ep);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* should warn */
    }

    GNI_EpDestroy (*ep);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* should warn */
    }

    *ep = 0;

    return OPAL_SUCCESS;
}

