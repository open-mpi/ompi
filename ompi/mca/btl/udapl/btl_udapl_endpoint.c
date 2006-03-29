/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"
#include "ompi/mca/mpool/udapl/mpool_udapl.h"
#include "btl_udapl.h"
#include "btl_udapl_endpoint.h" 
#include "btl_udapl_proc.h"
#include "btl_udapl_frag.h"


static int mca_btl_udapl_start_connect(mca_btl_base_endpoint_t* endpoint);

 
/* TODO - do we need to pass the endpoint? It's in the frag */
int mca_btl_udapl_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                mca_btl_udapl_frag_t* frag)
{
    int rc = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&endpoint->endpoint_send_lock);
    switch(endpoint->endpoint_state) {
        case MCA_BTL_UDAPL_CONNECTED:
            /* just send it already.. */
            rc = dat_ep_post_send(endpoint->endpoint_ep, 1, &frag->triplet,
                    (DAT_DTO_COOKIE)(void*)frag, DAT_COMPLETION_DEFAULT_FLAG);
            if(DAT_SUCCESS != rc) {
                mca_btl_udapl_error(rc, "dat_ep_post_send");
                rc = OMPI_ERROR;
            }

            break;
        case MCA_BTL_UDAPL_CLOSED:
            /* Initiate a new connection, add this send to a queue */
            rc = mca_btl_udapl_start_connect(endpoint);
            if(OMPI_SUCCESS != rc) {
                break;
            }

            /* Fall through on purpose to queue the send */
        case MCA_BTL_UDAPL_CONNECTING:
            /* Add this send to a queue */
            opal_list_append(&endpoint->endpoint_frags,
                             (opal_list_item_t*)frag);
            break;
        case MCA_BTL_UDAPL_FAILED:
            rc = OMPI_ERR_UNREACH;
            break;
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_send_lock);

    return rc;
}


static int mca_btl_udapl_start_connect(mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_udapl_module_t* btl = endpoint->endpoint_btl;
    mca_btl_udapl_frag_t* frag;
    int rc;

    /* Create a new uDAPL endpoint and start the connection process */
    rc = dat_ep_create(btl->udapl_ia, btl->udapl_pz,
            btl->udapl_evd_dto, btl->udapl_evd_dto, btl->udapl_evd_conn,
            NULL, &endpoint->endpoint_ep);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ep_create");
        goto failure_create;
    }

    rc = dat_ep_connect(endpoint->endpoint_ep, &endpoint->endpoint_addr.addr,
            endpoint->endpoint_addr.port, mca_btl_udapl_component.udapl_timeout,
            0, NULL, 0, DAT_CONNECT_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ep_connect");
        goto failure;
    }

    /* Send our local address data over this EP */
    /* Can't use btl_udapl_send here, will start an infinite loop! */
    frag = (mca_btl_udapl_frag_t*)mca_btl_udapl_alloc(
            (mca_btl_base_module_t*)btl, sizeof(mca_btl_udapl_addr_t));

    memcpy(frag->segment.seg_addr.pval,
            &btl->udapl_addr, sizeof(mca_btl_udapl_addr_t));
    frag->endpoint = endpoint;
    frag->type = MCA_BTL_UDAPL_CONN_SEND;

    /* Do the actual send now.. */
    rc = dat_ep_post_send(endpoint->endpoint_ep, 1, &frag->triplet,
            (DAT_DTO_COOKIE)(void*)frag, DAT_COMPLETION_DEFAULT_FLAG);
    if(DAT_SUCCESS != rc) {
        mca_btl_udapl_error(rc, "dat_ep_post_send");
        goto failure;
    }

    endpoint->endpoint_state = MCA_BTL_UDAPL_CONNECTING;
    return OMPI_SUCCESS;

failure:
    dat_ep_free(endpoint->endpoint_ep);
failure_create:
    endpoint->endpoint_ep = DAT_HANDLE_NULL;
    endpoint->endpoint_state = MCA_BTL_UDAPL_FAILED;
    return OMPI_ERROR;
}


/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_udapl_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_state = MCA_BTL_UDAPL_CLOSED;
    endpoint->endpoint_ep = DAT_HANDLE_NULL;

    OBJ_CONSTRUCT(&endpoint->endpoint_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, opal_mutex_t);
}


/*
 * Destroy a endpoint
 *
 */

static void mca_btl_udapl_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    OBJ_DESTRUCT(&endpoint->endpoint_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_send_lock);
    OBJ_DESTRUCT(&endpoint->endpoint_recv_lock);
}


OBJ_CLASS_INSTANCE(
    mca_btl_udapl_endpoint_t, 
    opal_list_item_t, 
    mca_btl_udapl_endpoint_construct, 
    mca_btl_udapl_endpoint_destruct);

