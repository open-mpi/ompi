/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"


#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "orte/mca/qos/base/base.h"
#include "orte/mca/qos/qos.h"

static int qos_noop_start (void);
static void qos_noop_shutdown (void);
static void* noop_create (opal_list_t *qos_attributes, uint32_t channel_num);
static int noop_open (void *qos_channel,
                       opal_buffer_t * buf);
static int noop_send ( void *qos_channel, orte_rml_send_t *msg);
static int noop_recv (void *channel, orte_rml_recv_t *msg);
static int noop_close (void * channel);
static int noop_init_recv (void *channel, opal_list_t *attributes);
static int noop_cmp (void *channel, opal_list_t *attributes);
static void noop_send_callback (orte_rml_send_t *msg);

/**
 * noop module definition
 */
orte_qos_module_t orte_qos_noop_module = {
   noop_create,
   noop_open,
   noop_send,
   noop_recv,
   noop_close,
   noop_init_recv,
   noop_cmp,
   noop_send_callback
};

/**
 * component definition
 */
mca_qos_base_component_t mca_qos_noop_component = {
    /* First, the mca_base_component_t struct containing meta
         information about the component itself */

    {
        MCA_QOS_BASE_VERSION_2_0_0,

        "noop", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
    },
    qos_noop_start,
    qos_noop_shutdown,
    orte_qos_noop,
    {
        noop_create,
        noop_open,
        noop_send,
        noop_recv,
        noop_close,
        noop_init_recv,
        noop_cmp,
        noop_send_callback
    }
};

static int qos_noop_start(void) {
    return ORTE_SUCCESS;
}

static void qos_noop_shutdown (void) {
}

static void* noop_create (opal_list_t *qos_attributes, uint32_t channel_num) {
    orte_qos_base_channel_t * noop_chan;
    int32_t rc, *window, window_val;
    orte_qos_type_t type_val = orte_qos_noop;
    orte_qos_type_t *type;

    noop_chan = OBJ_NEW (orte_qos_base_channel_t);
    noop_chan->channel_num = channel_num;
    type = &type_val;
    window = &window_val;
    // TBD _ we ignore inapplicable attributes for now - need to return error?
    // get attributes of interest to the base and store them locally.
    if (ORTE_SUCCESS == (rc = orte_set_attribute( &noop_chan->attributes, ORTE_QOS_TYPE, ORTE_ATTR_GLOBAL, (void*)type, OPAL_UINT8))) {
        // window size??
        if( orte_get_attribute (qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&window, OPAL_UINT32)) {
            if ( ORTE_QOS_MAX_WINDOW_SIZE < (*window)) {
                ORTE_ERROR_LOG(OPAL_ERR_VALUE_OUT_OF_BOUNDS);
                OBJ_RELEASE(noop_chan);
            }
            else {
                if (ORTE_SUCCESS != (rc = orte_set_attribute(&noop_chan->attributes, ORTE_QOS_WINDOW_SIZE,
                                          ORTE_ATTR_GLOBAL, (void*)window, OPAL_UINT32))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(noop_chan);
                }
            }
        }else
            OBJ_RELEASE(noop_chan);
    } else {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(noop_chan);
    }
    return noop_chan;
}

static int noop_open (void *qos_channel, opal_buffer_t * buf)
{
    int32_t rc = ORTE_SUCCESS;
    orte_qos_base_channel_t *noop_chan;
    noop_chan = (orte_qos_base_channel_t*) (qos_channel);
    // the Qos module puts the non local attributes  to be sent to the peer in a list at the time of create.
    // pack those attributes into the buffer.
    if (ORTE_SUCCESS != (rc =  orte_qos_base_pack_attributes(buf, &noop_chan->attributes)))
        ORTE_ERROR_LOG(rc);
    return rc;
}

static int noop_send ( void *qos_channel,  orte_rml_send_t *msg)
{
    //nothing to do
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s noop_send msg = %p to peer = %s\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, ORTE_NAME_PRINT(&msg->dst)));
    return ORTE_SUCCESS;
}

static int noop_recv (void *qos_channel, orte_rml_recv_t *msg)
{
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s noop_recv msg = %p from peer = %s\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (void*)msg, ORTE_NAME_PRINT(&msg->sender)));
    return ORTE_SUCCESS;
}

static int noop_close (void * channel)
{
    orte_qos_base_channel_t *noop_chan;
    if(NULL != channel) {
        noop_chan = (orte_qos_base_channel_t*) channel;
        OBJ_RELEASE (noop_chan);
        return ORTE_SUCCESS;
    } else
        return ORTE_ERR_BAD_PARAM;

}

static int noop_init_recv (void *channel, opal_list_t *attributes)
{
    return ORTE_SUCCESS;
}

static int noop_cmp (void *channel, opal_list_t *attributes)
{
    int32_t chan_typea, chan_typeb,  *ptr, window_sizea, window_sizeb;
    orte_qos_base_channel_t *noop_chan = (orte_qos_base_channel_t*) channel;
    ptr = &chan_typea;
    if (!orte_get_attribute(&noop_chan->attributes, ORTE_QOS_TYPE, (void**)&ptr, OPAL_UINT8))
        return ORTE_ERROR;
    ptr = &chan_typeb;
    if (!orte_get_attribute(attributes, ORTE_QOS_TYPE, (void**)&ptr, OPAL_UINT8))
        return ORTE_ERROR;
    if (chan_typea == chan_typeb) {
        ptr = &window_sizea;
        if (!orte_get_attribute(&noop_chan->attributes, ORTE_QOS_WINDOW_SIZE, (void**)&ptr, OPAL_UINT32))
            return ORTE_ERROR;
        ptr = &window_sizeb;
        if (!orte_get_attribute(attributes, ORTE_QOS_WINDOW_SIZE, (void**)&ptr, OPAL_UINT32))
            return ORTE_ERROR;
        return (window_sizea != window_sizeb);
    }
    else
        return ORTE_ERROR;
}

static void noop_send_callback (orte_rml_send_t *msg)
{
    // nothing to do for noop
    ORTE_RML_SEND_COMPLETE(msg);
}
