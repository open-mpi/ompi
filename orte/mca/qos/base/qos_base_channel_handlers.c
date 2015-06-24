/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * qos_base_channel_handlers.c - contains base functions handlers for open, send and close channel requests.
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/timings.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/qos/qos.h"
#include "orte/mca/qos/base/base.h"
#include "orte/mca/rml/base/base.h"


int orte_qos_base_pack_attributes (opal_buffer_t * buffer,
                                          opal_list_t * qos_attributes)
{
    int32_t num_attributes;
    int32_t rc= ORTE_SUCCESS;
    orte_attribute_t *kv;
    num_attributes = opal_list_get_size (qos_attributes);
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s orte_qos_base_pack_attributes num_attributes = %d\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_attributes));
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, (void*)(&num_attributes), 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG (rc);
        return rc;
    }
    OPAL_LIST_FOREACH(kv, qos_attributes, orte_attribute_t) {
        if (ORTE_ATTR_GLOBAL == kv->local) {
            OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                                 "%s orte_qos_base_pack_attributes attribute key = %d value =%d\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 kv->key, kv->data.uint8));
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, (void*)&kv, 1, ORTE_ATTRIBUTE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return rc;
}

void* orte_qos_get_module (opal_list_t *qos_attributes)
{
    int32_t * type, type_val =0;
    mca_qos_base_component_t *qos_comp;
    type = &type_val;
    if(!orte_get_attribute( qos_attributes, ORTE_QOS_TYPE, (void**)&type, OPAL_UINT8))
        return NULL;
    OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                         "%s orte_qos_get_module channel type = %d\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         type_val));
     //check if type is valid
     if (type_val < 0 || ORTE_QOS_MAX_COMPONENTS <= type_val)
        return  NULL;
    // associate the qos  module
    qos_comp = (mca_qos_base_component_t *) opal_pointer_array_get_item(&orte_qos_base.actives, type_val);
    if (NULL != qos_comp)
    {
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s qos_base_get_module returning  qos module %p type =%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (void*)&qos_comp->mod, type_val));
        return (void*)(&qos_comp->mod);
    } else {
        OPAL_OUTPUT_VERBOSE((1, orte_qos_base_framework.framework_output,
                             "%s qos_base_get_module failed to get qos component of type =%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             type_val));
    }
    return NULL;
}

void * orte_qos_create_channel (void *qos_mod, opal_list_t *qos_attributes, uint32_t channel_num) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        return qos->create(qos_attributes, channel_num);
    else
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
    return NULL;
}

int orte_qos_open_channel (void *qos_mod, void *qos_channel, opal_buffer_t * buffer) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        return (qos->open (qos_channel, buffer));
    else
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
    return ORTE_ERR_BAD_PARAM;
}

int orte_qos_close_channel (void *qos_mod, void *qos_channel) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if ((NULL != qos) && (NULL != qos_channel))
        return (qos->close (qos_channel));
    else
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
    return (ORTE_ERR_BAD_PARAM);
}

void orte_qos_init_recv_channel (void *qos_mod, void *qos_channel, opal_list_t * qos_attributes) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        qos->init_recv (qos_channel, qos_attributes);
    else
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
}

int orte_qos_cmp_channel (void *qos_mod, void *qos_channel, opal_list_t * qos_attributes) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        return (qos->cmp (qos_channel, qos_attributes));
    ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
    return -1;
}

int orte_qos_send_channel (void *qos_mod, void *qos_channel, orte_rml_send_t *msg) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        return(qos->send (qos_channel, msg));
    else
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
    return ORTE_ERROR;
}

int orte_qos_recv_channel (void *qos_mod, void *qos_channel, orte_rml_recv_t *msg) {
    orte_qos_module_t *qos = (orte_qos_module_t *) (qos_mod);
    if (NULL != qos)
        return(qos->recv(qos_channel, msg));
    else {
        ORTE_ERROR_LOG (ORTE_ERR_BAD_PARAM);
        return ORTE_ERROR;
    }
}


