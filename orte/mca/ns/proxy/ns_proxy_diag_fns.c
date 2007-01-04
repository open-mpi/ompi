/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/** @file:
 *
 */

#include "orte_config.h"

#include <string.h>

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/ns/base/base.h"
#include "ns_proxy.h"

/*
 * DIAGNOSTIC functions
 */
int orte_ns_proxy_dump_cells(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    command = ORTE_NS_DUMP_CELLS_CMD;

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* dump name service replica cell tracker */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, &cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }
    
    if (ORTE_NS_DUMP_CELLS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&answer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }

    return ORTE_SUCCESS;
}


int orte_ns_proxy_dump_jobs(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    command = ORTE_NS_DUMP_JOBIDS_CMD;

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* dump name service replica jobid tracker */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, &cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }
    
    if (ORTE_NS_DUMP_JOBIDS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&answer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }

    return ORTE_SUCCESS;
}


int orte_ns_proxy_dump_tags(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t i;
    orte_std_cntr_t count;
    orte_rml_tag_t j;
    orte_ns_proxy_tagitem_t **ptr;
    int rc;

    command = ORTE_NS_DUMP_TAGS_CMD;

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* dump name service replica tag tracker */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, &cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }
    
    if (ORTE_NS_DUMP_TAGS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&answer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }

    /* dump local tag tracker */
    opal_output(mca_ns_base_output, "\n\n[%lu,%lu,%lu] Dump of Local Tag Tracker\n",
        ORTE_NAME_ARGS(orte_process_info.my_name));
    ptr = (orte_ns_proxy_tagitem_t**)(orte_ns_proxy.tags)->addr;
    for (i=0, j=0; j < orte_ns_proxy.num_tags &&
                   i < (orte_ns_proxy.tags)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            opal_output(mca_ns_base_output, "Num: %lu\tTag: %lu\tTag name: %s\n",
                (unsigned long)j, (unsigned long)ptr[i]->tag, ptr[i]->name);
        }
    }

    return ORTE_SUCCESS;
}


int orte_ns_proxy_dump_datatypes(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t i, j;
    orte_std_cntr_t count;
    orte_ns_proxy_dti_t **ptr;
    int rc;

    command = ORTE_NS_DUMP_DATATYPES_CMD;

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* dump name service replica datatype tracker */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, &cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }
    
    if (ORTE_NS_DUMP_DATATYPES_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&answer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }

    /* dump local datatype tracker */
    opal_output(mca_ns_base_output, "\n\n[%lu,%lu,%lu] Dump of Local Datatype Tracker\n",
        ORTE_NAME_ARGS(orte_process_info.my_name));
    ptr = (orte_ns_proxy_dti_t**)(orte_ns_proxy.dts)->addr;
    for (i=0, j=0; j < orte_ns_proxy.num_dts &&
                   i < (orte_ns_proxy.dts)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            opal_output(mca_ns_base_output, "Num: %lu\tDatatype id: %lu\tDatatype name: %s\n",
                (unsigned long)j, (unsigned long)ptr[i]->id, ptr[i]->name);
        }
    }

    return ORTE_SUCCESS;
}


