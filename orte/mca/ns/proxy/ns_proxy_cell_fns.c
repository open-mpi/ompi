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
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "ns_proxy.h"

/**
 * globals
 */

/*
 * functions
 */

int orte_ns_proxy_create_cellid(orte_cellid_t *cellid, char *site, char *resource)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    OPAL_TRACE(1);
    
    /* set the default value of error */
    *cellid = ORTE_CELLID_INVALID;

    command = ORTE_NS_CREATE_CELLID_CMD;

    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &site, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &resource, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    if (ORTE_NS_CREATE_CELLID_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, cellid, &count, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    OBJ_RELEASE(answer);

    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_proxy_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    OPAL_TRACE(1);
    
    command = ORTE_NS_GET_CELL_INFO_CMD;

    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &cellid, 1, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (ORTE_NS_GET_CELL_INFO_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, site, &count, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, resource, &count, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

     OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
     return ORTE_SUCCESS;
}

int orte_ns_proxy_create_nodeids(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes,
                                 orte_cellid_t cellid, char **nodenames)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count, index;
    int rc;
    
    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);
    
    command = ORTE_NS_CREATE_NODEID_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &cellid, 1, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    count = opal_argv_count(nodenames);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &count, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, nodenames, count, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ORTE_NS_CREATE_NODEID_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &index, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    /** allocate the space for the nodeids */
    *nodeids = (orte_nodeid_t*)malloc(index * sizeof(orte_nodeid_t));
    if (NULL == *nodeids) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, nodeids, &index, ORTE_NODEID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    OBJ_RELEASE(answer);
    
    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_proxy_get_node_info(char ***nodenames, orte_cellid_t cellid,
                                orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count, index;
    int rc, ret=ORTE_SUCCESS;
    
    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);
    
    command = ORTE_NS_GET_NODE_INFO_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &cellid, 1, ORTE_CELLID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &num_nodes, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, nodeids, num_nodes, ORTE_NODEID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    if (ORTE_NS_GET_NODE_INFO_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &index, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    /** create the space for the nodenames */
    *nodenames = (char**)malloc(index * sizeof(char*));
    if (NULL == *nodenames) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, *nodenames, &index, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret, &count, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    
    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
    return ret;
}
