/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
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
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rml/rml.h"

#include "ns_proxy.h"

/*
 * PEER functions
 */
int orte_ns_proxy_get_peers(orte_process_name_t **procs,
                            orte_std_cntr_t *num_procs, opal_list_t *attrs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count, nprocs, i;
    orte_cellid_t *cptr;
    orte_attribute_t *attr;
    int rc;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* set default value */
    *procs = NULL;
    *num_procs = 0;

    /* check the attributes to see if USE_JOB or USE_CELL has been set. If not, then this is
     * a request for my own job peers - process that one locally
     */

    /* if the cell is given AND it matches my own, then we can process this
     * quickly. Otherwise, we have to do some more work.
     *
     * RHC: when we go multi-cell, we need a way to find all the cells upon
     * which a job is executing so we can make this work!
     */
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_CELL))) {
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&cptr, attr->value, ORTE_CELLID))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return rc;
        }
        if (*cptr != ORTE_PROC_MY_NAME->cellid && *cptr != ORTE_CELLID_WILDCARD) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return ORTE_ERR_NOT_IMPLEMENTED;            
        }
    }
    
    if (NULL == (attr = orte_rmgr.find_attribute(attrs, ORTE_NS_USE_JOBID))) {
        /* get my own job peers, assuming all are on this cell - process here
         *
         * RHC: This is a bad assumption. When we go multi-cell, we are going to have to process
         * get peer requests solely on the HNP since we won't know the cellid otherwise
         */
        *procs = (orte_process_name_t*)malloc(orte_process_info.num_procs * sizeof(orte_process_name_t));
        if (NULL == *procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        for (i=0; i < orte_process_info.num_procs; i++) {
            (*procs)[i].cellid = ORTE_PROC_MY_NAME->cellid;
            (*procs)[i].jobid = ORTE_PROC_MY_NAME->jobid;
            (*procs)[i].vpid = orte_process_info.vpid_start + i;
        }
        
        *num_procs = orte_process_info.num_procs;
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_SUCCESS;
    }
    
    /* non-local request for peers in another job - send to replica for processing */
    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    command = ORTE_NS_GET_PEERS_CMD;
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) { /* got a problem */
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    /* pack the attributes */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attrs, 1, ORTE_ATTR_LIST))) {
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

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
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

    if (ORTE_NS_GET_PEERS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &nprocs, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    /* allocate space for array of proc names */
    if (0 < nprocs) {
        *procs = (orte_process_name_t*)malloc((nprocs) * sizeof(orte_process_name_t));
        if (NULL == *procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(answer);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, *procs, &nprocs, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return rc;
        }
    }
    *num_procs = nprocs;

    OBJ_RELEASE(answer);
    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_proxy_assign_rml_tag(orte_rml_tag_t *tag,
                                 char *name)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_ns_proxy_tagitem_t* tagitem, **tags;
    orte_std_cntr_t count, i;
    orte_rml_tag_t j;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    if (NULL != name) {
        /* see if this name is already in list - if so, return tag */
        tags = (orte_ns_proxy_tagitem_t**)orte_ns_proxy.tags->addr;
        for (i=0, j=0; j < orte_ns_proxy.num_tags &&
                       i < (orte_ns_proxy.tags)->size; i++) {
            if (NULL != tags[i]) {
                j++;
                if (tags[i]->name != NULL &&
                    0 == strcmp(name, tags[i]->name)) { /* found name on list */
                    *tag = tags[i]->tag;
                    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
                    return ORTE_SUCCESS;
                }
            }
        }
    }

    /* okay, not on local list - so go get one from tag server */
    command = ORTE_NS_ASSIGN_OOB_TAG_CMD;
    *tag = ORTE_RML_TAG_MAX;  /* set the default error value */

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (NULL == name) {
        name = "NULL";
    }

    if (0 > (rc = orte_dss.pack(cmd, &name, 1, ORTE_STRING))) {
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

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
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

    if (ORTE_NS_ASSIGN_OOB_TAG_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, tag, &count, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    OBJ_RELEASE(answer);

    /* add the new tag to the local list so we don't have to get it again */
    tagitem = OBJ_NEW(orte_ns_proxy_tagitem_t);
    if (NULL == tagitem) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_proxy.tags, tagitem))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    tagitem->tag = *tag;
    (orte_ns_proxy.num_tags)++;
    if (NULL != name) {  /* provided - can look it up later */
        tagitem->name = strdup(name);
    } else {
        tagitem->name = NULL;
    }
    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);

    /* all done */
    return ORTE_SUCCESS;
}


int orte_ns_proxy_define_data_type(const char *name,
                                   orte_data_type_t *type)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_ns_proxy_dti_t **dti, *dtip;
    orte_std_cntr_t count, i, j;
    int rc=ORTE_SUCCESS;

    if (NULL == name || 0 < *type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    /* first, check to see if name is already on local list
     * if so, return id, ensure registered with dss
     */
    dti = (orte_ns_proxy_dti_t**)orte_ns_proxy.dts->addr;
    for (i=0, j=0; j < orte_ns_proxy.num_dts &&
                   i < orte_ns_proxy.dts->size; i++) {
        if (NULL != dti[i]) {
            j++;
            if (dti[i]->name != NULL &&
                0 == strcmp(name, dti[i]->name)) { /* found name on list */
                *type = dti[i]->id;
                OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
                return ORTE_SUCCESS;
            }
        }
    }


    /* okay, not on local list - so go get one from tag server */
    command = ORTE_NS_DEFINE_DATA_TYPE_CMD;
    *type = ORTE_DSS_ID_MAX;  /* set the default error value */

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&name, 1, ORTE_STRING))) {
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

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
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

    if (ORTE_NS_ASSIGN_OOB_TAG_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, type, &count, ORTE_DATA_TYPE))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_UNPACK_FAILURE;
    }
    OBJ_RELEASE(answer);

    /* add the new id to the local list so we don't have to get it again */
    dtip = OBJ_NEW(orte_ns_proxy_dti_t);
    if (NULL == dtip) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    dtip->name = strdup(name);
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_proxy.dts, dtip))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    dtip->id = *type;
    (orte_ns_proxy.num_dts)++;

    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);

    /* all done */
    return rc;
}

/*
 * Take advantage of the way the RML uses the process name as its index into
 * the RML communicator table. Because the RML needs a name right away, it will
 * automatically assign us one when it receives a message - and it communicates
 * that assignment back to us automatically. Thus, to get a name for ourselves,
 * all we have to do is send a message! No response from the replica is required.
 */
int orte_ns_proxy_create_my_name(void)
{
    orte_buffer_t* cmd;
    orte_ns_cmd_flag_t command;
    int rc;

    command = ORTE_NS_CREATE_MY_NAME_CMD;

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

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    return ORTE_SUCCESS;
}

