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
    orte_std_cntr_t count, index;
    int rc;
    orte_ns_proxy_cell_info_t *new_cell;

    /* set the default value of error */
    *cellid = ORTE_CELLID_MAX;

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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
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

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    /* store the info locally for later retrieval */
    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);
    new_cell = OBJ_NEW(orte_ns_proxy_cell_info_t);
    if (NULL == new_cell) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&index,
                                orte_ns_proxy.cells, new_cell))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }
    if (NULL != site) {
        new_cell->site = strdup(site);
    }
    if (NULL != resource) {
        new_cell->resource = strdup(resource);
    }

    new_cell->cellid = orte_ns_proxy.num_cells;
    *cellid = new_cell->cellid;
    (orte_ns_proxy.num_cells)++;

    OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
    return ORTE_SUCCESS;
}


int orte_ns_proxy_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_cellid_t j;
    orte_std_cntr_t i, count, index;
    orte_ns_proxy_cell_info_t **cell, *new_cell;
    int rc, ret=ORTE_SUCCESS;

    /* see if we already have the info locally */
    OPAL_THREAD_LOCK(&orte_ns_proxy.mutex);

    cell = (orte_ns_proxy_cell_info_t**)(orte_ns_proxy.cells)->addr;
    for (i=0, j=0; j < orte_ns_proxy.num_cells &&
                   i < (orte_ns_proxy.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            if (cellid == cell[i]->cellid) {
                *site = strdup(cell[i]->site);
                *resource = strdup(cell[i]->resource);
                OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
                return ORTE_SUCCESS;
            }
         }
     }

     /* okay, don't have it locally - go ask for it */

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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
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

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return rc;
    }

    if (ORTE_SUCCESS == ret) {
        /* remote operation worked - store the info locally for any subsequent requests */
        new_cell = OBJ_NEW(orte_ns_proxy_cell_info_t);
        if (NULL == new_cell) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&index,
                                    orte_ns_proxy.cells, new_cell))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
            return rc;
        }
        if (NULL != site) {
            new_cell->site = strdup(*site);
        }
        if (NULL != resource) {
            new_cell->resource = strdup(*resource);
        }

        new_cell->cellid = cellid;
    }

     OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
     return ret;
}

int orte_ns_proxy_create_jobid(orte_jobid_t *job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    /* set default value */
    *job = ORTE_JOBID_MAX;

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    command = ORTE_NS_CREATE_JOBID_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) { /* got a problem */
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    if (ORTE_NS_CREATE_JOBID_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, job, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}


int orte_ns_proxy_reserve_range(orte_jobid_t job, orte_vpid_t range, orte_vpid_t *starting_vpid)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    /* set default return value */
    *starting_vpid = ORTE_VPID_MAX;

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
       ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    command = ORTE_NS_RESERVE_RANGE_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) { /* got a problem */
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(cmd);
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&job, 1, ORTE_JOBID))) { /* got a problem */
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(cmd);
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&range, 1, ORTE_VPID))) { /* got a problem */
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, ORTE_RML_TAG_NS, 0)) {
       ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       OBJ_RELEASE(cmd);
       return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);


    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
       ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
       ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       OBJ_RELEASE(answer);
       return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if ((ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_NS_CMD)))
    || (ORTE_NS_RESERVE_RANGE_CMD != command)) {
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(answer);
       return rc;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, starting_vpid, &count, ORTE_VPID))) {
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(answer);
       return rc;
    }
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}


/*
 * PEER functions
 */
int orte_ns_proxy_get_job_peers(orte_process_name_t **procs,
                                  orte_std_cntr_t *num_procs, orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    /* set default value */
    *procs = NULL;
    *num_procs = 0;

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    command = ORTE_NS_GET_JOB_PEERS_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) { /* got a problem */
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, ORTE_RML_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    if (ORTE_NS_GET_JOB_PEERS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &num_procs, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    /* allocate space for array of proc names */
    if (0 < *num_procs) {
        *procs = (orte_process_name_t*)malloc((*num_procs) * sizeof(orte_process_name_t));
        if (NULL == *procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(answer);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, procs, num_procs, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return rc;
        }
    }

    OBJ_RELEASE(answer);
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, ORTE_RML_TAG_NS, 0)) {
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

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, ORTE_RML_TAG_NS, 0)) {
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

    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, answer, ORTE_RML_TAG_NS)) {
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    return ORTE_SUCCESS;
}

/*
 * DIAGNOSTIC functions
 */
int orte_ns_proxy_dump_cells(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t i;
    orte_cellid_t j;
    orte_ns_proxy_cell_info_t **ptr;
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, &cmd, MCA_OOB_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&answer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&answer);
        return rc;
    }

    /* dump local cell tracker */
    opal_output(mca_ns_base_output, "\n\n[%lu,%lu,%lu] Dump of Local Cell Tracker\n",
        ORTE_NAME_ARGS(orte_process_info.my_name));
    ptr = (orte_ns_proxy_cell_info_t**)(orte_ns_proxy.cells)->addr;
    for (i=0, j=0; j < orte_ns_proxy.num_cells &&
                   i < (orte_ns_proxy.cells)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            opal_output(mca_ns_base_output, "Num: %lu\tCell: %lu\n",
                (unsigned long)j, (unsigned long)ptr[i]->cellid);
        }
    }

    return ORTE_SUCCESS;
}


int orte_ns_proxy_dump_jobs(void)
{
    orte_buffer_t cmd;
    orte_buffer_t answer;
    orte_ns_cmd_flag_t command;
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, &cmd, MCA_OOB_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, &cmd, MCA_OOB_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
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

    if (0 > orte_rml.send_buffer(orte_ns_proxy.my_replica, &cmd, MCA_OOB_TAG_NS, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&cmd);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&cmd);

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.recv_buffer(orte_ns_proxy.my_replica, &answer, ORTE_RML_TAG_NS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&answer);
        OPAL_THREAD_UNLOCK(&orte_ns_proxy.mutex);
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


