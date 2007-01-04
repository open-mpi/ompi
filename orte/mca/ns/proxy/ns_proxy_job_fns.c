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

#include "ns_proxy.h"

/****    CREATE JOBID    ****/
int orte_ns_proxy_create_jobid(orte_jobid_t *job, opal_list_t *attrs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    OPAL_TRACE(1);
    
    /* set default value */
    *job = ORTE_JOBID_INVALID;

    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    command = ORTE_NS_CREATE_JOBID_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attrs, 1, ORTE_ATTR_LIST))) {
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

    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
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


/****    GET JOB DESCENDANTS    ****/
int orte_ns_proxy_get_job_descendants(orte_jobid_t **descendants, orte_std_cntr_t *num_desc, orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count, ndesc=0;
    orte_jobid_t *jobs=NULL;
    int rc;
    
    OPAL_TRACE(1);
    
    /* set default response */
    *descendants = NULL;
    *num_desc = 0;
    
    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    command = ORTE_NS_GET_JOB_DESC_CMD;
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

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
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

    if (ORTE_NS_GET_JOB_DESC_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ndesc, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    /* if there are any descendants, allocate space for them and unpack */
    if (0 < ndesc) {
        jobs = (orte_jobid_t*)malloc(ndesc * sizeof(orte_jobid_t));
        if (NULL == jobs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(answer);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        count = ndesc;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, jobs, &count, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return rc;
        }
    }

    OBJ_RELEASE(answer);

    *descendants = jobs;
    *num_desc = count;

    return ORTE_SUCCESS;
}

/****    GET JOB CHILDREN    ****/
int orte_ns_proxy_get_job_children(orte_jobid_t **descendants, orte_std_cntr_t *num_desc, orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count, ndesc=0;
    orte_jobid_t *jobs=NULL;
    int rc;
    
    OPAL_TRACE(1);
    
    /* set default response */
    *descendants = NULL;
    *num_desc = 0;
    
    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    command = ORTE_NS_GET_JOB_CHILD_CMD;
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

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
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

    if (ORTE_NS_GET_JOB_DESC_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ndesc, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    /* if there are any descendants, allocate space for them and unpack */
    if (0 < ndesc) {
        jobs = (orte_jobid_t*)malloc(ndesc * sizeof(orte_jobid_t));
        if (NULL == jobs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(answer);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        count = ndesc;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, jobs, &count, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(answer);
            return rc;
        }
    }

    OBJ_RELEASE(answer);

    *descendants = jobs;
    *num_desc = count;

    return ORTE_SUCCESS;
}

int orte_ns_proxy_get_root_job(orte_jobid_t *root_job, orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    /* set default value */
    *root_job = ORTE_JOBID_INVALID;
    
    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    command = ORTE_NS_GET_ROOT_JOB_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
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
    
    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
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
    
    if (ORTE_NS_GET_ROOT_JOB_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, root_job, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;    
}

int orte_ns_proxy_get_parent_job(orte_jobid_t *parent, orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_ns_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    /* set default value */
    *parent = ORTE_JOBID_INVALID;
    
    if ((cmd = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    command = ORTE_NS_GET_PARENT_JOB_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, (void*)&command, 1, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
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
    
    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(answer);
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
    
    if (ORTE_NS_GET_PARENT_JOB_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, parent, &count, ORTE_JOBID))) {
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

    OPAL_TRACE(1);
    
    /* set default return value */
    *starting_vpid = ORTE_VPID_INVALID;

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

    if (0 > orte_rml.send_buffer(ORTE_NS_MY_REPLICA, cmd, ORTE_RML_TAG_NS, 0)) {
       ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       OBJ_RELEASE(cmd);
       return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);


    if ((answer = OBJ_NEW(orte_buffer_t)) == NULL) {
       ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(ORTE_NS_MY_REPLICA, answer, ORTE_RML_TAG_NS)) {
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

