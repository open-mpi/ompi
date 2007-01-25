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
/** @file:
 *
 */

#include "orte_config.h"

#include <string.h>

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/pls/base/pls_private.h"
#include "pls_proxy.h"

/**
 * globals
 */

/*
 * functions
 */

int orte_pls_proxy_launch(orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;

    OPAL_TRACE(1);
    
    command = ORTE_PLS_LAUNCH_JOB_CMD;

    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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

    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

int orte_pls_proxy_terminate_job(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    int32_t timefield;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_TERMINATE_JOB_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attrs, 1, ORTE_ATTR_LIST))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    timefield = timeout->tv_sec;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &timefield, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    timefield = timeout->tv_usec;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &timefield, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

int orte_pls_proxy_terminate_orteds(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    int32_t timefield;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_TERMINATE_ORTEDS_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attrs, 1, ORTE_ATTR_LIST))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    timefield = timeout->tv_sec;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &timefield, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    timefield = timeout->tv_usec;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &timefield, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

int orte_pls_proxy_signal_job(orte_jobid_t job, int32_t signal, opal_list_t *attrs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_SIGNAL_JOB_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &signal, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attrs, 1, ORTE_ATTR_LIST))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}


int orte_pls_proxy_terminate_proc(const orte_process_name_t* name)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_TERMINATE_PROC_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &name, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

int orte_pls_proxy_signal_proc(const orte_process_name_t* name, int32_t signal)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_TERMINATE_PROC_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &name, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &signal, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

int orte_pls_proxy_cancel_operation(void)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_pls_cmd_flag_t command, ret_cmd;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    command = ORTE_PLS_CANCEL_OPERATION_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    if (0 > orte_rml.send_buffer(orte_pls_proxy_replica, cmd, ORTE_RML_TAG_PLS, 0)) {
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
    
    if (0 > orte_rml.recv_buffer(orte_pls_proxy_replica, answer, ORTE_RML_TAG_PLS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &ret_cmd, &count, ORTE_PLS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    if (ret_cmd != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

