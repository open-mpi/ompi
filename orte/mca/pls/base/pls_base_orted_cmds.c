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
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/pls_private.h"


int orte_pls_base_orted_exit(opal_list_t *daemons)
{
    int rc;
    orte_buffer_t cmd, answer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_EXIT_CMD;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;

    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    opal_output(0, "pls_base_orted_exit: called with %ld daemons", (long)opal_list_get_size(daemons));
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send the commands as fast as we can */
    for (item = opal_list_get_first(daemons);
         item != opal_list_get_end(daemons);
         item = opal_list_get_next(item)) {
        dmn = (orte_pls_daemon_info_t*)item;

        opal_output(0, "pls_base_orted_exit: sending cmd to [%ld,%ld,%ld]", ORTE_NAME_ARGS(dmn->name));
        
        if (0 > orte_rml.send_buffer(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return ORTE_ERR_COMM_FAILURE;
        }
            
        OBJ_CONSTRUCT(&answer, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(dmn->name, &answer, ORTE_RML_TAG_PLS_ORTED)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        }
        OBJ_DESTRUCT(&answer);  
    }

CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}


int orte_pls_base_orted_kill_local_procs(opal_list_t *daemons, orte_jobid_t job)
{
    int rc;
    orte_buffer_t cmd, answer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_KILL_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send the commands as fast as we can */
    for (item = opal_list_get_first(daemons);
         item != opal_list_get_end(daemons);
         item = opal_list_get_next(item)) {
        dmn = (orte_pls_daemon_info_t*)item;

        if (0 > orte_rml.send_buffer(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
            
        OBJ_CONSTRUCT(&answer, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(dmn->name, &answer, ORTE_RML_TAG_PLS_ORTED)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        }
        OBJ_DESTRUCT(&answer);  
    }
    
CLEANUP:
        OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}



int orte_pls_base_orted_signal_local_procs(opal_list_t *daemons, int32_t signal)
{
    int rc;
    orte_buffer_t cmd, answer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SIGNAL_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &signal, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send the commands as fast as we can */
    for (item = opal_list_get_first(daemons);
         item != opal_list_get_end(daemons);
         item = opal_list_get_next(item)) {
        dmn = (orte_pls_daemon_info_t*)item;
        
        if (0 > orte_rml.send_buffer(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
        
        OBJ_CONSTRUCT(&answer, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(dmn->name, &answer, ORTE_RML_TAG_PLS_ORTED)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        }
        OBJ_DESTRUCT(&answer);  
    }
    
CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}
    
    
int orte_pls_base_orted_add_local_procs(opal_list_t *daemons, orte_gpr_notify_data_t *ndat)
{
    int rc;
    orte_buffer_t cmd, answer;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_ADD_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &ndat, 1, ORTE_GPR_NOTIFY_DATA))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send the commands as fast as we can */
    for (item = opal_list_get_first(daemons);
         item != opal_list_get_end(daemons);
         item = opal_list_get_next(item)) {
        dmn = (orte_pls_daemon_info_t*)item;
        
        if (0 > orte_rml.send_buffer(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
        
        OBJ_CONSTRUCT(&answer, orte_buffer_t);
        if (0 > orte_rml.recv_buffer(dmn->name, &answer, ORTE_RML_TAG_PLS_ORTED)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        }
        OBJ_DESTRUCT(&answer);  
    }
    
CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}

