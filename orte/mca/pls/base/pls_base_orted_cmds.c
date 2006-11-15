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

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"

static orte_std_cntr_t orted_cmd_num_active;

static void orte_pls_base_orted_send_cb(int status,
                                        orte_process_name_t* peer,
                                        orte_buffer_t* req,
                                        orte_rml_tag_t tag,
                                        void* cbdata)
{
    /* nothing to do here - this just catches the callback when
     * the send is received on the far end
     */
    return;
}

static void orte_pls_base_cmd_ack(int status, orte_process_name_t* sender,
                                  orte_buffer_t* buffer, orte_rml_tag_t tag,
                                  void* cbdata)
{
    int ret;
    
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    
    orted_cmd_num_active--;
    if (orted_cmd_num_active == 0) {
        opal_condition_signal(&orte_pls_base.orted_cmd_cond);
    } else {
        ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                      ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
        if (ret != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(ret);
            return;
        }
    }
    
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    return;
}


int orte_pls_base_orted_exit(opal_list_t *daemons)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_EXIT_CMD;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;

    OPAL_TRACE(1);
    
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    
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
        
        if (0 > orte_rml.send_buffer_nb(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED,
                                        0, orte_pls_base_orted_send_cb, NULL)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return ORTE_ERR_COMM_FAILURE;
        }
        orted_cmd_num_active++;
    }
    
    /* post the receive for the ack's */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                 ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* wait for all commands to have been ack'd */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        opal_condition_wait(&orte_pls_base.orted_cmd_cond, &orte_pls_base.orted_cmd_lock);
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    
CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}


int orte_pls_base_orted_kill_local_procs(opal_list_t *daemons, orte_jobid_t job)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_KILL_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OPAL_TRACE(1);

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

        if (0 > orte_rml.send_buffer_nb(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED,
                                        0, orte_pls_base_orted_send_cb, NULL)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
            
        orted_cmd_num_active++;
    }

    /* post the receive for the ack's */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                 ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* wait for all commands to have been received */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        opal_condition_wait(&orte_pls_base.orted_cmd_cond, &orte_pls_base.orted_cmd_lock);
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    
    
CLEANUP:
        OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}



int orte_pls_base_orted_signal_local_procs(opal_list_t *daemons, int32_t signal)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SIGNAL_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OPAL_TRACE(1);
    
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
        
        if (0 > orte_rml.send_buffer_nb(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED,
                                        0, orte_pls_base_orted_send_cb, NULL)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
        
        orted_cmd_num_active++;
    }
    
    /* post the receive for the ack's */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                 ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* wait for all commands to have been received */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        opal_condition_wait(&orte_pls_base.orted_cmd_cond, &orte_pls_base.orted_cmd_lock);
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);

CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    /* we're done! */
    return ORTE_SUCCESS;
}
    
    
int orte_pls_base_orted_add_local_procs(opal_list_t *daemons)
{
    int rc;
    orte_buffer_t cmd;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_ADD_LOCAL_PROCS;
    opal_list_item_t *item;
    orte_pls_daemon_info_t *dmn;
    
    OPAL_TRACE(1);
    
    /* pack and send the commands as fast as we can - we have to
     * pack each time as the launch data could be different for
     * the various nodes
     */
    for (item = opal_list_get_first(daemons);
         item != opal_list_get_end(daemons);
         item = opal_list_get_next(item)) {
        dmn = (orte_pls_daemon_info_t*)item;
        
        OBJ_CONSTRUCT(&cmd, orte_buffer_t);
        
        /* pack the command */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        /* pack the launch data for this daemon */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &(dmn->ndat), 1, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        if (0 > orte_rml.send_buffer_nb(dmn->name, &cmd, ORTE_RML_TAG_PLS_ORTED,
                                        0, orte_pls_base_orted_send_cb, NULL)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            OBJ_DESTRUCT(&cmd);
            return rc;
        }
        OBJ_DESTRUCT(&cmd);
        
        orted_cmd_num_active++;
    }
    
    /* post the receive for the ack's */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED_ACK,
                                 ORTE_RML_NON_PERSISTENT, orte_pls_base_cmd_ack, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* wait for the command to have been received */
    OPAL_THREAD_LOCK(&orte_pls_base.orted_cmd_lock);
    if (orted_cmd_num_active > 0) {
        opal_condition_wait(&orte_pls_base.orted_cmd_cond, &orte_pls_base.orted_cmd_lock);
    }
    OPAL_THREAD_UNLOCK(&orte_pls_base.orted_cmd_lock);
    
    return ORTE_SUCCESS;

CLEANUP:
    OBJ_DESTRUCT(&cmd);
    
    return rc;
}

