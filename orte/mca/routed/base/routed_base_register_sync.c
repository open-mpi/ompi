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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/routed/base/base.h"

static bool sync_recvd;

static void report_sync(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag, void *cbdata)
{
    /* just copy the payload to the sync_buf */
    opal_dss.copy_payload(orte_process_info.sync_buf, buffer);
    /* flag as complete */
    sync_recvd = true;
}

int orte_routed_base_register_sync(bool setup)
{
    opal_buffer_t buffer;
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SYNC_BY_PROC;
    char *rml_uri;
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync to daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON)));
    
    /* we need to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    
    /* if we are setting up, tell the daemon to send back a nidmap */
    if (setup) {
        command = ORTE_DAEMON_SYNC_WANT_NIDMAP;
    }


    /* tell the daemon to sync */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    
    /* add our contact info to the buffer so the daemon can explicitly
     * store it
     */
    rml_uri = orte_rml.get_contact_info();
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &rml_uri, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        free(rml_uri);
        return rc;
    }
    if (NULL != rml_uri) free(rml_uri);
    
    /* send the sync command to our daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    OBJ_DESTRUCT(&buffer);
    
    /* get the ack - need this to ensure that the sync communication
     * gets serviced by the event library on the orted prior to the
     * process exiting
     */
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync waiting for ack",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    sync_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SYNC,
                                 ORTE_RML_NON_PERSISTENT, report_sync, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(sync_recvd, 0, 1);
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s registering sync ack recvd",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;
}
