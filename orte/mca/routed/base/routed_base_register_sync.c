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

#include "orte/mca/routed/base/base.h"

int orte_routed_base_register_sync(void)
{
    opal_buffer_t buffer, ack;
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SYNC_BY_PROC;
    char *rml_uri;
    
    /* we need to send a very small message to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    
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
    OBJ_CONSTRUCT(&ack, opal_buffer_t);
    if (0 > orte_rml.recv_buffer(ORTE_PROC_MY_DAEMON, &ack, ORTE_RML_TAG_SYNC, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&ack);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&ack);
    
    return ORTE_SUCCESS;
}
