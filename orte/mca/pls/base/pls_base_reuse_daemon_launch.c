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

#include "opal/util/show_help.h"

#include "orte/dss/dss.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"

#include "orte/mca/pls/base/pls_private.h"

int orte_pls_base_launch_apps(orte_job_map_t *map)
{
    orte_daemon_cmd_flag_t command;
    orte_buffer_t *buffer;
    orte_gpr_notify_data_t *launch_data;
    int rc;

    /* let the local launcher provide its required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(&launch_data, map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the buffer */
    buffer = OBJ_NEW(orte_buffer_t);
    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    
    /* pack the launch data */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &launch_data, 1, ORTE_GPR_NOTIFY_DATA))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    
    /* send the command to the daemons */
    if (ORTE_SUCCESS != (rc = orte_rml.xcast(0, buffer, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(buffer);
    
    return rc;
}

int orte_pls_base_daemon_callback(orte_std_cntr_t num_daemons)
{
    orte_std_cntr_t i;
    orte_buffer_t ack, handoff;
    orte_process_name_t name;
    int src[4];
    int rc, idx;
    
    for(i = 0; i < num_daemons; i++) {
        OBJ_CONSTRUCT(&ack, orte_buffer_t);
        rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &ack, ORTE_RML_TAG_ORTED_CALLBACK);
        if(0 > rc) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&ack);
            return rc;
        }
        /* a daemon actually only sends us back one int value. However, if
         * the daemon fails to launch, our local launcher may have additional
         * info it wants to pass back to us, so we allow up to four int
         * values to be returned. Fortunately, the DSS unpack routine
         * knows how to handle this situation - it will only unpack the
         * actual number of packed entries up to the number we specify here
         */
        idx = 4;
        rc = orte_dss.unpack(&ack, &src, &idx, ORTE_INT);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&ack);
            return rc;
        }
        
        if(-1 == src[0]) {
            /* one of the daemons has failed to properly launch. The error is sent
            * by orte_pls_bproc_waitpid_daemon_cb  */
            if(-1 == src[1]) { /* did not die on a signal */
                opal_show_help("help-pls-base.txt", "daemon-died-no-signal", true, src[2]);
            } else { /* died on a signal */
                opal_show_help("help-pls-base.txt", "daemon-died-signal", true,
                               src[2], src[1]);
            }
            rc = ORTE_ERR_FAILED_TO_START;
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&ack);
            return rc;
        }
        
        /* okay, so the daemon says it started up okay - get the daemon's name */
        idx = 1;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(&ack, &name, &idx, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&ack);
            return rc;
        }
        
        /* transfer the gpr compound command buffer it sent (if it did send one) and hand
         * it off for processing
         */
        OBJ_CONSTRUCT(&handoff, orte_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_dss.xfer_payload(&handoff, &ack))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&ack);
            OBJ_DESTRUCT(&handoff);
            return rc;
        }
        OBJ_DESTRUCT(&ack);  /* done with this */
        
        if (ORTE_SUCCESS != (rc = orte_gpr.process_compound_cmd(&handoff, &name))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&handoff);
            return rc;
        }
        OBJ_DESTRUCT(&handoff);  /* done with this */
    }

    return ORTE_SUCCESS;
}
