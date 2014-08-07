/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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

/*
 * includes
 */
#include "orte_config.h"


#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/error_strings.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"

int orte_grpcomm_base_xcast(orte_jobid_t job,
                            opal_buffer_t *buffer,
                            orte_rml_tag_t tag)
{
    int rc = ORTE_SUCCESS;
    opal_buffer_t *buf;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:xcast sent to job %s tag %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), (long)tag));
    
    /* if there is no message to send, then just return ok */
    if (NULL == buffer) {
        return ORTE_SUCCESS;
    }
    
    /* prep the output buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_xcast(job, buf, buffer, tag))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* send it to the HNP (could be myself) for relay */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_XCAST,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    rc = ORTE_SUCCESS;
    
CLEANUP:
    return rc;
}



int orte_grpcomm_base_pack_xcast(orte_jobid_t job,
                                 opal_buffer_t *buffer,
                                 opal_buffer_t *message,
                                 orte_rml_tag_t tag)
{
    orte_daemon_cmd_flag_t command;
    int rc;

    /* if this isn't intended for the daemon command tag, then we better
     * tell the daemon to deliver it to the procs, and what job is supposed
     * to get it - this occurs when a caller just wants to send something
     * to all the procs in a job. In that use-case, the caller doesn't know
     * anything about inserting daemon commands or what routing algo might
     * be used, so we have to help them out a little. Functions that are
     * sending commands to the daemons themselves are smart enough to know
     * what they need to do.
     */
    if (ORTE_RML_TAG_DAEMON != tag) {
        command = ORTE_DAEMON_MESSAGE_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &job, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &tag, 1, ORTE_RML_TAG))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }
    
    /* copy the payload into the new buffer - this is non-destructive, so our
     * caller is still responsible for releasing any memory in the buffer they
     * gave to us
     */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(buffer, message))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    return ORTE_SUCCESS;
}

