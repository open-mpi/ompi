/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_types.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"

static void orte_gpr_replica_remote_send_cb(
                   int status,
                   orte_process_name_t* peer,
                   orte_buffer_t* req,
                   orte_rml_tag_t tag,
                   void* cbdata)
{
    OBJ_RELEASE(req);
    return;
}

int orte_gpr_replica_remote_notify(orte_process_name_t *recipient,
                orte_gpr_notify_message_t *message)
{
    orte_buffer_t * buffer;
    orte_gpr_cmd_flag_t command;
    int rc;

    OPAL_TRACE(3);
    
    command = ORTE_GPR_NOTIFY_CMD;

    buffer = OBJ_NEW(orte_buffer_t);
    if(NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &message, 1, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
       
    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    
    if (0 > orte_rml.send_buffer_nb(recipient, buffer, ORTE_RML_TAG_GPR_NOTIFY, 0,
                                    orte_gpr_replica_remote_send_cb, NULL)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_ERR_COMM_FAILURE;
    }
    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    return ORTE_SUCCESS;
}
