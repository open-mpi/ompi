/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "include/orte_types.h"

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"

#include "gpr_replica_comm.h"

int orte_gpr_replica_remote_notify(orte_process_name_t *recipient, orte_gpr_notify_id_t remote_idtag,
                 orte_gpr_notify_message_t *message)
{
    orte_buffer_t msg;
    orte_gpr_cmd_flag_t command;
    int rc;

    if (orte_gpr_replica_globals.debug) {
        ompi_output(0, "sending trigger message");
    }

    command = ORTE_GPR_NOTIFY_CMD;

    OBJ_CONSTRUCT(&msg, orte_buffer_t);

    if (ORTE_SUCCESS != (rc = orte_dps.pack(&msg, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(&msg, &remote_idtag, 1, ORTE_GPR_NOTIFY_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(&msg, &message->cnt, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if(message->cnt > 0) {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(&msg, message->data, message->cnt, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (0 > orte_rml.send_buffer(recipient, &msg, ORTE_RML_TAG_GPR_NOTIFY, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            return ORTE_ERR_COMM_FAILURE;
        }
    }

    OBJ_DESTRUCT(&msg);

    return ORTE_SUCCESS;
}
