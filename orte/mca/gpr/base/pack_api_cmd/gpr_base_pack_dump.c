/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/include/orte_constants.h"
#include "orte/dps/dps.h"
#include "orte/mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_pack_dump_all(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_ALL_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}

int orte_gpr_base_pack_dump_segments(orte_buffer_t *cmd, char *segment)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_DUMP_SEGMENTS_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &segment, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_dump_triggers(orte_buffer_t *cmd, orte_gpr_trigger_id_t start)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_DUMP_TRIGGERS_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &start, 1, ORTE_GPR_TRIGGER_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_dump_subscriptions(orte_buffer_t *cmd, orte_gpr_subscription_id_t start)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_DUMP_SUBSCRIPTIONS_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &start, 1, ORTE_GPR_SUBSCRIPTION_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_dump_a_trigger(orte_buffer_t *cmd, char *name, orte_gpr_trigger_id_t id)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_DUMP_A_TRIGGER_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &name, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &id, 1, ORTE_GPR_TRIGGER_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_dump_a_subscription(orte_buffer_t *cmd, char *name,
                        orte_gpr_subscription_id_t id)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_DUMP_A_SUBSCRIPTION_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &name, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &id, 1, ORTE_GPR_SUBSCRIPTION_ID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_pack_dump_callbacks(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_CALLBACKS_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}
