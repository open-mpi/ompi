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

#include "include/orte_constants.h"
#include "dps/dps.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_pack_dump_all(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_ALL_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}

int orte_gpr_base_pack_dump_segments(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_SEGMENTS_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}

int orte_gpr_base_pack_dump_triggers(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_TRIGGERS_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}

int orte_gpr_base_pack_dump_subscriptions(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_SUBSCRIPTIONS_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}

int orte_gpr_base_pack_dump_callbacks(orte_buffer_t *cmd)
{
    orte_gpr_cmd_flag_t command;

    command = ORTE_GPR_DUMP_CALLBACKS_CMD;

    return orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD);
}
