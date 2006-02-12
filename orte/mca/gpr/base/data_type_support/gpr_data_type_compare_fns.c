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

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

/*
 * NUMERIC COMPARE FUNCTIONS
 */
int orte_gpr_base_compare_cmd(orte_gpr_cmd_flag_t *value1,
                              orte_gpr_cmd_flag_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_subscription_id(orte_gpr_subscription_id_t *value1,
                              orte_gpr_subscription_id_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_trigger_id(orte_gpr_trigger_id_t *value1,
                              orte_gpr_trigger_id_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_notify_action(orte_gpr_notify_action_t *value1,
                              orte_gpr_notify_action_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_trigger_action(orte_gpr_trigger_action_t *value1,
                              orte_gpr_trigger_action_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_notify_msg_type(orte_gpr_notify_msg_type_t *value1,
                              orte_gpr_notify_msg_type_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_gpr_base_compare_addr_mode(orte_gpr_addr_mode_t *value1,
                              orte_gpr_addr_mode_t *value2,
                              orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

/* NON-NUMERIC COMPARE FUNCTIONS */


int orte_gpr_base_compare_keyval(orte_gpr_keyval_t *value1,
                              orte_gpr_keyval_t *value2,
                              orte_data_type_t type)
{
    int rc;

    /* check to see if the keys are the same */
    if (0 != (rc = strcmp(value1->key, value2->key))) {
        if (0 < rc) return ORTE_VALUE1_GREATER;
        return ORTE_VALUE2_GREATER;
    }

    /* okay, keys are the same - compare the values using their native comparators */
    return orte_dss.compare(value1->value, value2->value, ORTE_DATA_VALUE);

}

int orte_gpr_base_compare_gpr_value(orte_gpr_value_t *value1,
                              orte_gpr_value_t *value2,
                              orte_data_type_t type)
{
    /* no real way to do this right now */
    ORTE_ERROR_LOG(ORTE_ERR_COMPARE_FAILURE);
    return ORTE_EQUAL;
}

int orte_gpr_base_compare_subscription(orte_gpr_subscription_t *value1,
                              orte_gpr_subscription_t *value2,
                              orte_data_type_t type)
{
    /* no real way to do this right now */
    ORTE_ERROR_LOG(ORTE_ERR_COMPARE_FAILURE);
    return ORTE_EQUAL;
}

int orte_gpr_base_compare_trigger(orte_gpr_trigger_t *value1,
                              orte_gpr_trigger_t *value2,
                              orte_data_type_t type)
{
    /* no real way to do this right now */
    ORTE_ERROR_LOG(ORTE_ERR_COMPARE_FAILURE);
    return ORTE_EQUAL;
}

int orte_gpr_base_compare_notify_data(orte_gpr_notify_data_t *value1,
                              orte_gpr_notify_data_t *value2,
                              orte_data_type_t type)
{
    /* no real way to do this right now */
    ORTE_ERROR_LOG(ORTE_ERR_COMPARE_FAILURE);
    return ORTE_EQUAL;
}

int orte_gpr_base_compare_notify_msg(orte_gpr_notify_message_t *value1,
                              orte_gpr_notify_message_t *value2,
                              orte_data_type_t type)
{
    /* no real way to do this right now */
    ORTE_ERROR_LOG(ORTE_ERR_COMPARE_FAILURE);
    return ORTE_EQUAL;
}
