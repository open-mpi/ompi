/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

static void orte_gpr_base_quick_print(char **output, char *type_name, char *prefix, void *src, orte_std_cntr_t src_size);

/*
 * STANDARD PRINT FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_gpr_base_std_print(char **output, char *prefix, void *src, orte_data_type_t type)
{
    /* set default result */
    *output = NULL;

    switch(type) {
        case ORTE_GPR_CMD:
            orte_gpr_base_quick_print(output, "ORTE_GPR_CMD", prefix, src, sizeof(orte_gpr_cmd_flag_t));
            break;

        case ORTE_GPR_SUBSCRIPTION_ID:
            orte_gpr_base_quick_print(output, "ORTE_GPR_SUBSCRIPTION_ID", prefix, src, sizeof(orte_gpr_subscription_id_t));
            break;

        case ORTE_GPR_TRIGGER_ID:
            orte_gpr_base_quick_print(output, "ORTE_GPR_TRIGGER_ID", prefix, src, sizeof(orte_gpr_trigger_id_t));
            break;

        case ORTE_GPR_NOTIFY_ACTION:
            orte_gpr_base_quick_print(output, "ORTE_GPR_NOTIFY_ACTION", prefix, src, sizeof(orte_gpr_notify_action_t));
            break;

        case ORTE_GPR_TRIGGER_ACTION:
            orte_gpr_base_quick_print(output, "ORTE_GPR_TRIGGER_ACTION", prefix, src, sizeof(orte_gpr_trigger_action_t));
            break;

        case ORTE_GPR_NOTIFY_MSG_TYPE:
            orte_gpr_base_quick_print(output, "ORTE_GPR_NOTIFY_MSG_TYPE", prefix, src, sizeof(orte_gpr_notify_msg_type_t));
            break;

        case ORTE_GPR_ADDR_MODE:
            orte_gpr_base_quick_print(output, "ORTE_GPR_ADDR_MODE", prefix, src, sizeof(orte_gpr_addr_mode_t));
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}

/* PRINT FUNCTIONS FOR COMPLEX TYPES */

/* KEYVAL */
int orte_gpr_base_print_keyval(char **output, char *prefix, orte_gpr_keyval_t *src, orte_data_type_t type)
{
    char *tmp, *tmp2, *pfx, *prefx;
    int rc;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (NULL == src->key) {
        asprintf(&tmp, "%sData for keyval: NULL key\n", prefx);
    } else {
        asprintf(&tmp, "%sData for keyval: Key: %s\n", prefx, src->key);
    }

    asprintf(&pfx, "%s\t", prefx);

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp2, pfx, src->value, ORTE_DATA_VALUE))) {
        ORTE_ERROR_LOG(rc);
        free(pfx);
        free(tmp);
        return rc;
    }

    asprintf(output, "%s%s\n", tmp, tmp2);
    free(pfx);
    free(tmp);
    free(tmp2);

    return ORTE_SUCCESS;
}

/* VALUE */
int orte_gpr_base_print_gpr_value(char **output, char *prefix, orte_gpr_value_t *value, orte_data_type_t type)
{
    orte_gpr_addr_mode_t addr;
    char *tmp, *tmp2, *tmp3, *pfx, *prefx;
    orte_std_cntr_t j;
    int rc;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (NULL == value->segment) {
        asprintf(&tmp, "%sValue from NULL segment name - %lu keyvals",
            prefx, (unsigned long) value->cnt);
    } else {
        asprintf(&tmp, "%sValue from segment %s with %lu keyvals",
                prefx, value->segment, (unsigned long) value->cnt);
    }

    if (NULL == value->tokens) {
            asprintf(&tmp2, "%s\n%s\tNULL tokens (wildcard)", tmp, prefx);
        free(tmp);
    } else {
        asprintf(&tmp2, "%s\n%s\t%lu Tokens returned", tmp, prefx, (unsigned long) value->num_tokens);
        free(tmp);
        for (j=0; j < value->num_tokens; j++) {
            if (NULL == value->tokens[j]) {
                asprintf(&tmp, "%s\n%s\t\tToken %lu: NULL token pointer", tmp2, prefx, (unsigned long) j);
            } else {
                asprintf(&tmp, "%s\n%s\t\tToken %lu: %s", tmp2, prefx, (unsigned long) j, value->tokens[j]);
            }
            free(tmp2);
            tmp2 = tmp;
        }
    }

    addr = value->addr_mode;
    asprintf(&tmp, "%s\n%s\tToken addressing mode:", tmp2, prefx);
    free(tmp2);

    if (0x0000 == (0x00ff & addr)) {
        asprintf(&tmp2, "%s\n%s\t\tNONE\n", tmp, prefx);
        free(tmp);
        tmp = tmp2;
    } else {
        if (ORTE_GPR_TOKENS_AND & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TOKENS_AND", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TOKENS_OR & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TOKENS_OR", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TOKENS_XAND & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TOKENS_XAND", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TOKENS_XOR & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TOKENS_XOR", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TOKENS_NOT & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TOKENS_NOT", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
    }

    asprintf(&tmp2, "%s\n%s\tKey addressing mode:", tmp, prefx);
    free(tmp);
    tmp = tmp2;

    if (0x0000 == (0xff00 & addr)) {
        asprintf(&tmp2, "%s\n%s\t\tNONE\n", tmp, prefx);
        free(tmp);
        tmp = tmp2;
    } else {
        if (ORTE_GPR_KEYS_AND & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_KEYS_AND", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_KEYS_OR & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_KEYS_OR", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_KEYS_XAND & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_KEYS_XAND", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_KEYS_XOR & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_KEYS_XOR", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_KEYS_NOT & addr) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_KEYS_NOT", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
    }

    /* indent another level for keyvals */
    asprintf(&pfx, "%s\t", prefx);
    for (j=0; j < value->cnt; j++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_print_keyval(&tmp2, pfx,
                                (orte_gpr_keyval_t*)(value->keyvals[j]), ORTE_GPR_KEYVAL))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            free(pfx);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* put results in final location */
    *output = tmp;

    return ORTE_SUCCESS;
}


/* SUBSCRIPTION */
int orte_gpr_base_print_subscription(char **output, char *prefix, orte_gpr_subscription_t *sub, orte_data_type_t type)
{
    int rc;
    char *tmp, *tmp2, *tmp3, *pfx, *prefx;
    orte_std_cntr_t j;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (NULL == sub->name) {
        asprintf(&tmp, "%sSubscription - NO NAME\tid: %lu", prefx, (unsigned long) sub->id);
    } else {
        asprintf(&tmp, "%sSubscription - Name: %s\tid: %lu", prefx, sub->name, (unsigned long) sub->id);
    }

    asprintf(&tmp2, "%s\n%s\tAction flags:", tmp, prefx);
    free(tmp);
    tmp = tmp2;

    if (ORTE_GPR_NOTIFY_NONE == sub->action) {
        asprintf(&tmp2, "%s\n%s\t\tNONE\n", tmp, prefx);
        free(tmp);
        tmp = tmp2;
    } else {
        if (ORTE_GPR_NOTIFY_VALUE_CHG_TO & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_VALUE_CHG_TO", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_VALUE_CHG_FRM & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_VALUE_CHG_FRM", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_ADD_ENTRY & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_ADD_ENTRY", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_DEL_ENTRY & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_DEL_ENTRY", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_PRE_EXISTING & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_PRE_EXISTING", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_STARTS_AFTER_TRIG & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_STARTS_AFTER_TRIG", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG & sub->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_NOTIFY_DELETE_AFTER_TRIG", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
    }

    /* indent another level for values */
    asprintf(&pfx, "%s\t", prefx);
    for (j=0; j < sub->cnt; j++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_print_gpr_value(&tmp2, pfx, (orte_gpr_value_t*)(sub->values[j]), ORTE_GPR_VALUE))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            free(pfx);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* put results in final location */
    *output = tmp;

    return ORTE_SUCCESS;
}

/* TRIGGER */
int orte_gpr_base_print_trigger(char **output, char *prefix, orte_gpr_trigger_t *trig, orte_data_type_t type)
{
    int rc;
    char *tmp, *tmp2, *tmp3, *pfx, *prefx;
    orte_std_cntr_t j;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (NULL == trig->name) {
        asprintf(&tmp, "%sTrigger - NO NAME\tid: %lu", prefx, (unsigned long) trig->id);
    } else {
        asprintf(&tmp, "%sTrigger - Name: %s\tid: %lu", prefx, trig->name, (unsigned long) trig->id);
    }

    asprintf(&tmp2, "%s\n%s\tAction flags:", tmp, prefx);
    free(tmp);
    tmp = tmp2;

    if (0x00 == trig->action) {
        asprintf(&tmp2, "%s\n%s\t\tNONE\n", tmp, prefx);
        free(tmp);
        tmp = tmp2;
    } else {
        if (ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS & trig->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TRIG_ONE_SHOT & trig->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TRIG_ONE_SHOT", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME & trig->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TRIG_ROUTE_DATA_THRU_ME", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TRIG_AT_LEVEL & trig->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TRIG_AT_LEVEL", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
        if (ORTE_GPR_TRIG_CMP_LEVELS & trig->action) {
            asprintf(&tmp2, "%s\n%s\t\tORTE_GPR_TRIG_CMP_LEVELS", tmp, prefx);
            free(tmp);
            tmp = tmp2;
        }
    }

    /* indent another level for values */
    asprintf(&pfx, "%s\t", prefx);
    for (j=0; j < trig->cnt; j++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_print_gpr_value(&tmp2, pfx, (orte_gpr_value_t*)(trig->values[j]), ORTE_GPR_VALUE))) {
            ORTE_ERROR_LOG(rc);
            free(tmp);
            free(pfx);
            return rc;
        }
        asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* put results in final location */
    *output = tmp;

    return ORTE_SUCCESS;
}

/* NOTIFY DATA */
int orte_gpr_base_print_notify_data(char **output, char *prefix, orte_gpr_notify_data_t *data, orte_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx, *prefx;
    orte_std_cntr_t i, j;
    orte_gpr_value_t **values;
    int rc;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (NULL != data->target) {
        asprintf(&tmp, "%sNotify Data: %lu values going to subscription target %s", prefx,
             (unsigned long) data->cnt, data->target);
    } else {
        asprintf(&tmp, "%sNotify Data: %lu values going to subscription num %lu", prefx,
             (unsigned long) data->cnt, (unsigned long) data->id);
    }

    values = (orte_gpr_value_t**)(data->values)->addr;
    if (0 < data->cnt) {
        /* indent another level for values */
        asprintf(&pfx, "%s\t", prefx);

        for (i=0, j=0; j < data->cnt &&
                       i < (data->values)->size; i++) {
            if (NULL != values[i]) {
                j++;
                if (ORTE_SUCCESS != (rc = orte_gpr_base_print_gpr_value(&tmp2, pfx, (orte_gpr_value_t*)values[i], ORTE_GPR_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    free(tmp);
                    return rc;
                }
                asprintf(&tmp3, "%s\n%s", tmp, tmp2);
                free(tmp2);
                tmp = tmp3;
            }
        }
    }

    /* put results in final location */
    *output = tmp;

    return ORTE_SUCCESS;
}

/* NOTIFY MSG */
int orte_gpr_base_print_notify_msg(char **output, char *prefix, orte_gpr_notify_message_t *msg, orte_data_type_t type)
{
    char *tmp, *tmp2, *tmp3, *pfx, *prefx;
    orte_std_cntr_t i, j;
    orte_gpr_notify_data_t **data;
    int rc;

    /* set default result */
    *output = NULL;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    if (ORTE_GPR_TRIGGER_MSG == msg->msg_type) {
        asprintf(&tmp, "%sTRIGGER message", prefx);
    } else if (ORTE_GPR_SUBSCRIPTION_MSG == msg->msg_type) {
        asprintf(&tmp, "%sSUBSCRIPTION message", prefx);
    }

    if (NULL == msg->target) {
        asprintf(&tmp2, "%s\n%s\tTrigger target: NULL", tmp, prefx);
    } else {
        asprintf(&tmp2, "%s\n%s\tTrigger target: %s", tmp, prefx, msg->target);
    }
    free(tmp);

    asprintf(&tmp, "%s\n%s\tTrigger id: %lu", tmp2, prefx, (unsigned long)msg->id);
    free(tmp2);

    asprintf(&tmp2, "%s\n%s\t%lu Notify data structures in message", tmp2, prefx,
             (unsigned long) msg->cnt);
    free(tmp);
    tmp = tmp2;

    if (0 < msg->cnt) {
        /* indent another level for notify data */
        asprintf(&pfx, "%s\t", prefx);

        data = (orte_gpr_notify_data_t**)(msg->data)->addr;
        for (i=0, j=0; j < msg->cnt &&
                       i < (msg->data)->size; i++) {
            if (NULL != data[i]) {
                j++;
                if (ORTE_SUCCESS != (rc = orte_gpr_base_print_notify_data(&tmp2, pfx, (orte_gpr_notify_data_t*)data[i], ORTE_GPR_NOTIFY_DATA))) {
                    ORTE_ERROR_LOG(rc);
                    free(tmp);
                    return rc;
                }
                asprintf(&tmp3, "%s\n%s", tmp, tmp2);
                free(tmp2);
                tmp = tmp3;
            }
        }
    }

    /* put results in final location */
    *output = tmp;

    return ORTE_SUCCESS;
}

static void orte_gpr_base_quick_print(char **output, char *type_name, char *prefix, void *src, orte_std_cntr_t src_size)
{
    char *prefx;
    uint8_t *ui8;
    uint16_t *ui16;
    uint32_t *ui32;
#ifdef HAVE_INT64_T
    uint64_t *ui64;
#endif

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    switch(src_size) {
        case 1:
            ui8 = (uint8_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", prefx, type_name, (int) *ui8);
            break;

        case 2:
            ui16 = (uint16_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", prefx, type_name, (int) *ui16);
            break;

        case 4:
            ui32 = (uint32_t*)src;
            asprintf(output, "%sData type: %s\tValue: %lu", prefx, type_name, (unsigned long) *ui32);
            break;

#ifdef HAVE_INT64_T
        case 8:
            ui64 = (uint64_t*)src;
            asprintf(output, "%sData type: %s\tValue: %lu", prefx, type_name, (unsigned long) *ui64);
            break;
#endif

        default:
            return;
    }

    return;
}
