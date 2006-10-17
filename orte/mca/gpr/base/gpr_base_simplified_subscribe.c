/* -*- C -*-
 *
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
/** @file
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

int orte_gpr_base_subscribe_1(orte_gpr_subscription_id_t *id,
                              char *trig_name,
                              char *sub_name,
                              orte_gpr_notify_action_t action,
                              orte_gpr_addr_mode_t addr_mode,
                              char *segment,
                              char **tokens,
                              char *key,
                              orte_gpr_notify_cb_fn_t cbfunc,
                              void *user_tag)
{
    orte_gpr_value_t *values;
    orte_gpr_keyval_t *keyval;
    orte_gpr_value_t value = ORTE_GPR_VALUE_EMPTY;
    orte_gpr_subscription_t *subs;
    orte_gpr_subscription_t sub = ORTE_GPR_SUBSCRIPTION_EMPTY;
    orte_gpr_trigger_t *trigs;
    orte_gpr_trigger_t trig = ORTE_GPR_TRIGGER_EMPTY;
    orte_std_cntr_t i;
    int rc;

    OPAL_TRACE(1);

    /* assemble the subscription object */
    subs = &sub;
    sub.name = sub_name;
    sub.action = action;
    sub.cnt = 1;
    values = &value;
    sub.values = &values;
    sub.cbfunc = cbfunc;
    sub.user_tag = user_tag;

    value.addr_mode = addr_mode;
    value.segment = segment;
    value.cnt = 1;
    value.keyvals = &keyval;

    value.tokens = tokens;
    /* must count the number of tokens */
    value.num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            (value.num_tokens)++;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&keyval, key,
                                                          ORTE_UNDEF, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* send the subscription */
    if (NULL == trig_name) { /* no trigger provided */
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &subs, 0, NULL))) {
            ORTE_ERROR_LOG(rc);
        }

    } else {
        trigs = &trig;
        trig.name = trig_name;
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &subs, 1, &trigs))) {
            ORTE_ERROR_LOG(rc);
        }

    }

    /* cleanup */
    OBJ_RELEASE(keyval);

    /* return the subscription id */
    *id = sub.id;

    return rc;
}


int orte_gpr_base_subscribe_N(orte_gpr_subscription_id_t *id,
                              char *trig_name,
                              char *sub_name,
                              orte_gpr_notify_action_t action,
                              orte_gpr_addr_mode_t addr_mode,
                              char *segment,
                              char **tokens,
                              orte_std_cntr_t n,
                              char **keys,
                              orte_gpr_notify_cb_fn_t cbfunc,
                              void *user_tag)
{
    orte_gpr_subscription_t *sub;
    orte_gpr_trigger_t *trig;
    orte_std_cntr_t i, num_tokens;
    int rc;

    OPAL_TRACE(1);

    /* assemble the subscription object */
    sub = OBJ_NEW(orte_gpr_subscription_t);
    if (NULL == sub) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (NULL != sub_name) {
        sub->name = strdup(sub_name);
    }
    sub->action = action;
    sub->cnt = 1;
    sub->cbfunc = cbfunc;
    sub->user_tag = user_tag;

    /* must count the number of tokens */
    num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            num_tokens++;
        }
    }

    /* create the value object */
    sub->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    if (NULL == sub->values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_value(&(sub->values[0]), addr_mode, segment, n, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(sub);
        return rc;
    }

    for (i=0; i < n; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&(sub->values[0]->keyvals[i]), keys[i], ORTE_UNDEF, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(sub);
            return rc;
        }
    }

    /* copy the tokens */
    for (i=0; i < sub->values[0]->num_tokens; i++) {
        sub->values[0]->tokens[i] = strdup(tokens[i]);
    }

    /* send the subscription */
    if (NULL == trig_name) { /* no trigger provided */
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &sub, 0, NULL))) {
            ORTE_ERROR_LOG(rc);
        }

    } else {
        trig = OBJ_NEW(orte_gpr_trigger_t);
        if (NULL == trig) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(sub);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig->name = strdup(trig_name);
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &sub, 1, &trig))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_RELEASE(trig);
    }

    /* return the subscription id */
    *id = sub->id;

    /* clean up memory */
    OBJ_RELEASE(sub);

    return rc;
}


int orte_gpr_base_define_trigger(orte_gpr_trigger_id_t *id,
                                 char *trig_name,
                                 orte_gpr_trigger_action_t action,
                                 orte_gpr_addr_mode_t addr_mode,
                                 char *segment,
                                 char **tokens,
                                 orte_std_cntr_t n,
                                 char **keys,
                                 orte_gpr_trigger_cb_fn_t cbfunc,
                                 void *user_tag)
{
    orte_gpr_trigger_t *trig;
    orte_std_cntr_t i, num_tokens;
    int rc;

    OPAL_TRACE(1);

    /* check for error - this function can only be used to define triggers
     * that compare their values to each other. It cannot be used to define
     * triggers that fire when reaching a specified value as there is no
     * way to specify a trigger level within this API
     */
    if (ORTE_GPR_TRIG_AT_LEVEL & action) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* assemble the trigger object */
    trig = OBJ_NEW(orte_gpr_trigger_t);
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (NULL != trig_name) {
        trig->name = strdup(trig_name);
    }
    trig->action = action;
    trig->cnt = 1;
    trig->cbfunc = cbfunc;
    trig->user_tag = user_tag;

    /* must count the number of tokens */
    num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            num_tokens++;
        }
    }

    /* create the value object */
    trig->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    if (NULL == trig->values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_value(&(trig->values[0]), addr_mode, segment, n, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(trig);
        return rc;
    }

    for (i=0; i < n; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&(trig->values[0]->keyvals[i]), keys[i], ORTE_UNDEF, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(trig);
            return rc;
        }
    }

    for (i=0; i < trig->values[0]->num_tokens; i++) {
        trig->values[0]->tokens[i] = strdup(tokens[i]);
    }

    /* send the subscription */
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(0, NULL, 1, &trig))) {
        ORTE_ERROR_LOG(rc);
    }

    /* return the subscription id */
    *id = trig->id;

    /* clean up memory */
    OBJ_RELEASE(trig);

    return rc;
}

int orte_gpr_base_define_trigger_level(orte_gpr_trigger_id_t *id,
                                 char *trig_name,
                                 orte_gpr_trigger_action_t action,
                                 orte_gpr_addr_mode_t addr_mode,
                                 char *segment,
                                 char **tokens,
                                 orte_std_cntr_t n,
                                 char **keys,
                                 orte_std_cntr_t *levels,
                                 orte_gpr_trigger_cb_fn_t cbfunc,
                                 void *user_tag)
{
    orte_gpr_trigger_t *trig;
    orte_std_cntr_t i, num_tokens;
    int rc;

    OPAL_TRACE(1);

    /* check for error - this function can only be used to define triggers
     * that fire at a specified level. It cannot be used to define
     * triggers that compare their values to each other
     */
    if (ORTE_GPR_TRIG_CMP_LEVELS & action || NULL == trig_name) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* assemble the trigger object */
    trig = OBJ_NEW(orte_gpr_trigger_t);
    if (NULL == trig) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != trig_name) {
        trig->name = strdup(trig_name);
    }
    trig->action = action;
    trig->cnt = 1;
    trig->cbfunc = cbfunc;
    trig->user_tag = user_tag;

    /* must count the number of tokens */
    num_tokens = 0;
    if (NULL != tokens) {
        for (i=0; NULL != tokens[i]; i++) {
            num_tokens++;
        }
    }

    /* create the value object */
    trig->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    if (NULL == trig->values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_create_value(&(trig->values[0]), addr_mode, segment, n, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(trig);
        return rc;
    }

    for (i=0; i < n; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_create_keyval(&(trig->values[0]->keyvals[i]), keys[i], ORTE_STD_CNTR, &(levels[i])))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(trig);
            return rc;
        }
    }

    for (i=0; i < trig->values[0]->num_tokens; i++) {
        trig->values[0]->tokens[i] = strdup(tokens[i]);
    }

    /* send the subscription */
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(0, NULL, 1, &trig))) {
        ORTE_ERROR_LOG(rc);
    }

    /* return the subscription id */
    *id = trig->id;

    /* clean up memory */
    OBJ_RELEASE(trig);

    return rc;
}
