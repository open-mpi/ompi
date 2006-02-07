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
 */
/** @file:
 *
 * The Open MPI general purpose registry - unit test
 *
 */

/*
 * includes
 */

#include "orte_config.h"
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/gpr/gpr.h"

static void test_cbfunc1(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc2(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc3(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc4(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc5(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc6(orte_gpr_notify_data_t *data, void *user_tag);

static int test1(void);
static int test2(void);
static int test3(void);
static int test4(void);
static int test5(void);

int main(int argc, char **argv)
{
    int rc=0;

    /* initialize system */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        fprintf(stderr, "couldn't complete init of system - error code %d\n", rc);
        exit(1);
    }

    /* test triggers that compare two counters to each other */
    if (ORTE_SUCCESS == test1()) {
        fprintf(stderr, "triggers: compare two counters successful\n");
    } else {
        fprintf(stderr, "triggers: compare two counters failed\n");
        rc = 1;
    }

    /* test triggers that fire at a level */
    if (ORTE_SUCCESS == test2()) {
        fprintf(stderr, "triggers: trigger at level successful\n");
    } else {
        fprintf(stderr, "triggers: trigger at level failed\n");
        rc = 1;
    }

    /* test notification on value added */
    if (ORTE_SUCCESS == test3()) {
        fprintf(stderr, "triggers: notify upon value added successful\n");
    } else {
        fprintf(stderr, "triggers: notify upon value added failed\n");
        rc = 1;
    }

    /* test notification on value changed */
    if (ORTE_SUCCESS == test4()) {
        fprintf(stderr, "triggers: notify upon value changed successful\n");
    } else {
        fprintf(stderr, "triggers: notify upon value changed failed\n");
        rc = 1;
    }

    /* test notification on value changed to specific value */
    if (ORTE_SUCCESS == test5()) {
        fprintf(stderr, "triggers: notify upon value changed successful\n");
    } else {
        fprintf(stderr, "triggers: notify upon value changed failed\n");
        rc = 1;
    }

    /* cleanup and finalize */
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(stderr, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }


    return rc;
}

static int test1(void)
{
    int rc, i, k;
    orte_gpr_value_t *values, value, *val;
    orte_gpr_trigger_t trig, *trigs;
    orte_gpr_subscription_t *subscriptions[5];
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        "setpoint",
        "counter",
    };
    uint32_t ui32;
    int32_t i32;
    int16_t i16;

    /* setup a pair of counters on the registry - one is the actual
     * counter, and the other will hold the end condition when the
     * trigger(s) should fire
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup("test-container");
    value.num_tokens = 1;
    value.cnt = 2;
    value.keyvals = (orte_gpr_keyval_t**)malloc(2 * sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < 2; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value.keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&value);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        value.keyvals[i]->key = strdup(keys[i]);
        value.keyvals[i]->value = OBJ_NEW(orte_data_value_t);
        value.keyvals[i]->value->type = ORTE_UINT32;
    }
    ui32 = 0;
    orte_dss.copy(&(value.keyvals[1]->value->data), &ui32, ORTE_UINT32);
    /* set value in keys[0] to 3 */
    ui32 = 3;
    orte_dss.copy(&(value.keyvals[0]->value->data), &ui32, ORTE_UINT32);

    values = &value;

    fprintf(stderr, "putting counters on registry\n");

    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    /* put some data on the registry that the subscriptions can return
     * we'll first put several keyvals in one container that have different
     * keys, plus a few that have the same key.
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_NO_OVERWRITE |
                      ORTE_GPR_TOKENS_XAND |
                      ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.num_tokens = 2;
    value.tokens = (char**)malloc(value.num_tokens * sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(value.tokens[i]), "dummy%d", i);
    }
    value.cnt = 5;
    value.keyvals = (orte_gpr_keyval_t**)malloc(5*sizeof(orte_gpr_keyval_t*));
    for (i=0; i < 5; i++) value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
    (value.keyvals[0])->key = strdup("stupid-value-one");
    value.keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    (value.keyvals[0])->value->type = ORTE_INT32;
    i32 = 654321;
    orte_dss.copy(&(value.keyvals[0]->value->data), &i32, ORTE_INT32);
    (value.keyvals[1])->key = strdup("stupid-value-two");
    value.keyvals[1]->value = OBJ_NEW(orte_data_value_t);
    (value.keyvals[1])->value->type = ORTE_INT16;
    i16 = 128;
    orte_dss.copy(&(value.keyvals[1]->value->data), &i16, ORTE_INT16);
    for (i=2; i < 5; i++) {
        (value.keyvals[i])->key = strdup("stupid-value-multi");
        value.keyvals[i]->value = OBJ_NEW(orte_data_value_t);
        (value.keyvals[i])->value->type = ORTE_INT32;
        i32 = i * 10;
        orte_dss.copy(&(value.keyvals[i]->value->data), &i32, ORTE_INT32);
    }
    values = &value;

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    /* now put some data in a second container, some of it with
     * matching keys
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_NO_OVERWRITE |
                      ORTE_GPR_TOKENS_XAND |
                      ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.num_tokens = 2;
    value.tokens = (char**)malloc(value.num_tokens * sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(value.tokens[i]), "dummy%d", i+5);
    }
    value.cnt = 3;
    value.keyvals = (orte_gpr_keyval_t**)malloc(5*sizeof(orte_gpr_keyval_t*));
    for (i=0; i < 3; i++) value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
    (value.keyvals[0])->key = strdup("stupid-value-one");
    value.keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    (value.keyvals[0])->value->type = ORTE_INT32;
    i32 = 123456;
    orte_dss.copy(&(value.keyvals[0]->value->data), &i32, ORTE_INT32);
    (value.keyvals[1])->key = strdup("stupid-value-three");
    value.keyvals[1]->value = OBJ_NEW(orte_data_value_t);
    (value.keyvals[1])->value->type = ORTE_INT16;
    i16 = 821;
    orte_dss.copy(&(value.keyvals[1]->value->data), &i16, ORTE_INT16);
    (value.keyvals[2])->key = strdup("stupid-value-multi");
    value.keyvals[2]->value = OBJ_NEW(orte_data_value_t);
    (value.keyvals[2])->value->type = ORTE_INT32;
    i32 = 2348;
    orte_dss.copy(&(value.keyvals[2]->value->data), &i32, ORTE_INT32);
    values = &value;

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    for (k=0; k < 10; k++) {
        /* setup the subscriptions, each defining a set of data that is to be
         * returned to the corresponding callback function
         */
        for (i=0; i < 5; i++) {
            subscriptions[i] = OBJ_NEW(orte_gpr_subscription_t);
            subscriptions[i]->action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
            subscriptions[i]->cnt = 1;
            subscriptions[i]->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
            subscriptions[i]->values[0] = OBJ_NEW(orte_gpr_value_t);
            val = subscriptions[i]->values[0];
            val->addr_mode = ORTE_GPR_TOKENS_OR;
            val->segment = strdup("test-segment");
            subscriptions[i]->user_tag = NULL;
        }
        /* sub-0 asks for the stupid-value-one data from the first
         * container ONLY
         */
        val = subscriptions[0]->values[0];
        val->num_tokens = 2;
        val->tokens = (char**)malloc(2*sizeof(char*));
        for (i=0; i < 2; i++) {
            asprintf(&(val->tokens[i]), "dummy%d", i);
        }
        val->cnt = 1;
        val->keyvals =(orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
        val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[0]->key = strdup("stupid-value-one");
        subscriptions[0]->cbfunc = test_cbfunc1;

        /* sub-1 asks for the stupid-value-one data from ALL containers
         */
        val = subscriptions[1]->values[0];
        val->num_tokens = 0;
        val->tokens = NULL;
        val->cnt = 1;
        val->keyvals =(orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
        val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[0]->key = strdup("stupid-value-one");
        subscriptions[1]->cbfunc = test_cbfunc2;

        /* sub-2 asks for the stupid-value-multi data from the first
         * container ONLY
         */
        val = subscriptions[2]->values[0];
        val->num_tokens = 2;
        val->tokens = (char**)malloc(2*sizeof(char*));
        for (i=0; i < 2; i++) {
            asprintf(&(val->tokens[i]), "dummy%d", i);
        }
        val->cnt = 1;
        val->keyvals =(orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
        val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[0]->key = strdup("stupid-value-multi");
        subscriptions[2]->cbfunc = test_cbfunc3;

        /* sub-3 asks for the stupid-value-three data from ALL containers */
        val = subscriptions[3]->values[0];
        val->num_tokens = 0;
        val->tokens = NULL;
        val->cnt = 1;
        val->keyvals =(orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
        val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[0]->key = strdup("stupid-value-three");
        subscriptions[3]->cbfunc = test_cbfunc4;

        /* sub-4 asks for ALL data from ALL containers */
        val = subscriptions[4]->values[0];
        val->num_tokens = 0;
        val->tokens = NULL;
        val->cnt = 0;
        val->keyvals = NULL;
        subscriptions[4]->cbfunc = test_cbfunc5;

        /* setup the trigger information - initialize the common elements */
        OBJ_CONSTRUCT(&trig, orte_gpr_trigger_t);
        trig.action = ORTE_GPR_TRIG_ALL_CMP;
        trig.cnt = 1;
        trig.values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
        trig.values[0] = OBJ_NEW(orte_gpr_value_t);
        val = trig.values[0];
        val->addr_mode = ORTE_GPR_TOKENS_XAND;
        val->segment = strdup("test-segment");
        val->tokens = (char**)malloc(sizeof(char*));
        if (NULL == val->tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            for (i=0; i < 5; i++) OBJ_RELEASE(subscriptions[i]);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        val->tokens[0] = strdup("test-container");
        val->num_tokens = 1;
        val->cnt = 2;
        val->keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
        val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[0]->key = strdup(keys[0]);

        val->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
        val->keyvals[1]->key = strdup(keys[1]);

       fprintf(stderr, "setting trigger\n");

       trigs = &trig;

       /* enter things as three different subscriptions */
       rc = orte_gpr.subscribe(
             2, subscriptions,
             1, &trigs);


       rc = orte_gpr.subscribe(
             2, &(subscriptions[2]),
             1, &trigs);

       rc = orte_gpr.subscribe(
             1, &(subscriptions[4]),
             1, &trigs);

        for (i=0; i < 5; i++) OBJ_RELEASE(subscriptions[i]);
        OBJ_DESTRUCT(&trig);

        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    orte_gpr.dump_triggers(0, 0);
    return ORTE_SUCCESS;

    fprintf(stderr, "incrementing until trigger\n");
    /* increment the value in keys[1] until the trig fires */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup("test-container");
    value.num_tokens = 1;
    value.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.cnt = 1;
    value.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.keyvals[0]->key = strdup(keys[1]);

    for (i=0; i < 10; i++) {
        fprintf(stderr, "\tincrement %s\n", keys[1]);
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(&value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&value);
            return rc;
        }
    }

    orte_gpr.dump_all(0);
    OBJ_DESTRUCT(&value);

    return ORTE_SUCCESS;
}

int test2(void)
{
    int rc, i;
    orte_gpr_value_t *values, value;
    orte_gpr_trigger_t trig, *trigs;
    orte_gpr_subscription_t *subscription;
    uint32_t ui32;

    /* setup a counter on the registry that the trigger will later refer
     * to when defining the level at which to fire
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup("test-level-trigger");
    value.num_tokens = 1;
    value.cnt = 1;
    value.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.keyvals[0]->key = strdup("level-counter");
    value.keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    value.keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 0;
    orte_dss.copy(&(value.keyvals[0]->value->data), &ui32, ORTE_UINT32);

    values = &value;

    fprintf(stderr, "putting level test counter on registry\n");

    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */

    /* setup a subscription that defines a set of data that is to be
     * returned to the corresponding callback function
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    subscription->cnt = 1;
    subscription->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    subscription->values[0] = OBJ_NEW(orte_gpr_value_t);
    values = subscription->values[0];
    values->addr_mode = ORTE_GPR_TOKENS_OR;
    values->segment = strdup("test-segment");
    subscription->user_tag = NULL;
    /* ask for the stupid-value-one data from the first
     * container ONLY
     */
    values->num_tokens = 2;
    values->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(values->tokens[i]), "dummy%d", i);
    }
    values->cnt = 1;
    values->keyvals =(orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    values->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    values->keyvals[0]->key = strdup("stupid-value-one");
    subscription->cbfunc = test_cbfunc6;

    /* setup the trigger information - want trigger to fire when
     * a specific counter reaches a specified value
     */
    OBJ_CONSTRUCT(&trig, orte_gpr_trigger_t);
    trig.action = ORTE_GPR_TRIG_ALL_AT;
    trig.cnt = 1;
    trig.values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    trig.values[0] = OBJ_NEW(orte_gpr_value_t);
    values = trig.values[0];
    values->addr_mode = ORTE_GPR_TOKENS_XAND;
    values->segment = strdup("test-segment");
    values->tokens = (char**)malloc(sizeof(char*));
    if (NULL == values->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(subscription);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    values->tokens[0] = strdup("test-level-trigger");
    values->num_tokens = 1;
    values->cnt = 1;
    values->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    values->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    values->keyvals[0]->key = strdup("level-counter");
    values->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    values->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 2;
    orte_dss.copy(&(values->keyvals[0]->value->data), &ui32, ORTE_UINT32);

   fprintf(stderr, "setting level trigger\n");

   trigs = &trig;

   /* enter subscription */
   rc = orte_gpr.subscribe(
         1, &subscription,
         1, &trigs);

    orte_gpr.dump_triggers(0, 0);

    /* cleanup */
    OBJ_RELEASE(subscription);
    OBJ_DESTRUCT(&trig);

    fprintf(stderr, "incrementing until level trigger\n");
    /* increment the value in the counter until the trig fires */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup("test-level-trigger");
    value.num_tokens = 1;
    value.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.cnt = 1;
    value.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.keyvals[0]->key = strdup("level-counter");

    for (i=0; i < 10; i++) {
        fprintf(stderr, "\tincrement level-counter\n");
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(&value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&value);
            return rc;
        }
    }

    orte_gpr.dump_all(0);
    OBJ_DESTRUCT(&value);

    return ORTE_SUCCESS;
}


int test3(void)
{
    int rc;
    size_t i;
    orte_gpr_value_t value, *val;
    orte_gpr_subscription_t *subscription;
    uint32_t ui32;
    int32_t i32;

    /* put something on the registry to start */
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->segment = strdup("test-segment");
    val->num_tokens = 10;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < val->num_tokens; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    val->cnt = 20;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    for (i=0; i<val->cnt; i++) {
        val->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&((val->keyvals[i])->key), "stupid-test-%lu",
                 (unsigned long) i);
        val->keyvals[i]->value = OBJ_NEW(orte_data_value_t);
        (val->keyvals[i])->value->type = ORTE_UINT32;
        ui32 = i;
        orte_dss.copy(&(val->keyvals[i]->value->data), &ui32, ORTE_UINT32);
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "put failed with error code %d\n", rc);
        return rc;
    }
    OBJ_RELEASE(val);

    /* setup a subscription on one of the containers
     * that notifies callback 1 if something is added
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->action = ORTE_GPR_NOTIFY_ADD_ENTRY;
    subscription->cnt = 1;
    subscription->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    subscription->values[0] = OBJ_NEW(orte_gpr_value_t);
    val = subscription->values[0];
    val->addr_mode = ORTE_GPR_TOKENS_OR;
    val->segment = strdup("test-segment");
    subscription->user_tag = NULL;
    /* monitor the dummy-sub-xx container only
     */
    val->num_tokens = 2;
    val->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    /* get notified when anything is added */
    val->cnt = 0;
    val->keyvals = NULL;

    /* send notification to callback 1 */
    subscription->cbfunc = test_cbfunc1;

    /* enter subscription */
    rc = orte_gpr.subscribe(
         1, &subscription,
         0, NULL);

    orte_gpr.dump_triggers(0, 0);

    /* cleanup */
    OBJ_RELEASE(subscription);


    /* add something with two keyvals to the container */

    fprintf(stderr, "adding something - should trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-0");
    val->num_tokens = 1;
    val->cnt = 2;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("test-notify-add");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_NULL;
    val->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[1]->key = strdup("test-notify-add-2");
    val->keyvals[1]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[1]->value->type = ORTE_INT32;
    i32 = 12345;
    orte_dss.copy(&(val->keyvals[1]->value->data), &i32, ORTE_INT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    fprintf(stderr, "adding something - should NOT trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-10");
    val->num_tokens = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->cnt = 1;
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("test-notify-add-no-fire");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_NULL;

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    return ORTE_SUCCESS;
}


int test4(void)
{
    int rc;
    size_t i;
    orte_gpr_value_t value, *val;
    orte_gpr_subscription_t *subscription;
    uint32_t ui32;

    /* put something on the registry to start */
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->segment = strdup("test-segment-4");
    val->num_tokens = 10;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < val->num_tokens; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    val->cnt = 20;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    for (i=0; i<val->cnt; i++) {
        val->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&((val->keyvals[i])->key), "stupid-test-%lu",
                 (unsigned long) i);
        val->keyvals[i]->value = OBJ_NEW(orte_data_value_t);
        (val->keyvals[i])->value->type = ORTE_UINT32;
        ui32 = i;
        orte_dss.copy(&(val->keyvals[i]->value->data), &ui32, ORTE_UINT32);
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "put failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    }
    OBJ_RELEASE(val);


    /* setup a subscription on one of the containers
     * that notifies callback 1 if something is changed
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->action = ORTE_GPR_NOTIFY_VALUE_CHG;
    subscription->cnt = 1;
    subscription->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    subscription->values[0] = OBJ_NEW(orte_gpr_value_t);
    val = subscription->values[0];
    val->addr_mode = ORTE_GPR_TOKENS_OR;
    val->segment = strdup("test-segment-2");
    subscription->user_tag = NULL;
    /* monitor the dummy-sub-xx container only
     */
    val->num_tokens = 2;
    val->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    /* get notified when any value is changed */
    val->cnt = 0;
    val->keyvals = NULL;

    /* send notification to callback 1 */
    subscription->cbfunc = test_cbfunc1;

    /* enter subscription */
    rc = orte_gpr.subscribe(
         1, &subscription,
         0, NULL);

    orte_gpr.dump_triggers(0, 0);

    /* cleanup */
    OBJ_RELEASE(subscription);


    /* change something on the container */

    fprintf(stderr, "changing something - should trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment-2");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-0");
    val->num_tokens = 1;
    val->cnt = 2;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("stupid-test-0");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 12345;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);
    val->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[1]->key = strdup("stupid-test-5");
    val->keyvals[1]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[1]->value->type = ORTE_UINT32;
    ui32 = 6789;
    orte_dss.copy(&(val->keyvals[1]->value->data), &ui32, ORTE_UINT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    fprintf(stderr, "changing something else - should NOT trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment-2");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-1");
    val->num_tokens = 1;
    val->cnt = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("stupid-test-1");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 6789;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    return ORTE_SUCCESS;
}

int test5(void)
{
    int rc;
    size_t i;
    orte_gpr_value_t value, *val;
    orte_gpr_subscription_t *subscription;
    uint32_t ui32;

    /* put something on the registry to start */
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->segment = strdup("test-segment-5");
    val->num_tokens = 10;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < val->num_tokens; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    val->cnt = 20;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    for (i=0; i<val->cnt; i++) {
        val->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&((val->keyvals[i])->key), "stupid-test-%lu",
                 (unsigned long) i);
        val->keyvals[i]->value = OBJ_NEW(orte_data_value_t);
        (val->keyvals[i])->value->type = ORTE_UINT32;
        ui32 = i;
        orte_dss.copy(&(val->keyvals[i]->value->data), &ui32, ORTE_UINT32);
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "put failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    }
    OBJ_RELEASE(val);


    /* setup a subscription on one of the containers
     * that notifies callback 1 if something is changed
     * to a specific value
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->action = ORTE_GPR_NOTIFY_VALUE_CHG_TO;
    subscription->cnt = 1;
    subscription->values = (orte_gpr_value_t**)malloc(sizeof(orte_gpr_value_t*));
    subscription->values[0] = OBJ_NEW(orte_gpr_value_t);
    val = subscription->values[0];
    val->addr_mode = ORTE_GPR_TOKENS_OR;
    val->segment = strdup("test-segment-5");
    subscription->user_tag = NULL;
    /* monitor the dummy-sub-xx container only
     */
    val->num_tokens = 2;
    val->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(val->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    /* get notified when stupid-test-0 is changed to 654321 */
    val->cnt = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    val->keyvals[0]->key = strdup("stupid-test-0");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 654321;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);


    /* send notification to callback 1 */
    subscription->cbfunc = test_cbfunc1;

    /* enter subscription */
    rc = orte_gpr.subscribe(
         1, &subscription,
         0, NULL);

    orte_gpr.dump_triggers(0, 0);

    /* cleanup */
    OBJ_RELEASE(subscription);


    /* change the value to its trigger value */

    fprintf(stderr, "changing value to specified trigger - should trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment-2");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-0");
    val->num_tokens = 1;
    val->cnt = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("stupid-test-0");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 654321;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    fprintf(stderr, "changing something else - should NOT trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment-2");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-1");
    val->num_tokens = 1;
    val->cnt = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("stupid-test-1");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 6789;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    /* change the value to something other than its trigger value */

    fprintf(stderr, "changing value to non-trigger value - should NOT trigger\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    val->segment = strdup("test-segment-2");
    val->tokens = (char**)malloc(sizeof(char*));
    if (NULL == val->tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->tokens[0] = strdup("dummy-sub-0");
    val->num_tokens = 1;
    val->cnt = 1;
    val->keyvals = (orte_gpr_keyval_t**)malloc(val->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == val->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == val->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    val->keyvals[0]->key = strdup("stupid-test-0");
    val->keyvals[0]->value = OBJ_NEW(orte_data_value_t);
    val->keyvals[0]->value->type = ORTE_UINT32;
    ui32 = 12345;
    orte_dss.copy(&(val->keyvals[0]->value->data), &ui32, ORTE_UINT32);

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    orte_gpr.dump_all(0);
    OBJ_RELEASE(val);

    return ORTE_SUCCESS;
}


void test_cbfunc1(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 1\n");

    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc2(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 2\n");

    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc3(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 3\n");

    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc4(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 4\n");

    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc5(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 5\n");

    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc6(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 6\n");

    orte_gpr.dump_notify_data(data, 0);
}
