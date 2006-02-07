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

#include "orte/runtime/runtime.h"

#include "orte/mca/gpr/gpr.h"

static void test_cbfunc1(orte_gpr_notify_data_t *data, void *user_tag);
static int test_cbfunc2(orte_gpr_notify_message_t *msg);

int main(int argc, char **argv)
{
    int rc;
    orte_gpr_subscription_id_t id;
    orte_gpr_trigger_id_t id2;
    size_t i;
    char *tokens[5], *keys[5];

    /* initialize system */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        fprintf(stderr, "couldn't complete init of system - error code %d\n", rc);
        exit(1);
    }
  
    tokens[0] = strdup("test-token-1");
    tokens[1] = strdup("test-token-2");
    tokens[2] = NULL;
    fprintf(stderr, "quick-subscribe one value with single key\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            NULL,
                            NULL,
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of 1 key no names failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of 1 key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            NULL,
                            "orte-std-subscription-single1",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of 1 key sub name failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of 1 key sub name passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            "orte-std-trigger-single",
                            "orte-std-subscription-single2",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of 1 key trig and sub names failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of 1 key trig and sub names passed\n");
    }
    free(tokens[0]);
    free(tokens[1]);
    
    for (i=0; i < 4; i++) {
        asprintf(&tokens[i], "test-token-%lu", (unsigned long)i);
        asprintf(&keys[i], "test-keys-%lu", (unsigned long)i);
    }
    tokens[4] = NULL;
    keys[4] = NULL;
    fprintf(stderr, "quick-subscribe one value with multiple keyvals\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id,
                            NULL,
                            NULL,
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of multi key no names failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of multi key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id,
                            NULL,
                            "orte-std-subscription-multi",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of multi key sub name failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of multi key sub name passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id,
                            "orte-std-trigger-multi",
                            "orte-std-subscription-multi2",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc1, NULL))) {
        fprintf(stderr, "gpr_test: subscribe of multi key trig and sub names failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of multi key trig and sub names passed\n");
    }
    
    fprintf(stderr, "quick-define-trigger\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id2,
                            NULL,
                            ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc2, NULL))) {
        fprintf(stderr, "gpr_test: define_trigger of multi key no names failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: subscribe of multi key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id,
                            "orte-std-trigger",
                            ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc2, NULL))) {
        fprintf(stderr, "gpr_test: define_trigger of multi key sub name failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: define_trigger of multi key sub name passed\n");
    }
    orte_gpr.dump_subscriptions(0, 0);
    orte_gpr.dump_triggers(0, 0);
    orte_gpr.dump_local_subscriptions(0);
    orte_gpr.dump_local_triggers(0);
    
    fprintf(stderr, "now finalize and see if all memory cleared\n");
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(stderr, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }
    
    return(0);
}

void test_cbfunc1(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 1\n");
    
    orte_gpr.dump_notify_data(data, 0);
}

int test_cbfunc2(orte_gpr_notify_message_t *msg)
{
    fprintf(stderr, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 2\n");
    
    orte_gpr.dump_notify_msg(msg, 0);

    return ORTE_SUCCESS;
}

