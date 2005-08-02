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

#include "include/orte_constants.h"
#include "mca/schema/schema.h"
#include "mca/schema/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/soh/base/base.h"
#include "mca/rmgr/base/base.h"

#include "support.h"

#include "class/orte_pointer_array.h"
#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static void test_cbfunc1(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc2(orte_gpr_notify_message_t *msg, void *user_tag);

int main(int argc, char **argv)
{
    int rc;
    orte_gpr_subscription_id_t id;
    orte_gpr_trigger_id_t id2;
    size_t i;
    char *tokens[5], *keys[5];
    orte_data_type_t types[5];
    orte_gpr_value_union_t value, values[5];
    
    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_gpr_quick_triggers", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      test_failure("gpr_test couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* start things up */
    orte_init();

    tokens[0] = strdup("test-token-1");
    tokens[1] = strdup("test-token-2");
    tokens[2] = NULL;
    value.i32 = 123456;
    fprintf(stderr, "quick-subscribe one value with single key\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            NULL,
                            NULL,
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(test_out, "gpr_test: subscribe of 1 key no names failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of 1 key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            NULL,
                            "orte-std-subscription-single1",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(test_out, "gpr_test: subscribe of 1 key sub name failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of 1 key sub name passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id,
                            "orte-std-trigger-single",
                            "orte-std-subscription-single2",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", test_cbfunc1, NULL))) {
        fprintf(test_out, "gpr_test: subscribe of 1 key trig and sub names failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of 1 key trig and sub names passed\n");
    }
    free(tokens[0]);
    free(tokens[1]);
    
    for (i=0; i < 4; i++) {
        asprintf(&tokens[i], "test-token-%lu", (unsigned long)i);
        asprintf(&keys[i], "test-keys-%lu", (unsigned long)i);
        types[i] = ORTE_INT16;
        values[i].i16 = i * 1000;
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
        fprintf(test_out, "gpr_test: subscribe of multi key no names failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of multi key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id,
                            NULL,
                            "orte-std-subscription-multi",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc1, NULL))) {
        fprintf(test_out, "gpr_test: subscribe of multi key sub name failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of multi key sub name passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id,
                            "orte-std-trigger-multi",
                            "orte-std-subscription-multi2",
                            ORTE_GPR_NOTIFY_VALUE_CHG,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc1, NULL))) {
        fprintf(test_out, "gpr_test: subscribe of multi key trig and sub names failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of multi key trig and sub names passed\n");
    }
    
    fprintf(stderr, "quick-define-trigger\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id2,
                            NULL,
                            ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc2, NULL))) {
        fprintf(test_out, "gpr_test: define_trigger of multi key no names failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: subscribe of multi key no names passed\n");
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id,
                            "orte-std-trigger",
                            ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS,
                            ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            4, keys, test_cbfunc2, NULL))) {
        fprintf(test_out, "gpr_test: define_trigger of multi key sub name failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(test_out, "gpr_test: define_trigger of multi key sub name passed\n");
    }
    orte_gpr.dump_subscriptions(0);
    orte_gpr.dump_triggers(0);
    orte_gpr.dump_local_subscriptions(0);
    orte_gpr.dump_local_triggers(0);
    
    fprintf(stderr, "now finalize and see if all memory cleared\n");
    orte_finalize();

    fclose( test_out );
/*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_gpr_replica failed");
    }
*/
    unlink("test_gpr_quick_triggers");

    return(0);
}

void test_cbfunc1(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 1\n");
    
    orte_gpr.dump_notify_data(data, 0);
}

void test_cbfunc2(orte_gpr_notify_message_t *msg, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 2\n");
    
    orte_gpr.dump_notify_msg(msg, 0);
}

