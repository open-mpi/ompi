/*
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
 * The Open MPI general purpose registry - unit test
 *
 */

/*
 * includes
 */

#include "orte_config.h"
#include <stdio.h>
#include <string.h>

#include "include/orte_constants.h"

#include "support.h"

#include "util/cmd_line.h"
#include "class/orte_pointer_array.h"
#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

#include "mca/errmgr/errmgr.h"
#include "mca/errmgr/base/base.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

static void test_cbfunc(orte_gpr_notify_data_t *data, void *user_tag);


int main(int argc, char **argv)
{
    ompi_cmd_line_t cmd_line;
    int rc, num_names, num_found, num_counters=6;
    int i, j, cnt, ret;
    orte_gpr_value_t *values, value, trig, *trigs;
    orte_gpr_subscription_t *subscription;
    orte_gpr_notify_id_t sub;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_ABORTED,
        ORTE_PROC_NUM_TERMINATED
    };

    test_init("test_gpr_replica_trigs");

   /*  test_out = fopen( "test_gpr_replica_out", "w+" ); */
    test_out = stderr;
    if( test_out == NULL ) {
      test_failure("gpr_test couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    OBJ_CONSTRUCT(&cmd_line, ompi_cmd_line_t);
    if (ORTE_SUCCESS != (rc = orte_init(&cmd_line, argc, argv))) {
        fprintf(test_out, "orte_init failed to start started\n");
    } else {
        fprintf(test_out, "orte_init_started\n");
    }
    
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->addr_mode = ORTE_GPR_TOKENS_OR;
    subscription->segment = strdup("test-segment");
    subscription->num_tokens = 0;
    subscription->tokens = NULL;
    subscription->num_keys = 0;
    subscription->keys = NULL;
    subscription->cbfunc = test_cbfunc;
    subscription->user_tag = NULL;
    
    fprintf(stderr, "register subscription on segment\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(ORTE_GPR_NOTIFY_ADD_ENTRY,
                                    1, &subscription,
                                    0, NULL,
                                    &sub))) {
        fprintf(test_out, "gpr_test_trigs: subscribe on seg failed with error %s\n",
                        ORTE_ERROR_NAME(rc));
        test_failure("gpr_test_trigs: subscribe on seg failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test_trigs: subscribe on seg registered\n");
    }
    
    orte_gpr.dump(0);

    /* setup some test counters */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    value.segment = strdup("test-segment");
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the segment's globals container */
    value.num_tokens = 1;
    value.cnt = num_counters;
    value.keyvals = (orte_gpr_keyval_t**)malloc(num_counters * sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < num_counters; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value.keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&value);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        value.keyvals[i]->key = strdup(keys[i]);
        value.keyvals[i]->type = ORTE_INT32;
        value.keyvals[i]->value.i32 = 0;
    }
    /* set value in keys[0] to 3 */
    value.keyvals[0]->value.i32 = 3;
    
    values = &value;
    
    fprintf(test_out, "putting counters on registry\n");
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    
    orte_gpr.dump(0);

    fprintf(test_out, "incrementing all counters\n");
    
    /* increment the counters */
    if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(&value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    
    orte_gpr.dump(0);
    
    fprintf(test_out, "decrementing all counters\n");
    
    /* decrement the counters */
    if (ORTE_SUCCESS != (rc = orte_gpr.decrement_value(&value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value);
    
    orte_gpr.dump(0);


    /* for testing the trigger, we'll just use the prior subscription setup.
     * setup the trigger information - initialize the common elements */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    trig.segment = strdup("test-segment");
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    trig.num_tokens = 1;
    trig.cnt = 2;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    trig.keyvals[0]->key = strdup(keys[0]);
    trig.keyvals[0]->type = ORTE_NULL;

    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    trig.keyvals[1]->key = strdup(keys[1]);
    trig.keyvals[1]->type = ORTE_NULL;
    
   fprintf(test_out, "setting trigger\n");
   
   trigs = &trig;
   rc = orte_gpr.subscribe(
         ORTE_GPR_TRIG_ALL_CMP,
         1, &subscription,
         1, &trigs,
         &sub);

     if(ORTE_SUCCESS != rc) {
         ORTE_ERROR_LOG(rc);
         OBJ_RELEASE(subscription);
         OBJ_DESTRUCT(&trig);
         return rc;
     }

    orte_gpr.dump(0);
    
    fprintf(test_out, "incrementing until trigger\n");
    
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
    value.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the segment's globals container */
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
    value.keyvals[0]->type = ORTE_NULL;
    
    for (i=0; i < 10; i++) {
        fprintf(test_out, "\tincrement %s\n", keys[1]);
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(&value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&value);
            return rc;
        }
    }

    orte_gpr.dump(0);
    
    fclose( test_out );
/*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_gpr_replica failed");
    }
*/
    test_finalize();

    return(0);
}

void test_cbfunc(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "TRIGGER FIRED AND RECEIVED\n");
    
/*    fprintf(test_out, "\tSegment: %s\tNumber of values: %d\n", (msg->values[0])->segment, msg->cnt);
*/
    orte_gpr.dump(0);
    
    OBJ_RELEASE(data);
}
