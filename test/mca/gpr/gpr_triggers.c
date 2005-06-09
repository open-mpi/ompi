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

#include "support.h"
#include "components.h"

#include "class/orte_pointer_array.h"
#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/malloc.h"
#include "util/output.h"

#include "mca/errmgr/errmgr.h"
#include "mca/errmgr/base/base.h"
#include "mca/schema/schema.h"
#include "mca/schema/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/soh/base/base.h"
#include "mca/rmgr/base/base.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

/* GPR module under test */
static orte_gpr_base_module_t *gpr_module = NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

static void test_cbfunc1(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc2(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc3(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc4(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc5(orte_gpr_notify_data_t *data, void *user_tag);
static void test_cbfunc6(orte_gpr_notify_data_t *data, void *user_tag);

static int test1(void);
static int test2(void);
static int test3(void);


int main(int argc, char **argv)
{
    int rc=0;
    test_component_handle_t handle;
    mca_gpr_base_component_t *gpr_component = NULL;
    bool allow, have;
    int priority;
    
    test_init("test_gpr_replica_trigs");

    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_gpr_replica_out", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      test_failure("gpr_test couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* ENSURE THE REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);
    
    /* Open up the output streams */
    if (!ompi_output_init()) {
        exit(1);
    }

    /* 
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    ompi_malloc_init();

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != orte_sys_info()) {
        exit(1);
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != orte_proc_info()) {
        exit(1);
    }
    
    orte_process_info.seed = true;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    /* startup the dps */
    if (OMPI_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* startup the name services */
    if (OMPI_SUCCESS == orte_ns_base_open()) {
        fprintf(test_out, "NS started\n");
    } else {
        fprintf(test_out, "NS could not start\n");
        exit (1);
    }

    /* startup the soh */
    if (OMPI_SUCCESS == orte_soh_base_open()) {
        fprintf(test_out, "SOH started\n");
    } else {
        fprintf(test_out, "SOH could not start\n");
        exit (1);
    }

    /* startup the rmgr */
    if (OMPI_SUCCESS == orte_rmgr_base_open()) {
        fprintf(test_out, "RMGR started\n");
    } else {
        fprintf(test_out, "RMGR could not start\n");
        exit (1);
    }

    /* startup the schema */
    if (OMPI_SUCCESS == orte_schema_base_open()) {
        fprintf(test_out, "SCHEMA started\n");
    } else {
        fprintf(test_out, "SCHEMA could not start\n");
        exit (1);
    }

    /* Open the gpr replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("gpr", "replica", &handle, 
                            (mca_base_component_t**) &gpr_component) ||
        NULL == gpr_component) {
        fprintf(test_out, "Could not open replica\n");
        exit(1);
    }
    gpr_module = gpr_component->gpr_init(&allow, &have, &priority);
    if (NULL == gpr_module) {
        fprintf(test_out, "replica component did not return a module\n");
        exit(1);
    }
                  
    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }
    
    if (ORTE_SUCCESS == orte_errmgr_base_open()) {
        fprintf(test_out, "error mgr started\n");
    } else {
        fprintf(test_out, "error mgr could not start\n");
        exit (1);
    }
    
//    /* test triggers that compare two counters to each other */
//    if (ORTE_SUCCESS == test1()) {
//        fprintf(test_out, "triggers: compare two counters successful\n");
//    } else {
//        fprintf(test_out, "triggers: compare two counters failed\n");
//        rc = 1;
//    }
//    
//    /* test triggers that fire at a level */
//    if (ORTE_SUCCESS == test2()) {
//        fprintf(test_out, "triggers: trigger at level successful\n");
//    } else {
//        fprintf(test_out, "triggers: trigger at level failed\n");
//        rc = 1;
//    }
//    
    /* test notification on value added */
    if (ORTE_SUCCESS == test3()) {
        fprintf(test_out, "triggers: notify upon value added successful\n");
    } else {
        fprintf(test_out, "triggers: notify upon value added failed\n");
        rc = 1;
    }
    
    /* cleanup and finalize */
    orte_dps_close();
    orte_gpr_base_close();
    orte_sys_info_finalize();
    orte_proc_info_finalize();
    mca_base_close();
    ompi_malloc_finalize();
    ompi_output_finalize();
    ompi_class_finalize();

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
    unlink("test_gpr_replica_out");

    return rc;
}

static int test1(void) 
{
    int rc, i, k;
    orte_gpr_value_t *values, value, trig, *trigs;
    orte_gpr_subscription_t *subscriptions[5];
    orte_gpr_notify_id_t sub[5];
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        "setpoint",
        "counter",
    };

    /* setup a pair of counters on the registry - one is the actual
     * counter, and the other will hold the end condition when the
     * trigger(s) should fire
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
        value.keyvals[i]->type = ORTE_UINT32;
        value.keyvals[i]->value.ui32 = 0;
    }
    /* set value in keys[0] to 3 */
    value.keyvals[0]->value.ui32 = 3;
    
    values = &value;
    
    fprintf(test_out, "putting counters on registry\n");
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &values))) {
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
    (value.keyvals[0])->type = ORTE_INT32;
    (value.keyvals[0])->value.i32 = 654321;
    (value.keyvals[1])->key = strdup("stupid-value-two");
    (value.keyvals[1])->type = ORTE_INT16;
    (value.keyvals[1])->value.i16 = 128;
    for (i=2; i < 5; i++) {
        (value.keyvals[i])->key = strdup("stupid-value-multi");
        (value.keyvals[i])->type = ORTE_INT32;
        (value.keyvals[i])->value.i32 = i * 10;
    }
    values = &value;
    
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &values))) {
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
    (value.keyvals[0])->type = ORTE_INT32;
    (value.keyvals[0])->value.i32 = 123456;
    (value.keyvals[1])->key = strdup("stupid-value-three");
    (value.keyvals[1])->type = ORTE_INT16;
    (value.keyvals[1])->value.i16 = 821;
    (value.keyvals[2])->key = strdup("stupid-value-multi");
    (value.keyvals[2])->type = ORTE_INT32;
    (value.keyvals[2])->value.i32 = 2348;
    values = &value;
    
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &values))) {
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
            subscriptions[i]->addr_mode = ORTE_GPR_TOKENS_OR;
            subscriptions[i]->segment = strdup("test-segment");
            subscriptions[i]->user_tag = NULL;
        }
        /* sub-0 asks for the stupid-value-one data from the first
         * container ONLY
         */
        subscriptions[0]->num_tokens = 2;
        subscriptions[0]->tokens = (char**)malloc(2*sizeof(char*));
        for (i=0; i < 2; i++) {
            asprintf(&(subscriptions[0]->tokens[i]), "dummy%d", i);
        }
        subscriptions[0]->num_keys = 1;
        subscriptions[0]->keys =(char**)malloc(sizeof(char*));
        subscriptions[0]->keys[0] = strdup("stupid-value-one");
        subscriptions[0]->cbfunc = test_cbfunc1;
    
        /* sub-1 asks for the stupid-value-one data from ALL containers
         */
        subscriptions[1]->num_tokens = 0;
        subscriptions[1]->tokens = NULL;
        subscriptions[1]->num_keys = 1;
        subscriptions[1]->keys =(char**)malloc(sizeof(char*));
        subscriptions[1]->keys[0] = strdup("stupid-value-one");
        subscriptions[1]->cbfunc = test_cbfunc2;
    
        /* sub-2 asks for the stupid-value-multi data from the first
         * container ONLY
         */
        subscriptions[2]->num_tokens = 2;
        subscriptions[2]->tokens = (char**)malloc(2*sizeof(char*));
        for (i=0; i < 2; i++) {
            asprintf(&(subscriptions[2]->tokens[i]), "dummy%d", i);
        }
        subscriptions[2]->num_keys = 1;
        subscriptions[2]->keys =(char**)malloc(sizeof(char*));
        subscriptions[2]->keys[0] = strdup("stupid-value-multi");
        subscriptions[2]->cbfunc = test_cbfunc3;
    
        /* sub-3 asks for the stupid-value-three data from ALL containers */
        subscriptions[3]->num_tokens = 0;
        subscriptions[3]->tokens = NULL;
        subscriptions[3]->num_keys = 1;
        subscriptions[3]->keys =(char**)malloc(sizeof(char*));
        subscriptions[3]->keys[0] = strdup("stupid-value-three");
        subscriptions[3]->cbfunc = test_cbfunc4;
    
        /* sub-4 asks for ALL data from ALL containers */
        subscriptions[4]->num_tokens = 0;
        subscriptions[4]->tokens = NULL;
        subscriptions[4]->num_keys = 0;
        subscriptions[4]->keys = NULL;
        subscriptions[4]->cbfunc = test_cbfunc5;
    
        /* setup the trigger information - initialize the common elements */
        OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
        trig.addr_mode = ORTE_GPR_TOKENS_XAND;
        trig.segment = strdup("test-segment");
        trig.tokens = (char**)malloc(sizeof(char*));
        if (NULL == trig.tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            for (i=0; i < 5; i++) OBJ_RELEASE(subscriptions[i]);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig.tokens[0] = strdup("test-container");
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
       
       /* enter things as three different subscriptions */
       rc = gpr_module->subscribe(
             ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_MONITOR_ONLY,
             2, subscriptions,
             1, &trigs,
             sub);
    
    
       rc = gpr_module->subscribe(
             ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_MONITOR_ONLY,
             2, &(subscriptions[2]),
             1, &trigs,
             sub);
    
       rc = gpr_module->subscribe(
             ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_MONITOR_ONLY,
             1, &(subscriptions[4]),
             1, &trigs,
             sub);
    
        for (i=0; i < 5; i++) OBJ_RELEASE(subscriptions[i]);
        OBJ_DESTRUCT(&trig);
    
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    gpr_module->dump_triggers(0);
    return ORTE_SUCCESS;
    
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
    value.keyvals[0]->type = ORTE_NULL;
    
    for (i=0; i < 10; i++) {
        fprintf(test_out, "\tincrement %s\n", keys[1]);
        if (ORTE_SUCCESS != (rc = gpr_module->increment_value(&value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&value);
            return rc;
        }
    }

    gpr_module->dump_all(0);
    OBJ_DESTRUCT(&value);
    
    return ORTE_SUCCESS;
}

int test2(void)
{
    int rc, i;
    orte_gpr_value_t *values, value, trig, *trigs;
    orte_gpr_subscription_t *subscription;
    orte_gpr_notify_id_t sub;

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
    value.keyvals[0]->type = ORTE_UINT32;
    value.keyvals[0]->value.ui32 = 0;
    
    values = &value;
    
    fprintf(test_out, "putting level test counter on registry\n");
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value); /* clean up */
    
    /* setup a subscription that defines a set of data that is to be
     * returned to the corresponding callback function
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->addr_mode = ORTE_GPR_TOKENS_OR;
    subscription->segment = strdup("test-segment");
    subscription->user_tag = NULL;
    /* ask for the stupid-value-one data from the first
     * container ONLY
     */
    subscription->num_tokens = 2;
    subscription->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(subscription->tokens[i]), "dummy%d", i);
    }
    subscription->num_keys = 1;
    subscription->keys =(char**)malloc(sizeof(char*));
    subscription->keys[0] = strdup("stupid-value-one");
    subscription->cbfunc = test_cbfunc6;

    /* setup the trigger information - want trigger to fire when
     * a specific counter reaches a specified value
     */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    trig.segment = strdup("test-segment");
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(subscription);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup("test-level-trigger");
    trig.num_tokens = 1;
    trig.cnt = 1;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    trig.keyvals[0]->key = strdup("level-counter");
    trig.keyvals[0]->type = ORTE_UINT32;
    trig.keyvals[0]->value.i32 = 2;

   fprintf(test_out, "setting level trigger\n");
   
   trigs = &trig;
   
   /* enter subscription */
   rc = gpr_module->subscribe(
         ORTE_GPR_TRIG_AT_LEVEL | ORTE_GPR_TRIG_MONITOR_ONLY,
         1, &subscription,
         1, &trigs,
         &sub);

    gpr_module->dump_triggers(0);
    
    /* cleanup */
    OBJ_RELEASE(subscription);
    OBJ_DESTRUCT(&trig);
    
    fprintf(test_out, "incrementing until level trigger\n");
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
    value.keyvals[0]->type = ORTE_NULL;
    
    for (i=0; i < 10; i++) {
        fprintf(test_out, "\tincrement level-counter\n");
        if (ORTE_SUCCESS != (rc = gpr_module->increment_value(&value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&value);
            return rc;
        }
    }

    gpr_module->dump_all(0);
    OBJ_DESTRUCT(&value);
    
    return ORTE_SUCCESS;
}


int test3(void)
{
    int rc;
    size_t i;
    orte_gpr_value_t value, *val;
    orte_gpr_subscription_t *subscription;
    orte_gpr_notify_id_t sub;

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
        (val->keyvals[i])->type = ORTE_UINT32;
        (val->keyvals[i])->value.ui32 = (uint32_t)i;
    }
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &val))) {
        fprintf(test_out, "put failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("put failed");
        test_finalize();
        return rc;
    }
    OBJ_RELEASE(val);
    
    /* setup a subscription on one of the containers
     * that notifies callback 1 if something is added
     */
    subscription = OBJ_NEW(orte_gpr_subscription_t);
    subscription->addr_mode = ORTE_GPR_TOKENS_OR;
    subscription->segment = strdup("test-segment");
    subscription->user_tag = NULL;
    /* monitor the dummy-sub-xx container only
     */
    subscription->num_tokens = 2;
    subscription->tokens = (char**)malloc(2*sizeof(char*));
    for (i=0; i < 2; i++) {
        asprintf(&(subscription->tokens[i]), "dummy-sub-%lu", (unsigned long) i);
    }
    /* get notified when anything is added */
    subscription->num_keys = 0;
    subscription->keys = NULL;

    /* send notification to callback 1 */
    subscription->cbfunc = test_cbfunc1;
    
    /* enter subscription */
    rc = gpr_module->subscribe(
         ORTE_GPR_NOTIFY_ADD_ENTRY,
         1, &subscription,
         0, NULL,
         &sub);

    gpr_module->dump_triggers(0);
    
    /* cleanup */
    OBJ_RELEASE(subscription);
    
    
    /* add something to the container */
    
    fprintf(test_out, "adding something - should trigger\n");
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
    val->keyvals[0]->key = strdup("test-notify-add");
    val->keyvals[0]->type = ORTE_NULL;
    
    if (ORTE_SUCCESS != (rc = gpr_module->put(1, &val))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(val);
        return rc;
    }

    gpr_module->dump_all(0);
    OBJ_RELEASE(val);
    
    return ORTE_SUCCESS;
}


void test_cbfunc1(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 1\n");
    
    gpr_module->dump_notify_data(data, 0);
}

void test_cbfunc2(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 2\n");
    
    gpr_module->dump_notify_data(data, 0);
}

void test_cbfunc3(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 3\n");
    
    gpr_module->dump_notify_data(data, 0);
}

void test_cbfunc4(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 4\n");
    
    gpr_module->dump_notify_data(data, 0);
}

void test_cbfunc5(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 5\n");
    
    gpr_module->dump_notify_data(data, 0);
}

void test_cbfunc6(orte_gpr_notify_data_t *data, void *tag)
{
    fprintf(test_out, "\n\n\nTRIGGER FIRED AND RECEIVED AT CALLBACK 6\n");
    
    gpr_module->dump_notify_data(data, 0);
}
