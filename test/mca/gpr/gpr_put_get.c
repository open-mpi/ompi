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

#include "orte/include/orte_constants.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/rmgr/base/base.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dps/dps.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/mca/gpr/base/base.h"
#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

int main(int argc, char **argv)
{
    int rc;
    size_t i, j, cnt;
    char *names[15], *keys[5];
    orte_gpr_keyval_t **kvals, *kval;
    orte_gpr_value_t **values, *val;
    
    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_gpr_replica_out", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      fprintf(stderr, "gpr_test couldn't open test file failed");
      exit(1);
    } 

    orte_init(false);
    
    fprintf(stderr, "put one value with single keyval\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->cnt = 1;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 14;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < 14; i++) {
        asprintf(&(val->tokens[i]), "dummy%lu", (unsigned long) i);
    }
    val->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    (val->keyvals[0])->key = strdup("stupid-value-next-one");
    (val->keyvals[0])->type = ORTE_INT32;
    (val->keyvals[0])->value.i32 = 654321;
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(test_out, "gpr_test: put of 1 value/1 keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: put of 1 value/1 keyval passed\n");
    }
    OBJ_RELEASE(val);
    
    fprintf(stderr, "put one value with multiple keyvals\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->cnt = 20;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 14;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < 14; i++) {
        asprintf(&(val->tokens[i]), "dummy%lu", (unsigned long) i);
    }
    val->keyvals = (orte_gpr_keyval_t**)malloc(20*sizeof(orte_gpr_keyval_t*));
    for (i=0; i<20; i++) {
        val->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&((val->keyvals[i])->key), "stupid-test-%lu", 
                 (unsigned long) i);
        (val->keyvals[i])->type = ORTE_UINT32;
        (val->keyvals[i])->value.ui32 = (uint32_t)i;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(test_out, "gpr_test: put 1 value/multiple keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: put 1 value/multiple keyval passed\n");
    }
    OBJ_RELEASE(val);
    
    fprintf(stderr, "put 1 value/multiple keyvals - second container\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 10;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < val->num_tokens; i++) {
        asprintf(&(val->tokens[i]), "dummy%lu", (unsigned long) i);
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
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(test_out, "gpr_test: put 1 value/multiple keyval in second container failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: put 1 value/multiple keyval in second container passed\n");
    }
    OBJ_RELEASE(val);
    
    fprintf(stderr, "dump\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.dump_all(0))) {
        fprintf(test_out, "gpr_test: dump failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: dump passed\n");
    }

    fprintf(stderr, "get\n");
    names[0] = strdup("dummy0");
    names[1] = NULL;
    keys[0] = strdup("stupid-test-1");
    keys[1] = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                names, keys,
                                &cnt, &values))) {
        fprintf(test_out, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get passed\n");
    }
    free(keys[0]);
    
    fprintf(stderr, "get results:\n");
    for (j=0; j < cnt; j++) {
        fprintf(stderr, "value %lu: cnt %lu\t segment %s num_tokens %lu\n",
                (unsigned long) j, (unsigned long) values[j]->cnt,
                values[j]->segment, (unsigned long) values[j]->num_tokens);
        for (i=0; i < values[j]->num_tokens; i++) {
            fprintf(stderr, "token: %lu %s\n", (unsigned long) i,
                    values[j]->tokens[i]);
        }
        kvals = values[j]->keyvals;
        for (i=0; i < values[j]->cnt; i++) {
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->type);
        }
        OBJ_RELEASE(values[j]);
    }
    free(values);
    
    fprintf(stderr, "get multiple in one segment, multiple containers\n");
    keys[0] = strdup("stupid-test-1");
    keys[1] = strdup("stupid-test-3");
    keys[2] = strdup("stupid-test-5");
    keys[3] = strdup("stupid-test-8");
    keys[4] = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                names, keys,
                                &cnt, &values))) {
        fprintf(test_out, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get passed\n");
    }
    for (i=0; i < 4; i++) free(keys[i]);
    free(names[0]);
    
    fprintf(stderr, "get results:\n");
    for (j=0; j < cnt; j++) {
        fprintf(stderr, "value %lu: cnt %lu\t segment %s num_tokens %lu\n", 
                (unsigned long) j, (unsigned long) values[j]->cnt,
                values[j]->segment, (unsigned long) values[j]->num_tokens);
        for (i=0; i < values[j]->num_tokens; i++) {
            fprintf(stderr, "token: %lu %s\n", (unsigned long) i,
                    values[j]->tokens[i]);
        }
        kvals = values[j]->keyvals;
        for (i=0; i < values[j]->cnt; i++) {
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->type);
        }
        OBJ_RELEASE(values[j]);
    }
    free(values);
    
    fprintf(stderr, "put multiple copies of same entry in single container\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->cnt = 1;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 5;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < 5; i++) {
        asprintf(&(val->tokens[i]), "multi-dum-dum-%lu", (unsigned long) i);
    }
    val->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    (val->keyvals[0])->key = strdup("stupid-value-next-one");
    (val->keyvals[0])->type = ORTE_STRING;
    (val->keyvals[0])->value.strptr = strdup("try-string-value");
    for (i = 0; i < 10; i++) {
        fprintf(stderr, "\tputting copy %lu\n", (unsigned long) i);
        if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
            fprintf(test_out, "gpr_test: put multiple copies of one keyval in a container failed with error code %d\n", rc);
            return rc;
        }
    }
    OBJ_RELEASE(val);
    
    orte_gpr.dump_all(0);
    
    fprintf(stderr, "put with no tokens puts in every container\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_NO_OVERWRITE;
    val->cnt = 1;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 0;
    val->tokens = NULL;
    val->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    (val->keyvals[0])->key = strdup("stupid-value-next-one");
    (val->keyvals[0])->type = ORTE_STRING;
    (val->keyvals[0])->value.strptr = strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
            fprintf(test_out, "gpr_test: put with no tokens failed - returned %d\n", rc);
    } else {
        fprintf(test_out, "gpr_test: put with no tokens passed\n");
    }
    OBJ_RELEASE(val);
    
    orte_gpr.dump_all(0);

    fprintf(stderr, "get with no tokens, KEYS_OR\n");
    keys[0] = strdup("stupid-test-1");
    keys[1] = strdup("stupid-test-3");
    keys[2] = strdup("stupid-test-5");
    keys[3] = strdup("stupid-test-8");
    keys[4] = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                NULL, keys,
                                &cnt, &values))) {
        fprintf(test_out, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get with no tokens, KEYS_OR passed\n");
    }
    for (i=0; i < 4; i++) free(keys[i]);
    
    fprintf(stderr, "get results:\n");
    for (j=0; j < cnt; j++) {
        fprintf(stderr, "value %lu: cnt %lu\t segment %s num_tokens %lu\n", 
                (unsigned long) j, (unsigned long) values[j]->cnt,
                values[j]->segment, (unsigned long) values[j]->num_tokens);
        for (i=0; i < values[j]->num_tokens; i++) {
            fprintf(stderr, "token: %lu %s\n", (unsigned long) i,
                    values[j]->tokens[i]);
        }
        kvals = values[j]->keyvals;
        for (i=0; i < values[j]->cnt; i++) {
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->type);
        }
        OBJ_RELEASE(values[j]);
    }
    free(values);
    
    fprintf(stderr, "get with no tokens, KEYS_AND\n");
    keys[0] = strdup("stupid-test-1");
    keys[1] = strdup("stupid-test-3");
    keys[2] = strdup("stupid-test-5");
    keys[3] = strdup("stupid-test-8");
    keys[4] = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_AND | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                NULL, keys,
                                &cnt, &values))) {
        fprintf(test_out, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get with no tokens, KEYS_AND passed\n");
    }
    for (i=0; i < 4; i++) free(keys[i]);
    
    fprintf(stderr, "get results:\n");
    for (j=0; j < cnt; j++) {
        fprintf(stderr, "value %lu: cnt %lu\t segment %s num_tokens %lu\n", 
                (unsigned long) j, (unsigned long) values[j]->cnt,
                values[j]->segment, (unsigned long) values[j]->num_tokens);
        for (i=0; i < values[j]->num_tokens; i++) {
            fprintf(stderr, "token: %lu %s\n", (unsigned long) i,
                    values[j]->tokens[i]);
        }
        kvals = values[j]->keyvals;
        for (i=0; i < values[j]->cnt; i++) {
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->type);
        }
        OBJ_RELEASE(values[j]);
    }
    free(values);
    
    fprintf(stderr, "overwrite a bunch of values with one\n");
    val = OBJ_NEW(orte_gpr_value_t);
    val->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND;
    val->cnt = 1;
    val->segment = strdup("test-put-segment");
    val->num_tokens = 5;
    val->tokens = (char**)malloc(val->num_tokens * sizeof(char*));
    for (i=0; i < 5; i++) {
        asprintf(&(val->tokens[i]), "multi-dum-dum-%lu", (unsigned long) i);
    }
    val->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    val->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    (val->keyvals[0])->key = strdup("stupid-value-next-one");
    (val->keyvals[0])->type = ORTE_STRING;
    (val->keyvals[0])->value.strptr = strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(test_out, "gpr_test: put multiple copies of one keyval in a container failed with error code %d\n", rc);
        return rc;
    }
    OBJ_RELEASE(val);
    
    orte_gpr.dump_all(0);
    
    fprintf(stderr, "get_conditional, KEYS_AND\n");
    keys[0] = strdup("stupid-test-1");
    keys[1] = strdup("stupid-test-3");
    keys[2] = NULL;
    kval = OBJ_NEW(orte_gpr_keyval_t);
    kval->key = strdup("stupid-test-6");
    kval->type = ORTE_UINT32;
    kval->value.ui32 = 6;
    
    values = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get_conditional(ORTE_GPR_KEYS_AND | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                NULL, keys, 1, &kval,
                                &cnt, &values))) {
        fprintf(test_out, "gpr_test: get conditional failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get conditional, KEYS_AND passed\n");
    }
    for (i=0; i < 2; i++) free(keys[i]);
    OBJ_RELEASE(kval);
    
    fprintf(stderr, "get results:\n");
    for (j=0; j < cnt; j++) {
        fprintf(stderr, "value %lu: cnt %lu\t segment %s num_tokens %lu\n", 
                (unsigned long) j, (unsigned long) values[j]->cnt,
                values[j]->segment, (unsigned long) values[j]->num_tokens);
        for (i=0; i < values[j]->num_tokens; i++) {
            fprintf(stderr, "token: %lu %s\n", (unsigned long) i,
                    values[j]->tokens[i]);
        }
        kvals = values[j]->keyvals;
        for (i=0; i < values[j]->cnt; i++) {
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->type);
        }
        OBJ_RELEASE(values[j]);
    }
    if (NULL != values) free(values);
    
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
    unlink("test_gpr_replica_out");

    return(0);
}
