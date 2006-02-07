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

int main(int argc, char **argv)
{
    int rc;
    size_t i, j, cnt;
    char *names[15], *keys[5];
    orte_gpr_keyval_t **kvals, *kval;
    orte_gpr_value_t **values, *val;
    int32_t i32;

    /* initialize system */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        fprintf(stderr, "couldn't complete init of system - error code %d\n", rc);
        exit(1);
    }

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
    (val->keyvals[0])->value = OBJ_NEW(orte_data_value_t);
    (val->keyvals[0])->value->type = ORTE_INT32;
    i32 = 654321;
    orte_dss.copy(&(val->keyvals[0]->value->data), &i32, ORTE_INT32);
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "gpr_test: put of 1 value/1 keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: put of 1 value/1 keyval passed\n");
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
        (val->keyvals[i])->value = OBJ_NEW(orte_data_value_t);
        (val->keyvals[i])->value->type = ORTE_UINT32;
        i32 = i;
        orte_dss.copy(&(val->keyvals[i]->value->data), &i32, ORTE_UINT32);
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval passed\n");
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
        (val->keyvals[i])->value = OBJ_NEW(orte_data_value_t);
        (val->keyvals[i])->value->type = ORTE_UINT32;
        i32 = i;
        orte_dss.copy(&(val->keyvals[i]->value->data), &i32, ORTE_UINT32);
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval in second container failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval in second container passed\n");
    }
    OBJ_RELEASE(val);

    fprintf(stderr, "dump\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.dump_all(0))) {
        fprintf(stderr, "gpr_test: dump failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: dump passed\n");
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
        fprintf(stderr, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: get passed\n");
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
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->value->type);
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
        fprintf(stderr, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: get passed\n");
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
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->value->type);
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
    (val->keyvals[0])->value = OBJ_NEW(orte_data_value_t);
    (val->keyvals[0])->value->type = ORTE_STRING;
    (val->keyvals[0])->value->data = strdup("try-string-value");
    for (i = 0; i < 10; i++) {
        fprintf(stderr, "\tputting copy %lu\n", (unsigned long) i);
        if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
            fprintf(stderr, "gpr_test: put multiple copies of one keyval in a container failed with error code %d\n", rc);
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
    (val->keyvals[0])->value = OBJ_NEW(orte_data_value_t);
    (val->keyvals[0])->value->type = ORTE_STRING;
    (val->keyvals[0])->value->data = strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "gpr_test: put with no tokens failed - returned %d\n", rc);
    } else {
        fprintf(stderr, "gpr_test: put with no tokens passed\n");
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
        fprintf(stderr, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: get with no tokens, KEYS_OR passed\n");
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
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->value->type);
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
        fprintf(stderr, "gpr_test: get failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: get with no tokens, KEYS_AND passed\n");
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
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->value->type);
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
    (val->keyvals[0])->value = OBJ_NEW(orte_data_value_t);
    (val->keyvals[0])->value->type = ORTE_STRING;
    (val->keyvals[0])->value->data = strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &val))) {
        fprintf(stderr, "gpr_test: put multiple copies of one keyval in a container failed with error code %d\n", rc);
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
    kval->value = OBJ_NEW(orte_data_value_t);
    kval->value->type = ORTE_UINT32;
    i32 = 6;
    orte_dss.copy(&(kval->value->data), &i32, ORTE_UINT32);

    values = NULL;
    if (ORTE_SUCCESS != (rc = orte_gpr.get_conditional(ORTE_GPR_KEYS_AND | ORTE_GPR_TOKENS_OR,
                                "test-put-segment",
                                NULL, keys, 1, &kval,
                                &cnt, &values))) {
        fprintf(stderr, "gpr_test: get conditional failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: get conditional, KEYS_AND passed\n");
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
            fprintf(stderr, "\tkey %s type %d\n", kvals[i]->key, kvals[i]->value->type);
        }
        OBJ_RELEASE(values[j]);
    }
    if (NULL != values) free(values);

    fprintf(stderr, "now finalize and see if all memory cleared\n");
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(stderr, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }

    return(0);
}
