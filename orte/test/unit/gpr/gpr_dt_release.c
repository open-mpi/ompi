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

#include "orte/orte_constants.h"

#include "opal/runtime/opal.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"

static bool test_unstruct(void);
static bool test_keyval(void);
static bool test_val(void);
static bool test_sub(void);
static bool test_trig(void);
static bool test_notify_data(void);
static bool test_notify_msg(void);

FILE *test_out;

int main(int argc, char **argv)
{
    int ret;

    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    test_out = stderr;

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }

    orte_process_info.seed = true;
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->cellid = 0;
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(stderr, "MCA started\n");
    } else {
        fprintf(stderr, "MCA could not start\n");
        exit (1);
    }

    /* open the dss */
    if (ORTE_SUCCESS == orte_dss_open()) {
        fprintf(stderr, "DSS started\n");
    } else {
        fprintf(stderr, "DSS could not start\n");
        exit (1);
    }

    /* startup the gpr to register data types */
    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(stderr, "GPR opened\n");
    } else {
        fprintf(stderr, "GPR could not open\n");
        exit (1);
    }

    /* Now do the tests */

    fprintf(stderr, "executing test_unstruct\n");
    if (test_unstruct()) {
        fprintf(stderr, "test_unstruct succeeded\n");
    }
    else {
        fprintf(stderr, "test_unstruct failed\n");
    }

    fprintf(stderr, "executing test_keyval\n");
    if (test_keyval()) {
        fprintf(stderr, "test_keyval succeeded\n");
    }
    else {
        fprintf(stderr, "test_keyval failed\n");
    }

    fprintf(stderr, "executing test_val\n");
    if (test_val()) {
        fprintf(stderr, "test_val succeeded\n");
    }
    else {
        fprintf(stderr, "test_val failed\n");
    }

    fprintf(stderr, "executing test_sub\n");
    if (test_sub()) {
        fprintf(stderr, "test_sub succeeded\n");
    }
    else {
        fprintf(stderr, "test_sub failed\n");
    }

    fprintf(stderr, "executing test_trig\n");
    if (test_trig()) {
        fprintf(stderr, "test_trig succeeded\n");
    }
    else {
        fprintf(stderr, "test_trig failed\n");
    }

    fprintf(stderr, "executing test_notify_data\n");
    if (test_notify_data()) {
        fprintf(stderr, "test_notify_data succeeded\n");
    }
    else {
        fprintf(stderr, "test_notify_data failed\n");
    }

    fprintf(stderr, "executing test_notify_msg\n");
    if (test_notify_msg()) {
        fprintf(stderr, "test_notify_msg succeeded\n");
    }
    else {
        fprintf(stderr, "test_notify_msg failed\n");
    }

    orte_dss_close();

    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return 0;
}

static bool test_unstruct(void)
{
    orte_gpr_cmd_flag_t scmd, *dcmd;
    orte_gpr_subscription_id_t ssubid, *dsubid;
    orte_gpr_trigger_id_t strigid, *dtrigid;
    orte_gpr_notify_action_t snotact, *dnotact;
    orte_gpr_trigger_action_t strigact, *dtrigact;
    orte_gpr_notify_msg_type_t snottype, *dnottype;
    orte_gpr_addr_mode_t smode, *dmode;

    orte_data_value_t *dval2, dval = { {OBJ_CLASS(orte_data_value_t),0},ORTE_UNDEF,NULL};

    scmd = 0x0f;
    ssubid = 31;
    strigid = 28;
    snotact = ORTE_GPR_NOTIFY_ANY;
    strigact = ORTE_GPR_TRIG_ANY;
    snottype = 0x0f;
    smode = 0x0f0f;

    dval2 = OBJ_NEW(orte_data_value_t);
    dval2->type = ORTE_GPR_CMD;
    if (ORTE_SUCCESS != orte_dss.copy((void**)&(dval2->data), &scmd, ORTE_GPR_CMD)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_CMD returned error\n");
        return(false);
    }
    orte_dss.release(dval2);
    if (NULL != dval2->data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_CMD failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dsubid, &ssubid, ORTE_GPR_SUBSCRIPTION_ID)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_SUBSCRIPTION_ID returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_SUBSCRIPTION_ID;
    dval.data = dsubid;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_SUBSCRIPTION_ID failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dtrigid, &strigid, ORTE_GPR_TRIGGER_ID)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_TRIGGER_ID returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_TRIGGER_ID;
    dval.data = dtrigid;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_TRIGGER_ID failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dnotact, &snotact, ORTE_GPR_NOTIFY_ACTION)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_NOTIFY_ACTION returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_NOTIFY_ACTION;
    dval.data = dnotact;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_NOTIFY_ACTION failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dtrigact, &strigact, ORTE_GPR_TRIGGER_ACTION)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_TRIGGER_ACTION returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_TRIGGER_ACTION;
    dval.data = dtrigact;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_TRIGGER_ACTION failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dnottype, &snottype, ORTE_GPR_NOTIFY_MSG_TYPE)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_NOTIFY_MSG_TYPE returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_NOTIFY_MSG_TYPE;
    dval.data = dnottype;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_NOTIFY_MSG_TYPE failed to NULL pointer\n");
        return(false);
    }

    if (ORTE_SUCCESS != orte_dss.copy((void**)&dmode, &smode, ORTE_GPR_ADDR_MODE)) {
        fprintf(test_out, "orte_dss.copy ORTE_GPR_ADDR_MODE returned error\n");
        return(false);
    }
    dval.type = ORTE_GPR_ADDR_MODE;
    dval.data = dmode;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_ADDR_MODE failed to NULL pointer\n");
        return(false);
    }

    return (true);
}

static bool test_keyval(void)
{
    int32_t i16=2345;
    orte_gpr_keyval_t *src;
    orte_data_value_t *sval, dval = { {OBJ_CLASS(orte_data_value_t),0},ORTE_UNDEF,NULL};

    src = OBJ_NEW(orte_gpr_keyval_t);
    src->key = strdup("test-key");
    src->value = OBJ_NEW(orte_data_value_t);
    sval = src->value;
    sval->type = ORTE_INT16;
    if (ORTE_SUCCESS != orte_dss.copy((void**)&(sval->data), &i16, ORTE_INT16)) {
        fprintf(stderr, "orte_dss.copy int16 returned error\n");
        return(false);
    }

    dval.type = ORTE_GPR_KEYVAL;
    dval.data = src;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_KEYVAL failed to NULL pointer\n");
        return(false);
    }

    return (true);
}


static bool test_val(void)
{
    orte_gpr_value_t *src;
    orte_gpr_keyval_t *keyvals;
    orte_data_value_t *sval, dval = { {OBJ_CLASS(orte_data_value_t),0},ORTE_UNDEF,NULL};
    size_t j;
    int16_t i16;
    int32_t i32;

    src = OBJ_NEW(orte_gpr_value_t);
    src->addr_mode = 0x01;
    src->segment = strdup("test-segment");
    src->num_tokens = 3;
    src->tokens = (char**)malloc(src->num_tokens * sizeof(char*));
    for (j=0; j < src->num_tokens; j++) {
        src->tokens[j] = strdup("test-token");
    }
    src->cnt = 21;
    src->keyvals = (orte_gpr_keyval_t**)malloc(src->cnt * sizeof(orte_gpr_keyval_t*));
    for (j=0; j < src->cnt; j++) {
        src->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
        src->keyvals[j]->value = OBJ_NEW(orte_data_value_t);
        sval = src->keyvals[j]->value;
        asprintf(&((src->keyvals[j])->key), "%lu",
                    (unsigned long) j);
        sval->type = ((j % 2) == 0) ? ORTE_INT16 : ORTE_INT32;
        if (sval->type == ORTE_INT16) {
            i16 = 1024*j;
            if (ORTE_SUCCESS != orte_dss.copy((void**)&(sval->data), &i16, ORTE_INT16)) {
                fprintf(stderr, "orte_dss.copy returned error\n");
                return(false);
            }
        }  else {
            i32 = 2048*j;
            if (ORTE_SUCCESS != orte_dss.copy((void**)&(sval->data), &i32, ORTE_INT32)) {
                fprintf(stderr, "orte_dss.copy returned error\n");
                return(false);
            }
        }
    }

    dval.type = ORTE_GPR_VALUE;
    dval.data = src;
    orte_dss.release(&dval);
    if (NULL != dval.data) {
        fprintf(test_out, "orte_dss.release ORTE_GPR_KEYVAL failed to NULL pointer\n");
        return(false);
    }

    return(true);
}

static bool test_sub(void)
{
   size_t i, j, k;
   orte_gpr_subscription_t *src[2];
   orte_gpr_subscription_t *dst[2];

   return true; /* no comparison function defined yet */

   /* test gpr_subscription */
   for(i=0; i<2; i++) {
       src[i] = OBJ_NEW(orte_gpr_subscription_t);
       if (i % 2) {
           src[i]->name = strdup("dummy-name");
       }
       src[i]->id = (orte_gpr_subscription_id_t)i;
       src[i]->action = (orte_gpr_notify_action_t)0x0f;

       /* test value counts of 1 to +1 */
       src[i]->cnt = i + 1;
       src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));

       for (k=0; k < src[i]->cnt; k++) {
           src[i]->values[k] = OBJ_NEW(orte_gpr_value_t);
           src[i]->values[k]->addr_mode = (uint16_t) i;
           src[i]->values[k]->segment = strdup("test-segment");

           /* test token counts of 0! to  */
           src[i]->values[k]->num_tokens = i;
           if (src[i]->values[k]->num_tokens) { /* if to allow testing of token count of zero */
               src[i]->values[k]->tokens = (char**)malloc(src[i]->values[k]->num_tokens * sizeof(char*));
               for (j=0; j < src[i]->values[k]->num_tokens; j++) {
                   src[i]->values[k]->tokens[j] = strdup("test-token");
               }
           }

           /* test key counts of 0 to  */
           src[i]->values[k]->cnt = i;
           if (src[i]->values[k]->cnt) { /* if to allow testing of num_keys count of zero */
               src[i]->values[k]->keyvals = (orte_gpr_keyval_t**)malloc(
                       src[i]->values[k]->cnt * sizeof(orte_gpr_keyval_t*));
               for (j=0; j < src[i]->values[k]->cnt; j++) {
                   src[i]->values[k]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
                   src[i]->values[k]->keyvals[j]->key = strdup("test-key");
               }
           }
           /* skip the pointers for cb_func and user_tag */
       }
   }


   for (j=0; j<2; j++) {
       if (ORTE_SUCCESS != orte_dss.copy((void**)&dst[j], src[j], ORTE_GPR_SUBSCRIPTION)) {
           fprintf(test_out, "test_sub copy failed\n");
           return(false);
       }

       if (ORTE_EQUAL != orte_dss.compare(dst[j], src[j], ORTE_GPR_SUBSCRIPTION)) {
           fprintf(stderr, "orte_dss.compare sub failed\n");
           return(false);
       }
   }

   return (true);
}

static bool test_trig(void)
{
    size_t i, j, k;
    orte_gpr_trigger_t *src[2];
    orte_gpr_trigger_t *dst[2];

    return true; /* no comparison function defined yet */

    /* test gpr_trigger */
    for(i=0; i<2; i++) {
        src[i] = OBJ_NEW(orte_gpr_trigger_t);
        if (i % 2) {
            src[i]->name = strdup("dummy-name");
        }
        src[i]->id = (orte_gpr_trigger_id_t)i;
        src[i]->action = (orte_gpr_trigger_action_t)0x0f;

        /* test value counts of 1 to +1 */
        src[i]->cnt = i + 1;
        src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));

        for (k=0; k < src[i]->cnt; k++) {
            src[i]->values[k] = OBJ_NEW(orte_gpr_value_t);
            src[i]->values[k]->addr_mode = (uint16_t) i;
            src[i]->values[k]->segment = strdup("test-segment");

            /* test token counts of 0! to  */
            src[i]->values[k]->num_tokens = i;
            if (src[i]->values[k]->num_tokens) { /* if to allow testing of token count of zero */
                src[i]->values[k]->tokens = (char**)malloc(src[i]->values[k]->num_tokens * sizeof(char*));
                for (j=0; j < src[i]->values[k]->num_tokens; j++) {
                    src[i]->values[k]->tokens[j] = strdup("test-token");
                }
            }

            /* test key counts of 0 to  */
            src[i]->values[k]->cnt = i;
            if (src[i]->values[k]->cnt) { /* if to allow testing of num_keys count of zero */
                src[i]->values[k]->keyvals = (orte_gpr_keyval_t**)malloc(
                        src[i]->values[k]->cnt * sizeof(orte_gpr_keyval_t*));
                for (j=0; j < src[i]->values[k]->cnt; j++) {
                    src[i]->values[k]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
                    src[i]->values[k]->keyvals[j]->key = strdup("test-key");
                }
            }
            /* skip the pointers for cb_func and user_tag */
        }
    }

    for(j=0; j<2; j++) {
       if (ORTE_SUCCESS != orte_dss.copy((void**)&dst[j], src[j], ORTE_GPR_TRIGGER)) {
           fprintf(test_out, "test_trig copy failed\n");
           return(false);
        }

        if (ORTE_EQUAL != orte_dss.compare(dst[j], src[j], ORTE_GPR_TRIGGER)) {
            fprintf(stderr, "orte_dss.compare trig failed\n");
            return(false);
        }
    }

    return (true);
}


static bool test_notify_data(void)
{
    size_t i, j, k;
    orte_data_value_t *sdv;
    int32_t i32;
    orte_gpr_value_t *value;
    orte_gpr_notify_data_t *src[2];
    orte_gpr_notify_data_t *dst[2];

    return true; /* no comparison function defined yet */

    for(i=0; i<2; i++) {
        src[i] = OBJ_NEW(orte_gpr_notify_data_t);
        if (i % 2) { /* test half with a name and with remove=true */
            src[i]->target = strdup("test-notify-data-name");
            src[i]->remove = true;
        }
        src[i]->id = i;

        /* test value counts of 0 to -1 */
        src[i]->cnt = i; /* value count */

        for (j=0; j < src[i]->cnt; j++) {
            value = OBJ_NEW(orte_gpr_value_t);
            value->addr_mode = (orte_gpr_addr_mode_t) i+j+1;
            value->segment = strdup("test-gpr-notify-value-segment-name");  /* ek segment name again! */

            /* tokens */
            value->num_tokens = j; /* test tokens within gpr values within notify message between 0--1 */
            if (value->num_tokens) { /* if to allow testing of num_tokens count of zero */
                value->tokens = (char**)malloc(value->num_tokens * sizeof(char*));
                for (k=0; k < value->num_tokens; k++) {
                    value->tokens[k] = strdup("test-grp-notify-value-token");
                } /* for each token */
            } /* if tokens */

            /* keyval pairs (field name is 'cnt' same as used for value count so be careful) */
            value->cnt = j; /* test keyval pairs within gpr values within notify message between 0--1 */
            if (value->cnt) { /* if to allow testing of keyval pair count of zero */
                value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));
                for (k=0; k < value->cnt; k++) {
                    value->keyvals[k] = OBJ_NEW (orte_gpr_keyval_t);
                    value->keyvals[k]->value = OBJ_NEW(orte_data_value_t);
                    sdv = value->keyvals[k]->value;
                    value->keyvals[k]->key = strdup("test-grp-notify-value-key");
                    sdv->type = ORTE_INT32; /* make it simplier */
                    i32 = (i*100)+(j*10)+k;
                    if (ORTE_SUCCESS != orte_dss.copy((void**)&(sdv->data), &i32, ORTE_INT32)) {
                        fprintf(test_out, "test_notify_data: error copying data into source value\n");
                        return(false);
                    }
                } /* for each keyval pair */
            } /* if keyvals */

            /* add the value to the data object */
            orte_pointer_array_add(&k, src[i]->values, value);
        } /* for each value */
    }

    for(j=0; j<2; j++) {
        if (ORTE_SUCCESS != orte_dss.copy((void**)&dst[j], src[j], ORTE_GPR_NOTIFY_DATA)) {
            fprintf(test_out, "test_notify_data copy failed\n");
            return(false);
        }
        if (ORTE_EQUAL != orte_dss.compare(dst[j], src[j], ORTE_GPR_NOTIFY_DATA)) {
            fprintf(stderr, "orte_dss.compare notify_data failed\n");
            return(false);
        }

    } /* for each ELEMENT */
    return (true);
}

static bool test_notify_msg(void)
{
    return (true);
}

