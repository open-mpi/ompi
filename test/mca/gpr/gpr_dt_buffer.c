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

#define NUM_ITERS 1
#define NUM_ELEMS 3

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
    orte_buffer_t *bufA;
    orte_gpr_cmd_flag_t scmd[NUM_ELEMS], dcmd[NUM_ELEMS];
    orte_gpr_subscription_id_t ssubid[NUM_ELEMS], dsubid[NUM_ELEMS];
    orte_gpr_trigger_id_t strigid[NUM_ELEMS], dtrigid[NUM_ELEMS];
    orte_gpr_notify_action_t snotact[NUM_ELEMS], dnotact[NUM_ELEMS];
    orte_gpr_trigger_action_t strigact[NUM_ELEMS], dtrigact[NUM_ELEMS];
    orte_gpr_notify_msg_type_t snottype[NUM_ELEMS], dnottype[NUM_ELEMS];
    orte_gpr_addr_mode_t smode[NUM_ELEMS], dmode[NUM_ELEMS];
    size_t i;
    int rc;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

    for(i=0; i<NUM_ELEMS; i++) {
        scmd[i] = 0x0f;
        ssubid[i] = i;
        strigid[i] = i;
        snotact[i] = ORTE_GPR_NOTIFY_ANY;
        strigact[i] = ORTE_GPR_TRIG_ANY;
        snottype[i] = 0x0f;
        smode[i] = 0x0f0f;

    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, scmd, NUM_ELEMS, ORTE_GPR_CMD);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, ssubid, NUM_ELEMS, ORTE_GPR_SUBSCRIPTION_ID);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, strigid, NUM_ELEMS, ORTE_GPR_TRIGGER_ID);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, snotact, NUM_ELEMS, ORTE_GPR_NOTIFY_ACTION);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, strigact, NUM_ELEMS, ORTE_GPR_TRIGGER_ACTION);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, snottype, NUM_ELEMS, ORTE_GPR_NOTIFY_MSG_TYPE);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, smode, NUM_ELEMS, ORTE_GPR_ADDR_MODE);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed");
            fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        /* buffer is FIFO, so unpack in same order */
        rc = orte_dss.unpack(bufA, dcmd, &count, ORTE_GPR_CMD);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dsubid, &count, ORTE_GPR_SUBSCRIPTION_ID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dtrigid, &count, ORTE_GPR_TRIGGER_ID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dnotact, &count, ORTE_GPR_NOTIFY_ACTION);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dtrigact, &count, ORTE_GPR_TRIGGER_ACTION);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dnottype, &count, ORTE_GPR_NOTIFY_MSG_TYPE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dmode, &count, ORTE_GPR_ADDR_MODE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack failed");
            fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
            return(false);
        }

        /* all from this iteration are unpacked - check all values */
        for(j=0; j<NUM_ELEMS; j++) {
            if(scmd[j] != dcmd[j] ||
               ssubid[j] != dsubid[j] ||
               strigid[j] != dtrigid[j] ||
               snotact[j] != dnotact[j] ||
               strigact[j] != dtrigact[j] ||
               snottype[j] != dnottype[j] ||
               smode[j] != dmode[j]) {
                fprintf(test_out, "test unstructured values: invalid values returned\n");
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

static bool test_keyval(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    int16_t i16;
    int32_t i32;
    orte_gpr_keyval_t *src[NUM_ELEMS];
    orte_gpr_keyval_t *dst[NUM_ELEMS];
    orte_data_value_t *dval, *sval;

    /* setup source array of keyvals */
    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&(src[i]->key), "%lu", (unsigned long)i);
        src[i]->value = OBJ_NEW(orte_data_value_t);
        sval = src[i]->value;
        sval->type = ((i % 2) == 0) ? ORTE_INT16 : ORTE_INT32;
        i16 = i;
        i32 = i;
        if (ORTE_INT16 == sval->type) {
            if (ORTE_SUCCESS != orte_dss.copy((void**)&(sval->data), &i16, ORTE_INT16)) {
                fprintf(stderr, "orte_dss.copy returned error\n");
                return(false);
            }
        } else {
            if (ORTE_SUCCESS != orte_dss.copy((void**)&(sval->data), &i32, ORTE_INT32)) {
                fprintf(stderr, "orte_dss.copy returned error\n");
                return(false);
            }
        }
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_GPR_KEYVAL);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed with error %s\n",
                    ORTE_ERROR_NAME(rc));
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dss.unpack(bufA, dst, &count, ORTE_GPR_KEYVAL);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack (KEYVAL) failed on iteration %lu with error %s\n",
                    (unsigned long)i, ORTE_ERROR_NAME(rc));
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if (0 != strcmp(src[j]->key, dst[j]->key)) {
                fprintf(test_out, "test9: invalid results key mismatch from unpack\n");
                return(false);
            }
            if (src[j]->value->type != dst[j]->value->type) {
                fprintf(test_out, "test9: invalid results type mismatch from unpack\n");
                return(false);
            }
            sval = src[j]->value;
            dval = dst[j]->value;
            if (ORTE_EQUAL != orte_dss.compare(sval->data, dval->data, sval->type)) {
                fprintf(test_out, "test keyval: invalid value returned\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}


static bool test_val(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k;
    int16_t i16;
    int32_t i32;
    orte_gpr_value_t *src[NUM_ELEMS];
    orte_gpr_value_t *dst[NUM_ELEMS];
    orte_data_value_t *dval, *sval;

    /* test gpr_value */
   for(i=0; i<NUM_ELEMS; i++) {
       src[i] = OBJ_NEW(orte_gpr_value_t);
       src[i]->segment = strdup("test-segment");
       src[i]->num_tokens = (i % 10) + 1; /* ensure there is always at least one */
       src[i]->tokens = (char**)malloc(src[i]->num_tokens * sizeof(char*));
       for (j=0; j < src[i]->num_tokens; j++) {
           src[i]->tokens[j] = strdup("test-token");
       }
       src[i]->cnt = (i % 20) + 1;
       src[i]->keyvals = (orte_gpr_keyval_t**)malloc(src[i]->cnt * sizeof(orte_gpr_keyval_t*));
       for (j=0; j < src[i]->cnt; j++) {
           src[i]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
           src[i]->keyvals[j]->value = OBJ_NEW(orte_data_value_t);
           dval = src[i]->keyvals[j]->value;
           asprintf(&((src[i]->keyvals[j])->key), "%lu",
                       (unsigned long) j);
           dval->type = ((j % 2) == 0) ? ORTE_INT16 : ORTE_INT32;
           if (dval->type == ORTE_INT16) {
               i16 = j;
                if (ORTE_SUCCESS != orte_dss.copy((void**)&(dval->data), &i16, ORTE_INT16)) {
                    fprintf(stderr, "orte_dss.copy returned error\n");
                    return(false);
                }
           }  else {
                i32 = j;
                if (ORTE_SUCCESS != orte_dss.copy((void**)&(dval->data), &i32, ORTE_INT32)) {
                    fprintf(stderr, "orte_dss.copy returned error\n");
                    return(false);
                }
           }
       }
   }

   bufA = OBJ_NEW(orte_buffer_t);
   if (NULL == bufA) {
       fprintf(test_out, "orte_buffer failed init in OBJ_NEW");
       fprintf(test_out, "OBJ_NEW failed\n");
       return false;
   }

   for (i=0;i<NUM_ITERS;i++) {
       rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_GPR_VALUE);
       if (ORTE_SUCCESS != rc) {
           fprintf(test_out, "orte_dss.pack failed");
           fprintf(test_out, "orte_dss.pack failed with error %d\n", rc);
           return(false);
       }
   }

   for (i=0; i<NUM_ITERS; i++) {
       int j;
       size_t count = NUM_ELEMS;
       memset(dst,-1,sizeof(dst));

       rc = orte_dss.unpack(bufA, dst, &count, ORTE_GPR_VALUE);
       if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
           fprintf(test_out, "orte_dss.unpack failed");
           fprintf(test_out, "orte_dss.unpack failed with error %d\n", rc);
           return(false);
       }

       for(j=0; j<NUM_ELEMS; j++) {
           if(0 != strcmp(src[j]->segment, dst[j]->segment) ||
              src[j]->num_tokens != dst[j]->num_tokens ||
              src[j]->cnt != dst[j]->cnt) {
               fprintf(test_out, "test1_val: invalid results from unpack");
               return(false);
              }
              for (k=0; k<src[j]->num_tokens; k++) {
                  if (0 != strcmp(src[j]->tokens[k], dst[j]->tokens[k])) {
                      fprintf(test_out, "test1_val: invalid results (tokens) from unpack");
                      return(false);
                  }
              }
              for (k=0; k < src[j]->cnt; k++) {
                  if (0 != strcmp((src[j]->keyvals[k])->key,
                      (dst[j]->keyvals[k])->key)) {
                          fprintf(test_out, "test1_val: invalid results (keyvalues) from unpack");
                          return(false);
                  }
                  dval = dst[j]->keyvals[k]->value;
                  sval = src[j]->keyvals[k]->value;
                  if (sval->type != dval->type) {
                      fprintf(test_out, "test1_val: invalid results (keyvalue types) from unpack");
                      return(false);
                  }
                  if (ORTE_EQUAL != orte_dss.compare(dval->data, sval->data, dval->type)) {
                      fprintf(stderr, "test1_val: orte_dss.compare failed\n");
                      return(false);
                  }
             }
       }
   }

   OBJ_RELEASE(bufA);
   if (NULL != bufA) {
       fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
       fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
       return false;
   }

   return(true);
}

static bool test_sub(void)
{
   orte_buffer_t *bufA;
   int rc;
   size_t i, j, k, m;
   orte_gpr_subscription_t *src[NUM_ELEMS];
   orte_gpr_subscription_t *dst[NUM_ELEMS];

   /* test gpr_subscription */
   for(i=0; i<NUM_ELEMS; i++) {
       src[i] = OBJ_NEW(orte_gpr_subscription_t);
       if (i % 2) {
           src[i]->name = strdup("dummy-name");
       }
       src[i]->id = (orte_gpr_subscription_id_t)i;
       src[i]->action = (orte_gpr_notify_action_t)0x0f;

       /* test value counts of 1 to NUM_ELEMS+1 */
       src[i]->cnt = i + 1;
       src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));

       for (k=0; k < src[i]->cnt; k++) {
           src[i]->values[k] = OBJ_NEW(orte_gpr_value_t);
           src[i]->values[k]->addr_mode = (uint16_t) i;
           src[i]->values[k]->segment = strdup("test-segment");

           /* test token counts of 0! to NUM_ELEMS */
           src[i]->values[k]->num_tokens = i;
           if (src[i]->values[k]->num_tokens) { /* if to allow testing of token count of zero */
               src[i]->values[k]->tokens = (char**)malloc(src[i]->values[k]->num_tokens * sizeof(char*));
               for (j=0; j < src[i]->values[k]->num_tokens; j++) {
                   src[i]->values[k]->tokens[j] = strdup("test-token");
               }
           }

           /* test key counts of 0 to NUM_ELEMS */
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

   /* source data set, now create buffer and pack source data */

   bufA = OBJ_NEW(orte_buffer_t);
   if (NULL == bufA) {
       fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
       return false;
   }


   for (i=0;i<NUM_ITERS;i++) {
       rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_GPR_SUBSCRIPTION);
       if (ORTE_SUCCESS != rc) {
           fprintf(test_out, "test_sub pack failed with return code %d\n", rc);
           return(false);
       }
   }

   for (i=0; i<NUM_ITERS; i++) {
       int j;
       size_t count = NUM_ELEMS;
       memset(dst,-1,sizeof(dst));

       rc = orte_dss.unpack(bufA, dst, &count, ORTE_GPR_SUBSCRIPTION);
       if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
           fprintf(test_out, "test_sub unpack failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
           return(false);
       }

       for(j=0; j<NUM_ELEMS; j++) {

           if ((NULL == src[j]->name && NULL != dst[j]->name) ||
                (NULL != src[j]->name && NULL == dst[j]->name)
              ) {
               fprintf(test_out, "test_sub invalid results from unpack\n");
               return(false);
              }

              if ((NULL != src[j]->name &&
                   0 != strcmp(src[j]->name, dst[j]->name)) ||
                   src[j]->id != dst[j]->id ||
                   src[j]->action != dst[j]->action ||
                   src[j]->cnt != dst[j]->cnt
                 ) {
                  fprintf(test_out, "test_sub: invalid results from unpack\n");
                  return(false);
                 }

                 /* now compare each of the size/cnt dependent values */
                 for (k=0; k<src[j]->cnt; k++) {
                     if (src[j]->values[k]->num_tokens != dst[j]->values[k]->num_tokens) {
                         fprintf(test_out, "test_sub: invalid results (value num_tokens) from unpack\n");
                         return(false);
                     }

                     for (m=0; m < src[j]->values[k]->num_tokens; m++) {
                         if (0 != strcmp(src[j]->values[k]->tokens[m], dst[j]->values[k]->tokens[m])) {
                             fprintf(test_out, "test_sub: invalid results (tokens) from unpack\n");
                             return(false);
                         }
                     }

                     if (src[j]->values[k]->cnt != dst[j]->values[k]->cnt) {
                         fprintf(test_out, "test_sub: invalid results (value cnt) from unpack\n");
                         return(false);
                     }

                     for (m=0; m < src[j]->values[k]->cnt; m++) {
                         if (0 != strcmp(src[j]->values[k]->keyvals[m]->key,
                             dst[j]->values[k]->keyvals[m]->key)) {
                                 fprintf(test_out, "test_sub: invalid results (keys) from unpack\n");
                                 return(false);
                             }
                     }
                 }
       }
   }

   OBJ_RELEASE(bufA);
   if (NULL != bufA) {
       fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
       return false;
   }

   return (true);
}

static bool test_trig(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k, m;
    orte_gpr_trigger_t *src[NUM_ELEMS];
    orte_gpr_trigger_t *dst[NUM_ELEMS];

    /* test gpr_trigger */
    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_trigger_t);
        if (i % 2) {
            src[i]->name = strdup("dummy-name");
        }
        src[i]->id = (orte_gpr_trigger_id_t)i;
        src[i]->action = (orte_gpr_trigger_action_t)0x0f;

        /* test value counts of 1 to NUM_ELEMS+1 */
        src[i]->cnt = i + 1;
        src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));

        for (k=0; k < src[i]->cnt; k++) {
            src[i]->values[k] = OBJ_NEW(orte_gpr_value_t);
            src[i]->values[k]->addr_mode = (uint16_t) i;
            src[i]->values[k]->segment = strdup("test-segment");

            /* test token counts of 0! to NUM_ELEMS */
            src[i]->values[k]->num_tokens = i;
            if (src[i]->values[k]->num_tokens) { /* if to allow testing of token count of zero */
                src[i]->values[k]->tokens = (char**)malloc(src[i]->values[k]->num_tokens * sizeof(char*));
                for (j=0; j < src[i]->values[k]->num_tokens; j++) {
                    src[i]->values[k]->tokens[j] = strdup("test-token");
                }
            }

            /* test key counts of 0 to NUM_ELEMS */
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

    /* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }


    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_GPR_TRIGGER);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "test_trig pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dss.unpack(bufA, dst, &count, ORTE_GPR_TRIGGER);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "test_trig unpack failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {

            if ((NULL == src[j]->name && NULL != dst[j]->name) ||
                 (NULL != src[j]->name && NULL == dst[j]->name)
               ) {
                fprintf(test_out, "test_trig invalid results from unpack\n");
                return(false);
               }

               if ((NULL != src[j]->name &&
                    0 != strcmp(src[j]->name, dst[j]->name)) ||
                    src[j]->id != dst[j]->id ||
                    src[j]->action != dst[j]->action ||
                    src[j]->cnt != dst[j]->cnt
                  ) {
                   fprintf(test_out, "test_trig: invalid results from unpack\n");
                   return(false);
                  }

                  /* now compare each of the size/cnt dependent values */
                  for (k=0; k<src[j]->cnt; k++) {
                      if (src[j]->values[k]->num_tokens != dst[j]->values[k]->num_tokens) {
                          fprintf(test_out, "test_trig: invalid results (value num_tokens) from unpack\n");
                          return(false);
                      }

                      for (m=0; m < src[j]->values[k]->num_tokens; m++) {
                          if (0 != strcmp(src[j]->values[k]->tokens[m], dst[j]->values[k]->tokens[m])) {
                              fprintf(test_out, "test_trig: invalid results (tokens) from unpack\n");
                              return(false);
                          }
                      }

                      if (src[j]->values[k]->cnt != dst[j]->values[k]->cnt) {
                          fprintf(test_out, "test_trig: invalid results (value cnt) from unpack\n");
                          return(false);
                      }

                      for (m=0; m < src[j]->values[k]->cnt; m++) {
                          if (0 != strcmp(src[j]->values[k]->keyvals[m]->key,
                              dst[j]->values[k]->keyvals[m]->key)) {
                                  fprintf(test_out, "test_trig: invalid results (keys) from unpack\n");
                                  return(false);
                              }
                      }
                  }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}


static bool test_notify_data(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k, l, n;
    orte_data_value_t *sdv, *ddv;
    int32_t i32;
    orte_gpr_value_t *value, **sval, **dval;
    orte_gpr_notify_data_t *src[NUM_ELEMS];
    orte_gpr_notify_data_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_notify_data_t);
        if (i % 2) { /* test half with a name and with remove=true */
            src[i]->target = strdup("test-notify-data-name");
            src[i]->remove = true;
        }
        src[i]->id = i;

        /* test value counts of 0 to NUM_ELEMS-1 */
        src[i]->cnt = i; /* value count */

        for (j=0; j < src[i]->cnt; j++) {
            value = OBJ_NEW(orte_gpr_value_t);
            value->addr_mode = (orte_gpr_addr_mode_t) i+j+1;
            value->segment = strdup("test-gpr-notify-value-segment-name");  /* ek segment name again! */

            /* tokens */
            value->num_tokens = j; /* test tokens within gpr values within notify message between 0-NUM_ELEMS-1 */
            if (value->num_tokens) { /* if to allow testing of num_tokens count of zero */
                value->tokens = (char**)malloc(value->num_tokens * sizeof(char*));
                for (k=0; k < value->num_tokens; k++) {
                    value->tokens[k] = strdup("test-grp-notify-value-token");
                } /* for each token */
            } /* if tokens */

            /* keyval pairs (field name is 'cnt' same as used for value count so be careful) */
            value->cnt = j; /* test keyval pairs within gpr values within notify message between 0-NUM_ELEMS-1 */
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

    /* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }


    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_GPR_NOTIFY_DATA);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "test_notify_data pack failed with return code %d\n", rc);
            return(false);
        }
    }


    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dss.unpack(bufA, dst, &count, ORTE_GPR_NOTIFY_DATA);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "test_notify_data unpack failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {

            if (
                src[j]->id != dst[j]->id ||
                src[j]->cnt != dst[j]->cnt ||
                src[j]->remove != dst[j]->remove
               ) {
                fprintf(test_out, "test_notify_data: invalid results from unpack\n");
                return(false);
               }

            if ((NULL == src[j]->target && NULL != dst[j]->target) ||
                (NULL != src[j]->target && NULL == dst[j]->target)) {
                fprintf(test_out, "test_notify_data failed with mismatched NULL targets\n");
                return(false);
                }

            if (NULL != src[j]->target && NULL != dst[j]->target &&
                0 != strcmp(src[j]->target, dst[j]->target)) {
                fprintf(test_out, "test_notify_data failed with mismatched target names\n");
                return(false);
                }

            /* now compare each value of the cnt depedant values */
            sval = (orte_gpr_value_t**)(src[j]->values)->addr;
            dval = (orte_gpr_value_t**)(dst[j]->values)->addr;
/* because of the way this has been done, we can safely assume
            * that these are in the same relative order
*/
            for (k=0, n=0; n < src[j]->cnt &&
                k < (src[j]->values)->size; k++) {
                    if (NULL != sval[k]) {
                        n++;

                        if (sval[k]->addr_mode != dval[k]->addr_mode) {
                            fprintf(test_out, "test_notify_data: invalid results (values-addr-mode) from unpack\n");
                            return(false);
                        }

                        if (0 != strcmp(sval[k]->segment, dval[k]->segment)) {
                            fprintf(test_out, "test_notify_data: invalid results (values-segment) from unpack\n");
                            return(false);
                        }

                        if (sval[k]->num_tokens != dval[k]->num_tokens) {
                            fprintf(test_out, "test_notify_data: invalid results (values-num_tokens) from unpack\n");
                            return(false);
                        }
                        for (l=0; l<sval[k]->num_tokens; l++) {
                            if (0 != strcmp(sval[k]->tokens[l], dval[k]->tokens[l])) {
                                fprintf(test_out, "test_notify_data: invalid results (values-tokens) from unpack\n");
                                return(false);
                            }
                        } /* for each token inside each grp value */

                        if (sval[k]->cnt != dval[k]->cnt) {
                            fprintf(test_out, "test_notify_data: invalid results (values-cnt (of keyval pairs)) from unpack\n");
                            return(false);
                        }
                        for (l=0; l< sval[k]->cnt; l++) {
                            if (0 != strcmp(sval[k]->keyvals[l]->key, dval[k]->keyvals[l]->key)) {
                                fprintf(test_out, "test_notify_data: invalid results (values-keyvals-key) from unpack\n");
                                return(false);
                            }
                            sdv = sval[k]->keyvals[l]->value;
                            ddv = dval[k]->keyvals[l]->value;
                            if (sdv->type != ddv->type) {
                                fprintf(test_out, "test_notify_data: invalid results (values-keyvals-type) from unpack\n");
                                return(false);
                            }
                            if (ORTE_EQUAL != orte_dss.compare(sdv->data, ddv->data, sdv->type)) {
                                fprintf(test_out, "test_notify_data: invalid results (values-keyvals-value.i32) from unpack\n");
                                return(false);
                            }
                        }/* for each keyvalpair inside each grp value */
                    } /* for each grp value */
                }

        } /* for each ELEMENT */
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

static bool test_notify_msg(void)
{
    return (true);
}

