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
 * The OpenRTE Resource Manager data type function unit test
 *
 */

/*
 * includes
 */

#include "orte_config.h"
#include <stdio.h>
#include <string.h>

#include "orte/orte_constants.h"

#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/argv.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmgr/base/base.h"

#define NUM_ITERS 3
#define NUM_ELEMS 10

static bool test_ac(void);        /* includes app_context_map */

int main(int argc, char **argv)
{
    int ret;

    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

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
    if (OPAL_SUCCESS == mca_base_open()) {
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

    /* startup the rmgr to register data types */
    if (ORTE_SUCCESS == orte_rmgr_base_open()) {
        fprintf(stderr, "RMGR opened\n");
    } else {
        fprintf(stderr, "RMGR could not open\n");
        exit (1);
    }

    /* Now do the tests */

    fprintf(stderr, "executing test_ac\n");
    if (test_ac()) {
        fprintf(stderr, "test_ac succeeded\n");
    }
    else {
        fprintf(stderr, "test_ac failed\n");
    }
    orte_dss_close();

    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return 0;
}

static bool test_ac(void)        /* buffer actions */
{
    orte_buffer_t *bufA;
    int rc, n, count;
    size_t i, j, k;
    orte_app_context_t *src[NUM_ELEMS];
    orte_app_context_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_app_context_t);
        src[i]->idx = i;
        src[i]->app = strdup("test-application-name");
        src[i]->num_procs = i; /* test between 0 and NUM_ELEMS-1 proc counts */

        /* test arg counts of 1 to NUM_ELEMS+1 */
        count = i;
        if (count) { /* if to allow testing of argv count of zero */
            src[i]->argv = (char**)malloc((count+1) * sizeof(char*));
            for (n=0; n < count-1; n++) {
                src[i]->argv[n] = strdup("test-argv");
            }
            src[i]->argv[count] = NULL; /* be sure to null terminate the list */
        }

        /* test env counts of 1 to NUM_ELEMS+1 */
        count = i+1;
        if (count) { /* if to allow testing of num_env count of zero */
            src[i]->env = (char**)malloc((count+1) * sizeof(char*));
            for (n=0; n < count-1; n++) {
                src[i]->env[n] = strdup("test-env");
            }
            src[i]->env[count] = NULL; /* be sure to null terminate the list */
        }

        src[i]->cwd = strdup ("test-cwd");

        /* test imap data for map count = num_procs  */
        src[i]->num_map = i+1;
        if (src[i]->num_map) { /* if to allow testing of map count of zero */
            src[i]->map_data = (orte_app_context_map_t**)malloc(src[i]->num_map * sizeof(orte_app_context_map_t *));    /* map data type */
            for (j=0; j < src[i]->num_map; j++) {
                src[i]->map_data[j] = OBJ_NEW(orte_app_context_map_t);  /* assume we create with new rather than malloc? */
                src[i]->map_data[j]->map_type = (uint8_t) j;
                src[i]->map_data[j]->map_data = strdup("test-map-data");
            }
        }
    }

    /* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(stderr, "orte_buffer failed init in OBJ_NEW");
        fprintf(stderr, "OBJ_NEW failed\n");
        return false;
    }

    /* fprintf(stderr,"New buffer ready\n"); */
    /* fflush(stderr); */

    /*  orte_dss_dump_buffer_simple (bufA, 0); */


    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_APP_CONTEXT);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed");
            fprintf(stderr, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
        /*      fprintf(stderr,"Packed iter %d\n", i); */
        /*      fflush(stderr); */
        /*      orte_dss_dump_buffer_simple (bufA, 0); */
    }

    for (i=0; i<NUM_ITERS; i++) {
        count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dss.unpack(bufA, dst, &j, ORTE_APP_CONTEXT);
        if (ORTE_SUCCESS != rc || j != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed");
            fprintf(stderr, "orte_unpack_value failed with return code %d (count=%lu)\n", rc, (unsigned long) j);
            return(false);
        }

        /*      fprintf(stderr,"Unpacked iter %d\n", i); */
        /*      fflush(stderr); */
        /*      orte_dss_dump_buffer_simple (bufA, 0); */

        for(j=0; j<NUM_ELEMS; j++) {

            if (
                src[j]->idx != dst[j]->idx ||
                0 != strcmp(src[j]->app, dst[j]->app) ||
                src[j]->num_procs != dst[j]->num_procs ||
                0 != strcmp(src[j]->cwd, dst[j]->cwd) ||
                src[j]->num_map != dst[j]->num_map
               ) {
                fprintf(stderr, "test12: invalid results from unpack");
                return(false);
               }

               /* now compare each of the size/cnt depedant values */
               if (opal_argv_count(dst[j]->argv) != (count = opal_argv_count(src[j]->argv))) {
                   fprintf(stderr, "test12: invalid results (num argv) from unpack");
                   return(false);
               }
               for (n=0; n < count; n++) {
                   if (0 != strcmp(src[j]->argv[n], dst[j]->argv[n])) {
                       fprintf(stderr, "test12: invalid results (argv) from unpack");
                       return(false);
                   }
               }
               if (opal_argv_count(dst[j]->env) != (count = opal_argv_count(src[j]->env))) {
                   fprintf(stderr, "test12: invalid results (num env) from unpack");
                   return(false);
               }
               for (k=0; k < (size_t)count; k++) {
                   if (0 != strcmp(src[j]->env[k], dst[j]->env[k])) {
                       fprintf(stderr, "test12: invalid results (envs) from unpack");
                       return(false);
                   }
               }

               for (k=0; k< src[j]->num_map; k++) {
                   if ((src[j]->map_data[k])->map_type != (dst[j]->map_data[k])->map_type) {
                       fprintf(stderr, "test12: invalid results (map_data types) from unpack");
                       return(false);
                   }
                   if (0 != strcmp((src[j]->map_data[k])->map_data,
                       (dst[j]->map_data[k])->map_data)) {
                           fprintf(stderr, "test12: invalid results (map_data data) from unpack");
                           return(false);
                       }
               }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(stderr, "OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(stderr, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}
