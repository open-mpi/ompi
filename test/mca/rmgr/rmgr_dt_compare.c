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
    int n, rc, count;
    size_t i, j;
    orte_app_context_t *src[NUM_ELEMS];
    orte_app_context_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_app_context_t);
        src[i]->idx = i;
        src[i]->app = strdup("test-application-name");
        src[i]->num_procs = i; /* test between 0 and NUM_ELEMS-1 proc counts */

        /* test arg counts of 1 to NUM_ELEMS+1 */
        count = i+1;
        if (count) { /* if to allow testing of argv count of zero */
            src[i]->argv = (char**)malloc(count * sizeof(char*));
            for (n=0; n < count-1; n++) {
                src[i]->argv[n] = strdup("test-argv");
            }
        }
        src[i]->argv[count-1] = NULL;

        /* test env counts of 1 to NUM_ELEMS+1 */
        count = i+1;
        if (count) { /* if to allow testing of num_env count of zero */
            src[i]->env = (char**)malloc(count * sizeof(char*));
            for (n=0; n < count; n++) {
                src[i]->env[n] = strdup("test-env");
            }
        }
        src[i]->env[count-1] = NULL;

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

    /* source data set, now copy them */


    for (i=0;i<NUM_ELEMS;i++) {
        rc = orte_dss.copy((void**)&dst[i], src[i], ORTE_APP_CONTEXT);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.copy failed with return code %d\n", rc);
            return(false);
        }
    }

    for(j=0; j<NUM_ELEMS; j++) {

        if (ORTE_EQUAL != orte_dss.compare(dst[j], src[j], ORTE_APP_CONTEXT)) {
            fprintf(stderr, "orte_dss.compare failed\n");
            return(false);
        }
    }

    return (true);
}
