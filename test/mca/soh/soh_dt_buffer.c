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

#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/soh/base/base.h"

#define NUM_ITERS 3
#define NUM_ELEMS 10

static bool test_unstruct(void);      /* size */

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

    /* startup the name service to register data types */
    if (ORTE_SUCCESS == orte_soh_base_open()) {
        fprintf(stderr, "SOH opened\n");
    } else {
        fprintf(stderr, "SOH could not open\n");
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

    orte_soh_base_close();
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
    int rc;
    orte_exit_code_t sec[NUM_ELEMS], dec[NUM_ELEMS];
    orte_proc_state_t sps[NUM_ELEMS], dps[NUM_ELEMS];
    orte_job_state_t sjs[NUM_ELEMS], djs[NUM_ELEMS];
    orte_node_state_t sns[NUM_ELEMS], dns[NUM_ELEMS];
    int i;

    for (i=0; i < NUM_ELEMS; i++) {
        sec[i] = i % 256;
        sps[i] = (i+1)%256;
        sjs[i] = (i+2)%256;
        sns[i] = (i+3)%256;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(stderr, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, sec, NUM_ELEMS, ORTE_EXIT_CODE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, sps, NUM_ELEMS, ORTE_PROC_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, sjs, NUM_ELEMS, ORTE_JOB_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, sns, NUM_ELEMS, ORTE_NODE_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dss.unpack(bufA, dec, &count, ORTE_EXIT_CODE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dps, &count, ORTE_PROC_STATE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, djs, &count, ORTE_JOB_STATE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dns, &count, ORTE_NODE_STATE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(sec[j] != dec[j] ||
               sps[j] != dps[j] ||
               sjs[j] != djs[j] ||
               sns[j] != dns[j]) {
                fprintf(stderr, "test5: invalid results from unpack\n");
                return(false);
               }
        }
    }

    /* cleanup */

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(stderr, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return(true);
}
