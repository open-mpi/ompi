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

#include "orte/include/orte_constants.h"

#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"

#define NUM_ITERS 3
#define NUM_ELEMS 10

static bool test_name(void);        /* buffer actions */
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
    if (ORTE_SUCCESS == orte_ns_base_open()) {
        fprintf(stderr, "NS opened\n");
    } else {
        fprintf(stderr, "NS could not open\n");
        exit (1);
    }


    /* Now do the tests */

    fprintf(stderr, "executing test_name\n");
    if (test_name()) {
        fprintf(stderr, "Test_name succeeded\n");
    }
    else {
        fprintf(stderr, "Test_name failed\n");
    }

    fprintf(stderr, "executing test_unstruct\n");
    if (test_unstruct()) {
        fprintf(stderr, "test_unstruct succeeded\n");
    }
    else {
        fprintf(stderr, "test_unstruct failed\n");
    }

    orte_ns_base_close();
    orte_dss_close();

    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return 0;
}

static bool test_name(void)        /* buffer actions */
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    orte_process_name_t src[NUM_ELEMS];
    orte_process_name_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i].cellid = 1000+i;
        src[i].jobid = 100+i;
        src[i].vpid = i;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(stderr, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_NAME);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dss.unpack(bufA, dst, &count, ORTE_NAME);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j].cellid != dst[j].cellid ||
               src[j].jobid != dst[j].jobid ||
               src[j].vpid != dst[j].vpid) {
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

static bool test_unstruct(void)
{
    orte_buffer_t *bufA;
    int rc;
    orte_cellid_t scell[NUM_ELEMS], dcell[NUM_ELEMS];
    orte_vpid_t svpid[NUM_ELEMS], dvpid[NUM_ELEMS];
    orte_jobid_t sjobid[NUM_ELEMS], djobid[NUM_ELEMS];
    int i;

    for (i=0; i < NUM_ELEMS; i++) {
        scell[i] = i + 1000;
        svpid[i] = 208*i;
        sjobid[i] = i;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(stderr, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, scell, NUM_ELEMS, ORTE_CELLID);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, svpid, NUM_ELEMS, ORTE_VPID);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
        rc = orte_dss.pack(bufA, sjobid, NUM_ELEMS, ORTE_JOBID);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dss.unpack(bufA, dcell, &count, ORTE_CELLID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, dvpid, &count, ORTE_VPID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        rc = orte_dss.unpack(bufA, djobid, &count, ORTE_JOBID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(stderr, "orte_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(scell[j] != dcell[j] ||
               sjobid[j] != djobid[j] ||
               svpid[j] != dvpid[j]) {
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
