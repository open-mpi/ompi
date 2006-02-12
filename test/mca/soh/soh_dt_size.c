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
    int rc;
    orte_exit_code_t sec;
    orte_proc_state_t sps;
    orte_job_state_t sjs;
    orte_node_state_t sns;
    size_t sz;

        sec = 2;
        sps = 125;
        sjs = 117;
        sns = 0x7e;

        rc = orte_dss.size(&sz, &sec, ORTE_EXIT_CODE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
            return(false);
        }
        if (sz != sizeof(orte_exit_code_t)) {
            fprintf(stderr, "orte_dss.size failed with incorrect value (exit code)\n");
            return(false);
        }

        rc = orte_dss.size(&sz, &sps, ORTE_PROC_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
            return(false);
        }
        if (sz != sizeof(orte_proc_state_t)) {
            fprintf(stderr, "orte_dss.size failed with incorrect value (ORTE_PROC_STATE)\n");
            return(false);
        }

        rc = orte_dss.size(&sz, &sjs, ORTE_JOB_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
            return(false);
        }
        if (sz != sizeof(orte_job_state_t)) {
            fprintf(stderr, "orte_dss.size failed with incorrect value (ORTE_JOB_STATE)\n");
            return(false);
        }

        rc = orte_dss.size(&sz, &sns, ORTE_NODE_STATE);
        if (ORTE_SUCCESS != rc) {
            fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
            return(false);
        }
        if (sz != sizeof(orte_node_state_t)) {
            fprintf(stderr, "orte_dss.size failed with incorrect value (ORTE_NODE_STATE)\n");
            return(false);
        }

    return(true);
}
