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

static bool test_name(void);        /* buffer actions */
static bool test_unstruct(void);      /* size */

int main(int argc, char **argv)
{
    int ret;

    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    stderr = stderr;

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
    orte_process_name_t src;
    char *output;

    src.cellid = 1000;
    src.jobid = 100;
    src.vpid = 150;

    if (ORTE_SUCCESS != orte_dss.print(&output, NULL, &src, ORTE_NAME)) {
        fprintf(stderr, "orte_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(stderr, "orte_dss.print failed for ORTE_NAME value\n");
        return(false);
    }
    fprintf(stderr, "orte_dss.print output for ORTE_NAME value: %s\n", output);
    free(output);

    return(true);
}

static bool test_unstruct(void)
{
    orte_cellid_t scell;
    orte_vpid_t svpid;
    orte_jobid_t sjobid;
    char *output;

    scell = 1000;
    svpid = 208;
    sjobid = 5;

    if (ORTE_SUCCESS != orte_dss.print(&output, NULL, &scell, ORTE_CELLID)) {
        fprintf(stderr, "orte_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(stderr, "orte_dss.print failed for ORTE_CELLID value\n");
        return(false);
    }
    fprintf(stderr, "orte_dss.print output for ORTE_CELLID value: %s\n", output);
    free(output);

    if (ORTE_SUCCESS != orte_dss.print(&output, NULL, &sjobid, ORTE_JOBID)) {
        fprintf(stderr, "orte_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(stderr, "orte_dss.print failed for ORTE_JOBID value\n");
        return(false);
    }
    fprintf(stderr, "orte_dss.print output for ORTE_JOBID value: %s\n", output);
    free(output);

    if (ORTE_SUCCESS != orte_dss.print(&output, NULL, &svpid, ORTE_VPID)) {
        fprintf(stderr, "orte_dss.print returned error\n");
        return(false);
    }
    if (NULL == output) {
        fprintf(stderr, "orte_dss.print failed for ORTE_VPID value\n");
        return(false);
    }
    fprintf(stderr, "orte_dss.print output for ORTE_VPID value: %s\n", output);
    free(output);

    return(true);
}
