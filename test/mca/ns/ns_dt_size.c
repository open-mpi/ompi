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
    int rc;
    orte_process_name_t src;
    size_t sz;

    src.cellid = 1000;
    src.jobid = 100;
    src.vpid = 150;

    rc = orte_dss.size(&sz, &src, ORTE_NAME);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_process_name_t)) {
        fprintf(stderr, "orte_dss.size: invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, NULL, ORTE_NAME);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size with NULL failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_process_name_t)) {
        fprintf(stderr, "orte_dss.size: with NULL invalid results\n");
        return(false);
    }

    return(true);
}

static bool test_unstruct(void)
{
    int rc;
    orte_cellid_t scell;
    orte_vpid_t svpid;
    orte_jobid_t sjobid;
    size_t sz;

    scell = 1000;
    svpid = 208;
    sjobid = 5;

    rc = orte_dss.size(&sz, &scell, ORTE_CELLID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_cellid_t)) {
        fprintf(stderr, "orte_dss.size: invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, NULL, ORTE_CELLID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size with NULL failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_cellid_t)) {
        fprintf(stderr, "orte_dss.size: with NULL invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, &sjobid, ORTE_JOBID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_jobid_t)) {
        fprintf(stderr, "orte_dss.size: invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, NULL, ORTE_JOBID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size with NULL failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_jobid_t)) {
        fprintf(stderr, "orte_dss.size: with NULL invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, &svpid, ORTE_VPID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_vpid_t)) {
        fprintf(stderr, "orte_dss.size: invalid results\n");
        return(false);
    }

    rc = orte_dss.size(&sz, NULL, ORTE_VPID);
    if (ORTE_SUCCESS != rc) {
        fprintf(stderr, "orte_dss.size with NULL failed with return code %d\n", rc);
        return(false);
    }
    if(sz != sizeof(orte_vpid_t)) {
        fprintf(stderr, "orte_dss.size: with NULL invalid results\n");
        return(false);
    }

    return(true);
}
