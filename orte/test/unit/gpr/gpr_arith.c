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

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/gpr/base/base.h"

static bool test_add(void);
static bool test_sub(void);
static bool test_mult(void);
static bool test_div(void);

FILE *test_out;

int main(int argc, char **argv)
{
    int ret;

    orte_init(ORTE_INFRASTRUCTURE, ORTE_NON_BARRIER);

    /* Now do the tests */

    fprintf(stderr, "executing test_add\n");
    if (test_add()) {
        fprintf(stderr, "test_add succeeded\n");
    }
    else {
        fprintf(stderr, "test_add failed\n");
    }

    fprintf(stderr, "executing test_sub\n");
    if (test_sub()) {
        fprintf(stderr, "test_sub succeeded\n");
    }
    else {
        fprintf(stderr, "test_sub failed\n");
    }

    fprintf(stderr, "executing test_mult\n");
    if (test_mult()) {
        fprintf(stderr, "test_mult succeeded\n");
    }
    else {
        fprintf(stderr, "test_mult failed\n");
    }

    fprintf(stderr, "executing test_div\n");
    if (test_div()) {
        fprintf(stderr, "test_div succeeded\n");
    }
    else {
        fprintf(stderr, "test_div failed\n");
    }


    orte_dss_close();

    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return 0;
}

static bool test_add(void)
{
    orte_data_value_t dval = ORTE_DATA_VALUE_EMPTY;
    orte_std_cntr_t testval=5;
    char *tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *keys[] = {
        ORTE_JOB_SLOTS_KEY,
        NULL
    };
    int rc;
    
    /* increment a value that doesn't already exist */
    if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&testval, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return false;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.arith(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                             "test-seg", tokens, keys,
                                             ORTE_DSS_ADD, &dval))) {
        ORTE_ERROR_LOG(rc);
        return false;
    }
    
    orte_gpr.dump_segment("test-seg");
    
    /* increment the existing value */
    if (ORTE_SUCCESS != (rc = orte_gpr.arith(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                             "test-seg", tokens, keys,
                                             ORTE_DSS_ADD, &dval))) {
        ORTE_ERROR_LOG(rc);
        return false;
    }
    
    orte_gpr.dump_segment("test-seg");
    
    return (true);
}

static bool test_sub(void)
{
    return (true);
}


static bool test_mult(void)
{
   return(true);
}

static bool test_div(void)
{
   return (true);
}

