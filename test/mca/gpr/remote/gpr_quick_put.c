/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/runtime/runtime.h"

#include "orte/mca/gpr/gpr.h"

int main(int argc, char **argv)
{
    int rc;
    size_t i;
    char *tokens[5], *keys[5];
    orte_data_value_t value = ORTE_DATA_VALUE_EMPTY;
    orte_data_value_t *values[5];
    int32_t i32;
    int16_t i16;

    /* initialize system */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        fprintf(stderr, "couldn't complete init of system - error code %d\n", rc);
        exit(1);
    }

    tokens[0] = strdup("test-token-1");
    tokens[1] = strdup("test-token-2");
    tokens[2] = NULL;
    i32 = 123456;
    value.type = ORTE_INT32;
    value.data = &i32;
    fprintf(stderr, "quick-put one value with single keyval\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(ORTE_GPR_TOKENS_AND,
                            "test-put-segment", tokens,
                            "test-key", &value))) {
        fprintf(stderr, "gpr_test: put of 1 value/1 keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: quick-put of 1 value/1 keyval passed\n");
    }
    free(tokens[0]);
    free(tokens[1]);

    for (i=0; i < 4; i++) {
        asprintf(&tokens[i], "test-token-%lu", (unsigned long)i);
        asprintf(&keys[i], "test-keys-%lu", (unsigned long)i);
        values[i] = OBJ_NEW(orte_data_value_t);
        values[i]->type = ORTE_INT16;
        i16 = i * 1000;
        orte_dss.copy(&(values[i]->data), &i16, ORTE_INT16);
    }
    tokens[4] = NULL;
    keys[4] = NULL;
    fprintf(stderr, "quick-put one value with multiple keyvals\n");
    if (ORTE_SUCCESS != (rc = orte_gpr.put_N(ORTE_GPR_TOKENS_AND,
                            "test-put-segment23", tokens, 4,
                            keys, values))) {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval failed with error code %d\n", rc);
        return rc;
    } else {
        fprintf(stderr, "gpr_test: put 1 value/multiple keyval passed\n");
    }

    orte_gpr.dump_segment(NULL, 0);

    fprintf(stderr, "now finalize and see if all memory cleared\n");
    if (ORTE_SUCCESS != (rc = orte_finalize())) {
        fprintf(stderr, "couldn't complete finalize - error code %d\n", rc);
        exit(1);
    }

    return(0);
}
