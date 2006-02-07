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

#include "opal/runtime/opal.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/include/orte_constants.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/rmgr/base/base.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"

#include "orte/mca/gpr/base/base.h"
#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

int main(int argc, char **argv)
{
    int rc;
    size_t i;
    char *tokens[5], *keys[5];
    orte_data_value_t value = ORTE_DATA_VALUE_EMPTY;
    orte_data_value_t *values[5];
    int32_t i32;
    int16_t i16;
    
    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);


    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (rc = orte_proc_info())) {
        return rc;
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

    /* ENSURE THE REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);

    /* startup the gpr to register data types */
    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(stderr, "GPR opened\n");
    } else {
        fprintf(stderr, "GPR could not open\n");
        exit (1);
    }

    /* do a select on the registry components */
    if (OMPI_SUCCESS == orte_gpr_base_select()) {
        fprintf(stderr, "GPR selected\n");
    } else {
        fprintf(stderr, "GPR could not select\n");
        exit (1);
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
    orte_gpr_base_close();

    orte_dss_close();
    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return(0);
}
