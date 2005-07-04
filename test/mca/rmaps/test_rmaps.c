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

#include "orte_config.h"
#include <stdio.h>
#include <string.h>

#include "support.h"
#include "components.h"

#include "include/orte_constants.h"

#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "mca/gpr/base/base.h"
#include "mca/rmaps/base/base.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_replica_out ./test_ns_replica_out_std";


int
main(int argc, char **argv)
{
    int rc;
    bool allow, have;
    int priority;
    test_component_handle_t gpr_handle;
    mca_gpr_base_component_t *gpr_component = NULL;
    orte_gpr_base_module_t *gpr_module = NULL;
    test_component_handle_t rmaps_handle;
    orte_rmaps_base_component_t *rmaps_component = NULL;
    orte_rmaps_base_module_t *rmaps_module = NULL;
    
    test_init("test_rmaps_fn");

    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_rmaps_fn", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      test_failure("rmaps_fn couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* ENSURE THE GPR REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);
    
    /* Open up the output streams */
    if (!opal_output_init()) {
        return OMPI_ERROR;
    }

    /* 
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    opal_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    ompi_malloc_init();

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (rc = orte_sys_info())) {
        return rc;
    }

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
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    /* Open the gpr replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("gpr", "replica", &gpr_handle, 
                            (mca_base_component_t**) &gpr_component) ||
        NULL == gpr_component) {
        test_fail_stop("Could not open replica\n", 1);
    }
    gpr_module = gpr_component->gpr_init(&allow, &have, &priority);
    if (NULL == gpr_module) {
        test_fail_stop("replica component did not return a module\n", 1);
    }
    fprintf(test_out, "GPR replica started\n");

    /* dps */
    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* setup the RMAPS */
    if (OMPI_SUCCESS != 
        test_component_open("rmaps", "round_robin", &rmaps_handle, 
                            (mca_base_component_t**) &rmaps_component) ||
        NULL == rmaps_component) {
        test_fail_stop("Could not open rmaps round_robin component\n", 1);
    }
    rmaps_module = rmaps_component->rmaps_init(&priority);
    if (NULL == rmaps_module) {
        test_fail_stop("rmaps round_robin component did not return a module\n", 1);
    }
    fprintf(test_out, "RMAPS host component started\n");
    
    fprintf(stderr, "now finalize and see if all memory cleared\n");

    if (NULL != rmaps_module->finalize) {
        rmaps_module->finalize();
    }
    test_component_close(&rmaps_handle);

    orte_dps_close();

    if (NULL != gpr_component->gpr_finalize) {
        gpr_component->gpr_finalize();
    }
    test_component_close(&gpr_handle);

    orte_sys_info_finalize();
    orte_proc_info_finalize();
    mca_base_close();
    ompi_malloc_finalize();
    opal_output_finalize();
    ompi_class_finalize();
    
    fclose( test_out );
/*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_rmaps failed");
    }
*/
    test_finalize();

    exit (0);
}
