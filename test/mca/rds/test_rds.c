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
#include "util/malloc.h"
#include "util/output.h"

#include "mca/gpr/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/rds/base/base.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_replica_out ./test_ns_replica_out_std";


int
main(int argc, char **argv)
{
    int rc;
    bool allow, have;
    int priority;
    test_component_handle_t gpr_handle, ns_handle;
    mca_gpr_base_component_t *gpr_component = NULL;
    orte_gpr_base_module_t *gpr_module = NULL;
    mca_ns_base_component_t *ns_component = NULL;
    mca_ns_base_module_t *ns_module = NULL;
    test_component_handle_t rds_handle;
    orte_rds_base_component_t *rds_component = NULL;
    orte_rds_base_module_t *rds_module = NULL;
    
    test_init("test_rds_fn");

    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_rds_fn", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      test_failure("rds_fn couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* ENSURE THE GPR REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);
    
    /* ensure the NS replica is isolated */
    setenv("OMPI_MCA_ns_replica_isolate", "1", 1);
    
    /* Open up the output streams */
    if (!ompi_output_init()) {
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

    /* Open the ns replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("ns", "replica", &ns_handle, 
                            (mca_base_component_t**) &ns_component) ||
        NULL == ns_component) {
        test_fail_stop("Could not open ns replica\n", 1);
    }
    ns_module = ns_component->ns_init(&priority);
    if (NULL == ns_module) {
        test_fail_stop("NS replica component did not return a module\n", 1);
    }

     /* Open the gpr replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("gpr", "replica", &gpr_handle, 
                            (mca_base_component_t**) &gpr_component) ||
        NULL == gpr_component) {
        test_fail_stop("Could not open GPR replica\n", 1);
    }
    gpr_module = gpr_component->gpr_init(&allow, &have, &priority);
    if (NULL == gpr_module) {
        test_fail_stop("GPR replica did not return a component\n", 1);
    }
    fprintf(test_out, "GPR replica started\n");

    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        test_fail_stop("DPS could not start\n", 1);
    }

    /* setup the RDS */
    if (OMPI_SUCCESS != 
        test_component_open("rds", "hostfile", &rds_handle, 
                            (mca_base_component_t**) &rds_component) ||
        NULL == rds_component) {
        test_fail_stop("Could not open rds hostfile\n", 1);
    }
    rds_module = rds_component->rds_init();
    if (NULL == rds_module) {
        test_fail_stop("rds hostfile component did not return a module\n", 1);
    }
    
//    rds_module->query();

    fprintf(test_out, "RDS hostfile component executed\n");

    if (NULL != rds_module->finalize) {
        rds_module->finalize();
    }
    test_component_close(&rds_handle);

    fprintf(stderr, "test the resource file component\n");
    if (OMPI_SUCCESS != 
        test_component_open("rds", "resfile", &rds_handle, 
                            (mca_base_component_t**) &rds_component) ||
        NULL == rds_component) {
        test_fail_stop("Could not open rds resource file component\n", 1);
    }
    rds_module = rds_component->rds_init();
    if (NULL == rds_module) {
        test_fail_stop("rds resource file component did not return a module\n", 1);
    }

    rds_module->query();

    fprintf(test_out, "RDS resource file component executed\n");
    
    gpr_module->dump_segments(0);

    if (NULL != rds_module->finalize) {
        rds_module->finalize();
    }
    test_component_close(&rds_handle);

    orte_dps_close();

    if (NULL != gpr_component->gpr_finalize) {
        gpr_component->gpr_finalize();
    }
    test_component_close(&gpr_handle);

    orte_sys_info_finalize();
    orte_proc_info_finalize();
    mca_base_close();
    ompi_malloc_finalize();
    ompi_output_finalize();
    ompi_class_finalize();
    
    fclose( test_out );
/*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_rds failed");
    }
*/
    test_finalize();

    exit (0);
}
