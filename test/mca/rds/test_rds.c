/*
 * unit test for RDS resource file parser

 --------------------------------------------------------------------------

 Authors:    Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include "orte_config.h"
#include <stdio.h>
#include <string.h>

#include "support.h"

#include "include/orte_constants.h"

#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/environ.h"

#include "mca/gpr/base/base.h"
#include "mca/rds/base/base.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_replica_out ./test_ns_replica_out_std";


int
main(int argc, char **argv)
{
    int rc;
    
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
    ompi_setenv("OMPI_MCA_gpr_replica_isolate", "1", true, &environ);
    
    /* Open up the output streams */
    if (!ompi_output_init()) {
        return OMPI_ERROR;
    }
                                                                                                                   
    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);
                                                                                                                   
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

    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(test_out, "GPR started\n");
    } else {
        fprintf(test_out, "GPR could not start\n");
        exit (1);
    }
    
    if (ORTE_SUCCESS == orte_gpr_base_select()) {
        fprintf(test_out, "GPR replica selected\n");
    } else {
        fprintf(test_out, "GPR replica could not be selected\n");
        exit (1);
    }
                  
    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* setup the RDS */
    if (ORTE_SUCCESS == orte_rds_base_open()) {
        fprintf(test_out, "RDS started\n");
    } else {
        fprintf(test_out, "RDS could not start\n");
        exit (1);
    }
    
    if (ORTE_SUCCESS == orte_rds_base_select()) {
        fprintf(test_out, "RDS selected\n");
    } else {
        fprintf(test_out, "RDS could not be selected\n");
        exit (1);
    }
                  
    /* run the query */
    if (ORTE_SUCCESS != (rc = orte_rds_base_query())) {
        fprintf(test_out, "RDS query failed with code %s\n",
                                ORTE_ERROR_NAME(rc));
        test_failure("test_rds_fn rds_query failed");
        test_finalize();
        exit(1);
    } else {
        fprintf(test_out, "RDS query succeeded\n");
        test_success();
    }
    
    fprintf(stderr, "now finalize and see if all memory cleared\n");
    orte_rds_base_close();
    orte_dps_close();
    orte_gpr_base_close();
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
