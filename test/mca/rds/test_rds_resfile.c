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
#include "include/constants.h"
#include "include/orte_names.h"

#include "../../../src/mca/rds/resfile/resfile.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_replica_out ./test_ns_replica_out_std";


int
main(int argc, char **argv)
{
    test_init("test_ns_replica");

    test_out = fopen( "test_ns_replica_out", "w+" );
    if( test_out == NULL ) {
      test_failure("test_ns_replica couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    /* open the rds framework */
    if (ORTE_SUCCESS == orte_rds_open()) {
        fprintf(test_out, "RDS opened\n");
    } else {
        fprintf(test_out, "RDS could not open\n");
        exit (1);
    }
    
    /* startup the rds */
    if (ORTE_SUCCESS != orte_rds_base_select(&multi, &hidden)) {
        fprintf(test_out, "RDS could not start\n");
        test_failure("test_rds_resfile orte_rds_base_select failed");
        test_finalize();
        exit(1);
    } else {
        fprintf(test_out, "RDS started\n");
        test_success();
    }

    /* setup the environment for the resfile component */
    setenv("OMPI_MCA_RESOURCE_FILE", "../../../src/etc/lanl_resources.xml");
    
    /* call the rds resfile component */
    if (ORTE_SUCCESS != (rc = orte_rds_resfile_query())) {
        fprintf(test_out, "RDS Resfile query failed with code %s\n",
                                ORTE_ERROR_NAME(rc));
        test_failure("test_rds_resfile rds_resfile_query failed");
        test_finalize();
        exit(1);
    } else {
        fprintf(test_out, "RDS resfile_query succeeded\n");
        test_success();
    }
    
}