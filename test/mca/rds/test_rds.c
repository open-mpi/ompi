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

#include "runtime/runtime.h"
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

    /* startup the runtime */
    if (OMPI_SUCCESS == orte_init()) {
        fprintf(test_out, "orte_init completed\n");
    } else {
        fprintf(test_out, "orte_init failed\n");
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
    
    /* finalize */
    orte_finalize();
    
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
