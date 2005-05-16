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

#include "runtime/runtime.h"
#include "mca/gpr/gpr.h"
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

    orte_init();

    orte_rds_base_query();
    
    orte_gpr.dump_segments(0);

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
