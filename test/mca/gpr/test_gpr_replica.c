/*
 * unit test for General Purpose Registry replica.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include <stdio.h>
#include <string.h>

#include "ompi_config.h"
#include "support.h"

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/gpr_replica.h"
#include "mca/gpr/replica/gpr_replica_internals.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

int main(int argc, char **argv)
{
    mca_gpr_replica_key_t test_key, test_key2;
    bool multi, hidden;
    int i, j;
    char *tmp;
    int result; /* result from system call */

    test_init("test_gpr_replica");

    test_out = fopen( "test_gpr_replica_out", "w+" );
    if( test_out == NULL ) {
      test_failure("test_gpr_replica couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    ompi_process_info.seed = true;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
	fprintf(test_out, "MCA started\n");
    } else {
	fprintf(test_out, "MCA could not start - please report error to bugs@open-mpi.org\n");
	exit (1);
    }

    /* open the GPR */
    if (OMPI_SUCCESS == mca_gpr_base_open()) {
	fprintf(test_out, "GPR opened\n");
	test_success();
    } else {
	fprintf(test_out, "GPR could not open\n");
        test_failure("test_gpr_replica mca_gpr_base_open failed");
        test_finalize();
	exit(1);
    }

    /* startup the GPR replica */
    if (OMPI_SUCCESS != mca_gpr_base_select(&multi, &hidden)) {
	fprintf(test_out, "GPR replica could not start\n");
	test_failure("test_gpr_replica mca_gpr_base_select failed");
        test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "GPR replica started\n");
	test_success();
    }

    /* define a key */
    test_key = gpr_replica_define_key("universe", NULL);
    if (0 == test_key) {
	fprintf(test_out, "GPR replica: failed define key - %d\n", test_key);
	test_failure("test_gpr_replica define_key failed");
	test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "GPR replica: define_key succeeded - %d\n", test_key);
	test_success();
    }

    /* get a key - check for correctness */
    test_key2 = gpr_replica_get_key("universe", NULL);
    if (test_key != test_key2) {
	fprintf(test_out, "GPR replica: mismatched keys - %d %d\n", test_key, test_key2);
	test_failure("test_gpr_replica get_key failed");
	test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "GPR replica: get_key succeeded\n");
	test_success();
    }


    fclose( test_out );
    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_ns_replica ompi_name_server get_proc_name_string, etc failed");
    }

    test_finalize();

    return(0);
}
