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
    ompi_list_t *test_list, *internal_tests;
    ompi_registry_index_value_t *ptr;
    ompi_registry_internal_test_results_t *ptri;
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

    /* try to define a segment */
    if (OMPI_SUCCESS != ompi_registry.define_segment("test-segment")) {
	fprintf(test_out, "GPR replica: could not define segment\n");
	test_failure("test_gpr_replica define_segment failed");
	test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "GPR test segment defined\n");
	test_success();
    }

    /* check index */
    test_list = ompi_registry.index(NULL);
    if (0 == ompi_list_get_size(test_list)) { /* should have been something in dictionary */
	fprintf(test_out, "GPR replica: index function failed\n");
	test_failure("test_gpr_replica index_global_dictionary failed\n");
	test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "GPR index returned list\n");
	for (ptr = (ompi_registry_index_value_t*)ompi_list_get_first(test_list);
	     ptr != (ompi_registry_index_value_t*)ompi_list_get_end(test_list);
	     ptr = (ompi_registry_index_value_t*)ompi_list_get_next(ptr)) {
	    fprintf(test_out, "\t%s\n", ptr->token);
	}
	test_success();
    }


    /* check internals */
    internal_tests = ompi_registry.test_internals(1);
    if (0 == ompi_list_get_size(internal_tests)) { /* should have been something in dictionary */
	fprintf(test_out, "internal tests failed\n");
	test_failure("test_gpr_replica internal_tests failed\n");
	test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "internal test results list\n");
	for (ptri = (ompi_registry_internal_test_results_t*)ompi_list_get_first(internal_tests);
	     ptri != (ompi_registry_internal_test_results_t*)ompi_list_get_end(internal_tests);
	     ptri = (ompi_registry_internal_test_results_t*)ompi_list_get_next(ptri)) {
	    fprintf(test_out, "\t%s\n", ptri->test);
	    fprintf(test_out, "\t%s\n", ptri->message);
	}
	test_success();
    }


    /* check the universe segment - should have a key value of "1" */

    fclose( test_out );
    /*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_gpr_replica ompi_registry init, etc failed");
    }
    */
    test_finalize();

    return(0);
}
