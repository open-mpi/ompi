/*
 * unit test for gpr proxy.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>
            Tim S. Woodall <twoodall@lanl.gov>

 --------------------------------------------------------------------------

*/

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "support.h"

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "event/event.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/gpr_replica.h"
#include "mca/gpr/replica/gpr_replica_internals.h"


/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_proxy_out ./test_gpr_replica_out_std";
static int run_test(void);
static int exec_client(int argc, char** argv);


int main(int argc, char **argv)
{
    bool multi, hidden;
    char *seed;

    test_init("test_gpr_proxy");
    test_out = fopen( "test_ns_proxy_out", "w+" );
    if( test_out == NULL ) {
      test_failure("test_ns_proxy couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* setup environment for rte */
    seed = getenv("OMPI_DAEMON_SEED");
    setenv("OMPI_MCA_pcmclient_env_num_procs", "2", 1);
    setenv("OMPI_MCA_pcmclient_env_vpid_start", "0", 1);
    setenv("OMPI_MCA_pcmclient_env_cellid", "0", 1);
    setenv("OMPI_MCA_pcmclient_env_jobid", "0", 1);
    if(seed == NULL || atoi(seed) != 0) {
        ompi_process_info.seed = true;
        setenv("OMPI_MCA_pcmclient_env_procid", "0", 1);
    } else {
        ompi_process_info.seed = false;
        setenv("OMPI_MCA_pcmclient_env_procid", "1", 1);
    }
    /* require tcp oob */
    setenv("OMPI_MCA_oob_base_include", "tcp", 1);

    /* basic ompi init */
    if (OMPI_SUCCESS != ompi_init(argc, argv)) {
	    fprintf(test_out, "ompi_init failed - please report error to bugs@open-mpi.org\n");
	    exit (1);
    }

    /* initialize event library */
    if (OMPI_SUCCESS != ompi_event_init()) {
	    fprintf(test_out, "ompi_event_init failed - please report error to bugs@open-mpi.org\n");
	    exit (1);
    }

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
	    fprintf(test_out, "MCA started\n");
    } else {
	    fprintf(test_out, "MCA could not start - please report error to bugs@open-mpi.org\n");
	    exit (1);
    }

    /* initialize the rte - including ns and oob */
    if(OMPI_SUCCESS == ompi_rte_init(&multi, &hidden)) {
	    fprintf(test_out, "NS opened\n");
	    fprintf(test_out, "NS started\n");
    } else {
	    fprintf(test_out, "RTE could not start - please report error to bugs@open-mpi.org\n");
	    exit (1);
    }

    /* if daemon seed - just wait for requests */
    if(ompi_process_info.seed) {
#if 1
        /* wait on child to exit */
        int pid = exec_client(argc, argv);
        while(true) {
            int status;
            if(waitpid(pid, &status, WNOHANG) != 0)
                break;
            ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
        }
#else
        fprintf(stderr, "setenv OMPI_MCA_oob_base_seed %s\n", mca_oob_get_contact_info());
        ompi_event_loop(0);
#endif
        return(0);
    } else {
        return run_test();
    }
}
 

int exec_client(int argc, char** argv)
{
    int pid;
    pid = fork();
    if(pid < 0) {
	    fprintf(test_out, "fork() failed with errno=%d\n", errno);
        exit(1);
    } else if (pid == 0) {
        /* child process should not be a daemon */
        static char seed_flag[] = "OMPI_DAEMON_SEED=0";
        static char seed_addr[128];
        char* argp[] = {
            seed_flag,
            seed_addr,
            NULL
        };
        /* setup seed address for child process */
        sprintf(seed_addr, "OMPI_MCA_oob_base_seed=%s", mca_oob_get_contact_info());
        execve(argv[0], argv, argp);
    } 
    return pid;
}


int run_test()
{
    ompi_list_t *test_list, *internal_tests;
    ompi_registry_index_value_t *ptr;
    ompi_registry_internal_test_results_t *ptri;
    ompi_registry_object_t test_buffer;
    uint8_t *test_buf;
    ompi_registry_object_size_t input_size;
    ompi_registry_mode_t mode;
    ompi_list_t *answer;
    ompi_registry_value_t *ans;
    bool multi, hidden;
    int i, j;
    bool success;
    char name[30], *name2[30], *name3[30];
    int put_test; /* result from system call */

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

    /* check internals */
    internal_tests = ompi_registry.test_internals(1);
    if (0 == ompi_list_get_size(internal_tests)) { /* should have been something in list */
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


    /* test the put function */
    success = true;
    input_size = 10000;
    test_buffer = (ompi_registry_object_t)malloc(input_size);
    test_buf = (uint8_t*)test_buffer;
    for (i=0; i<input_size; i++) {
	*test_buf = i % 256;
	test_buf++;
    }
    for (j=0; j<10; j++) {
	asprintf(&name2[j], "test-key%d", j);
	name3[j] = strdup(name2[j]);
    }
    name2[j] = NULL;
    for (j=10; j<20; j++) {
	asprintf(&name3[j], "dummy-key%d", j);
    }
    name3[j] = NULL;

    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	if (OMPI_SUCCESS != ompi_registry.put(OMPI_REGISTRY_NONE, name,
					      name2, test_buffer, input_size)) {
	    fprintf(test_out, "put test failed for segment %s\n", name);
	    for (j=0; j<10; j++) {
		fprintf(test_out, "\t%s\n", name2[j]);
	    }
	    success = false;
	}
    }
    if (success) {
	fprintf(test_out, "put test: success\n");
	test_success();
    } else {
	fprintf(test_out, "put test failed\n");
	test_failure("test_gpr_replica put_test failed\n");
	test_finalize();
	exit(1);
    }

    /* test the put overwrite function */
    success = true;
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	if (10 %  i) {
	    mode = OMPI_REGISTRY_OVERWRITE;
	} else {
	    mode = OMPI_REGISTRY_NONE;
	}
	put_test = ompi_registry.put(mode, name, name2, test_buffer, input_size);
	if ((OMPI_REGISTRY_OVERWRITE == mode && OMPI_SUCCESS != put_test) ||
	    (OMPI_REGISTRY_NONE == mode && OMPI_SUCCESS == put_test)) {
	    fprintf(test_out, "put test failed for segment %s\n", name);
	    for (j=0; j<10; j++) {
		fprintf(test_out, "\t%s\n", name2[j]);
	    }
	    success = false;
	}
    }
    if (success) {
	fprintf(test_out, "put overwrite test: success\n");
	test_success();
    } else {
	fprintf(test_out, "put overwrite test failed\n");
	test_failure("test_gpr_replica put_test failed\n");
	test_finalize();
	exit(1);
    }

    /* test the get function */
    success = true;
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	if (10 %  i) {
	    mode = OMPI_REGISTRY_AND;
	    answer = ompi_registry.get(mode, name, name2);
	    for (ans = (ompi_registry_value_t*)ompi_list_get_first(answer);
		 ans != (ompi_registry_value_t*)ompi_list_get_end(answer);
		 ans = (ompi_registry_value_t*)ompi_list_get_next(ans)) {
		if (ans->object_size != input_size) {
		    success = false;
		}
	    }
	} else {
	    mode = OMPI_REGISTRY_XAND;
	    answer = ompi_registry.get(mode, name, name3);
	    if (0 < ompi_list_get_size(answer)) {  /* should not have gotten a result */
		success = false;
	    }
	}
    }
    if (success) {
	fprintf(test_out, "get test: success\n");
	test_success();
    } else {
	fprintf(test_out, "get test failed\n");
	test_failure("test_gpr_replica get_test failed\n");
	test_finalize();
	exit(1);
    }

    /* test the delete object function */
    success = true;
    for (i=0; i<5 && success; i+=2) {
	sprintf(name, "test-def-seg%d", i);
	mode = OMPI_REGISTRY_AND;
	if (OMPI_SUCCESS != ompi_registry.delete_object(mode, name, name2)) {
	    success = false;
	}
    }
    if (success) {
	mode = OMPI_REGISTRY_XAND;
	if (OMPI_SUCCESS == ompi_registry.delete_object(mode, name, name3)) {
	    success = false;
	}
    }
    if (success) {
	fprintf(test_out, "delete object test: success\n");
	test_success();
    } else {
	fprintf(test_out, "delete_object test failed\n");
	test_failure("test_gpr_replica delete_object_test failed\n");
	test_finalize();
	exit(1);
    }

    /* check index */
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	test_list = ompi_registry.index(name);
	if (0 == ompi_list_get_size(test_list)) { /* should have been something in dictionary */
	    fprintf(test_out, "GPR replica: index function failed\n");
	    test_failure("test_gpr_replica index_global_dictionary failed\n");
	    test_finalize();
	    exit(1);
	} else {
	    fprintf(test_out, "GPR index returned list for segment %s\n", name);
	    for (ptr = (ompi_registry_index_value_t*)ompi_list_get_first(test_list);
		 ptr != (ompi_registry_index_value_t*)ompi_list_get_end(test_list);
		 ptr = (ompi_registry_index_value_t*)ompi_list_get_next(ptr)) {
		fprintf(test_out, "\t%s\n", ptr->token);
	    }
	}
    }
    test_success();

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
