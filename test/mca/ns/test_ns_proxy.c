/*
 * unit test for name server proxy.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>
            Tim S. Woodall <twoodall@lanl.gov>

 --------------------------------------------------------------------------

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "ompi_config.h"
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

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_proxy_out ./test_ns_replica_out_std";
static void run_test(void);
static int exec_client(int argc, char** argv);


int main(int argc, char **argv)
{
    bool multi, hidden;
    char *seed;

    test_init("test_ns_proxy");
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
#if 0
        /* wait on child to exit */
        int pid = exec_client(argc, argv);
        while(true) {
            int status;
            if(waitpid(pid, &status, WNOHANG) != 0)
                break;
            ompi_event_loop(OMPI_EVLOOP_NONBLOCK);
        }
#else
        fprintf(stderr, "OMPI_MCA_oob_base_seed=%s", mca_oob_get_contact_info());
        ompi_event_loop(0);
#endif
    } else {
        run_test();
    }
    return(0);
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


void run_test(void)
{
    ompi_process_name_t *test_name;
    mca_ns_base_cellid_t cell;
    mca_ns_base_jobid_t job;
    mca_ns_base_vpid_t vpid;
    int i, j;
    char *tmp;
    int result; /* result from system call */

    /* create a name */
    test_name = ompi_name_server.create_process_name(0, 1, 1);
    if (NULL == test_name) { /* got error */
	fprintf(test_out, "create process name failed\n");
	test_failure("test_ns_replica ompi_name_server create_process_name failed");
        test_finalize();
	exit(1);
    } else {
	fprintf(test_out, "got process name: %0X %0X %0X\n", 
		test_name->cellid, test_name->jobid, test_name->vpid);
	test_success();
    }

    /* convert a string to a name */
    tmp = strdup("1234.5678.9AEF");
    test_name = ompi_name_server.convert_string_to_process_name(tmp);
    if (NULL == test_name) {  /* got error */
	fprintf(test_out, "convert string to process name failed\n");
	test_failure("test_ns_replica ompi_name_server convert_string_to_process_name failed");
        test_finalize();
	exit(1);
    } else {
      fprintf(test_out, "got process name: %0X(%ld)  %0X(%ld)  %0X(%ld)\n", 
	      (int)test_name->cellid, (long int)test_name->cellid, (int)test_name->jobid, 
	      (long int)test_name->jobid, (unsigned int)test_name->vpid, 
	      (long int)test_name->vpid);
        test_success();
    }

    /* create a cellid */
    cell = ompi_name_server.create_cellid();
    if (0 == cell) { /* got error */
        test_failure("test_ns_replica ompi_name_server test create_cellid");
	fprintf(test_out, "create cellid: error\n");
	test_finalize();
	exit(1);
    } else {
      fprintf(test_out, "cellid created: %d\n", cell);
      test_success();
    }

    for (i=0; i<10; i++) { /* loop through */
	/* create jobid */
	job = ompi_name_server.create_jobid();
	if (0 == job) { /* got error */
	    fprintf(test_out, "create jobid: error\n");
	    test_failure("test_ns_replica ompi_name_server create_jobid failed");
	    test_finalize();
	    exit(1);
	} else {
	    fprintf(test_out, "jobid created: %d\n", job);
	    test_success();
	}

	for (j=0; j<5; j++) { /* loop through several vpid ranges */
	    /* get range of vpids */
	    vpid = ompi_name_server.reserve_range(job, 250);
	    if (0 == vpid) { /* got error */
		fprintf(test_out, "get range: error\n");
		test_failure("test_ns_replica ompi_name_server reserve_range failed");
		test_finalize();
		exit(1);
	    } else {
		fprintf(test_out, "range reserved: %d\n", vpid);
		test_success();
	    }

	    /* create a name */
	    test_name = ompi_name_server.create_process_name((mca_ns_base_cellid_t)i, 
							     job, vpid);

	    /* get and print its string values */
	    tmp = ompi_name_server.get_proc_name_string(test_name);
	    fprintf(test_out, "(%d) strings: name - %s\n", i, tmp);
	    tmp = ompi_name_server.get_vpid_string(test_name);
	    fprintf(test_out, "\tvpid: %s\n", tmp);
	    tmp = ompi_name_server.get_jobid_string(test_name);
	    fprintf(test_out, "\tjobid: %s\n", tmp);
	    tmp = ompi_name_server.get_cellid_string(test_name);
	    fprintf(test_out, "\tcellid: %s\n", tmp);

	    /* get and print its numeric values */
	    vpid = ompi_name_server.get_vpid(test_name);
	    job = ompi_name_server.get_jobid(test_name);
	    cell = ompi_name_server.get_cellid(test_name);
	    fprintf(test_out, "(%d) ints cell %0X(%ld) job %0X(%ld) vpid %0x(%ld)\n\n", 
		    i, cell, (long int)cell, job, (long int)job, vpid, (long int)vpid);
	}

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
}

