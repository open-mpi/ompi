/*
 * unit test for name server replica.

 --------------------------------------------------------------------------

 Authors:	Ralph H. Castain <rhc@lanl.gov>

 --------------------------------------------------------------------------

*/

#include <stdio.h>
#include <string.h>

#include "ompi_config.h"

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"

int main(int argc, char **argv)
{
    ompi_process_name_t *test_name;
    ompi_process_id_t test, job, tmpi, tmpj, tmpk;
    bool multi, hidden;
    int i, j;
    char *tmp;

    /* get system info */
    ompi_sys_info();
    /* set us to be seed */
    ompi_process_info.seed = true;
    ompi_proc_info();

    /* check that proc info made seed name */
    if (NULL == ompi_process_info.name) {
	fprintf(stderr, "seed name not made\n");
	exit(1);
    } else {
	fprintf(stderr, "seed name made: %x %x %x\n", ompi_process_info.name->cellid,
		ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
	fprintf(stderr, "MCA started\n");
    } else {
	fprintf(stderr, "MCA could not start - please report error to bugs@open-mpi.org\n");
	exit (1);
    }

    /* open the name server */
    if (OMPI_SUCCESS == mca_ns_base_open()) {
	fprintf(stderr, "NS opened\n");
    } else {
	fprintf(stderr, "NS could not open\n");
	exit(1);
    }

    /* startup the name server */
    if (OMPI_SUCCESS != mca_ns_base_select(&multi, &hidden)) {
	fprintf(stderr, "NS could not start\n");
	exit(1);
    } else {
	fprintf(stderr, "NS started\n");
    }

    /* create a name */
    test_name = ompi_name_server.create_process_name(0, 1, 1);
    if (NULL == test_name) { /* got error */
	fprintf(stderr, "create process name failed\n");
	exit(1);
    } else {
	fprintf(stderr, "got process name: %x %x %x\n", test_name->cellid, test_name->jobid, test_name->vpid);
    }

    /* create a cellid */
    test = ompi_name_server.create_cellid();
    if (0 == test) { /* got error */
	fprintf(stderr, "create cellid: error %d\n", test);
	exit(1);
    } else {
	fprintf(stderr, "cellid created: %d\n", test);
    }

    for (i=0; i<10; i++) { /* loop through */
	/* create jobid */
	job = ompi_name_server.create_jobid();
	if (0 == job) { /* got error */
	    fprintf(stderr, "create jobid: error\n");
	    exit(1);
	} else {
	    fprintf(stderr, "jobid created: %d\n", job);
	}

	for (j=0; j<5; j++) { /* loop through several vpid ranges */
	    /* get range of vpids */
	    test = ompi_name_server.reserve_range(job, 250);
	    if (0 == test) { /* got error */
		fprintf(stderr, "get range: error\n");
		exit(1);
	    } else {
		fprintf(stderr, "range reserved: %d\n", test);
	    }

	    /* create a name */
	    test_name = ompi_name_server.create_process_name((ompi_process_id_t)i, job, test);

	    /* get and print its string values */
	    tmp = ompi_name_server.get_proc_name_string(test_name);
	    fprintf(stderr, "(%d) strings: name - %s\n", i, tmp);
	    tmp = ompi_name_server.get_vpid_string(test_name);
	    fprintf(stderr, "\tvpid: %s\n", tmp);
	    tmp = ompi_name_server.get_jobid_string(test_name);
	    fprintf(stderr, "\tjobid: %s\n", tmp);
	    tmp = ompi_name_server.get_cellid_string(test_name);
	    fprintf(stderr, "\tcellid: %s\n", tmp);

	    tmpi = ompi_name_server.get_vpid(test_name);
	    tmpj = ompi_name_server.get_jobid(test_name);
	    tmpk = ompi_name_server.get_cellid(test_name);
	    fprintf(stderr, "(%d) ints cell %x job %x vpid %x\n\n", i, tmpk, tmpj, tmpi);
	}
    }


    return(0);
}
