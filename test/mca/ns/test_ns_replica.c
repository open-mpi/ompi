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
    mca_ns_base_cellid_t cell;
    mca_ns_base_jobid_t job;
    mca_ns_base_vpid_t vpid;
    bool multi, hidden;
    int i, j;
    char *tmp;

    ompi_process_info.seed = true;

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
	fprintf(stderr, "got process name: %0X %0X %0X\n", test_name->cellid, test_name->jobid, test_name->vpid);
    }

    /* convert a string to a name */
    tmp = strdup("1234.5678.9AEF");
    test_name = ompi_name_server.convert_string_to_process_name(tmp);
    if (NULL == test_name) {  /* got error */
	fprintf(stderr, "convert string to process name failed\n");
	exit(1);
    } else {
	fprintf(stderr, "got process name: %0X(%ld)  %0X(%ld)  %0X(%ld)\n", test_name->cellid, test_name->cellid,
		test_name->jobid, test_name->jobid, test_name->vpid, test_name->vpid);
    }

    /* create a cellid */
    cell = ompi_name_server.create_cellid();
    if (0 == cell) { /* got error */
	fprintf(stderr, "create cellid: error\n");
	exit(1);
    } else {
	fprintf(stderr, "cellid created: %d\n", cell);
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
	    vpid = ompi_name_server.reserve_range(job, 250);
	    if (0 == vpid) { /* got error */
		fprintf(stderr, "get range: error\n");
		exit(1);
	    } else {
		fprintf(stderr, "range reserved: %d\n", vpid);
	    }

	    /* create a name */
	    test_name = ompi_name_server.create_process_name((mca_ns_base_cellid_t)i, job, vpid);

	    /* get and print its string values */
	    tmp = ompi_name_server.get_proc_name_string(test_name);
	    fprintf(stderr, "(%d) strings: name - %s\n", i, tmp);
	    tmp = ompi_name_server.get_vpid_string(test_name);
	    fprintf(stderr, "\tvpid: %s\n", tmp);
	    tmp = ompi_name_server.get_jobid_string(test_name);
	    fprintf(stderr, "\tjobid: %s\n", tmp);
	    tmp = ompi_name_server.get_cellid_string(test_name);
	    fprintf(stderr, "\tcellid: %s\n", tmp);

	    /* get and print its numeric values */
	    vpid = ompi_name_server.get_vpid(test_name);
	    job = ompi_name_server.get_jobid(test_name);
	    cell = ompi_name_server.get_cellid(test_name);
	    fprintf(stderr, "(%d) ints cell %0X(%ld) job %0X(%ld) vpid %0x(%ld)\n\n", i,
		    cell, cell, job, job, vpid, vpid);
	}
    }


    return(0);
}
