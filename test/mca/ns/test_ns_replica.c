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
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"

int main(int argc, char **argv)
{
    ompi_process_name_t *test_name;

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
    if (NULL == mca_ns_replica_init()) {
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

    return(0);
}
