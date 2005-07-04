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
#include "mca/schema/schema.h"
#include "util/proc_info.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_ns_replica_out ./test_ns_replica_out_std";

int main(int argc, char **argv)
{
    orte_process_name_t *test_name;
    orte_cellid_t cell;
    orte_jobid_t job;
    orte_vpid_t vpid;
    int i, j, rc;
    char *tmp, *site, *resource;
    test_component_handle_t ns_handle;
    mca_ns_base_component_t *ns_component = NULL;
    mca_ns_base_module_t *ns_module = NULL;
    int priority;

    test_init("test_ns_replica");

    test_out = fopen( "test_ns_replica_out", "w+" );
    if( test_out == NULL ) {
      test_failure("test_ns_replica couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* Open up the output streams */
    if (!opal_output_init()) {
        return OMPI_ERROR;
    }

    /* 
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    opal_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    opal_malloc_init();

    /* set seed to true to force replica selection */
    orte_process_info.seed = true;

    /* ensure the replica is isolated */
    setenv("OMPI_MCA_ns_replica_isolate", "1", 1);
    
    /* init the proc info structure */
    orte_proc_info();
    
    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
	fprintf(test_out, "MCA started\n");
    } else {
	fprintf(test_out, "MCA could not start - please report error to bugs@open-mpi.org\n");
	exit (1);
    }

    /* Open the ns replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("ns", "replica", &ns_handle, 
                            (mca_base_component_t**) &ns_component) ||
        NULL == ns_component) {
        test_fail_stop("Could not open ns replica\n", 1);
    }
    ns_module = ns_component->ns_init(&priority);
    if (NULL == ns_module) {
        test_fail_stop("NS replica component did not return a module\n", 1);
    }

    /* create a name */
    if (ORTE_SUCCESS != (rc = ns_module->create_process_name(&test_name, 0, 1, 1))) { /* got error */
	   fprintf(test_out, "create process name failed with error %s\n",
                ORTE_ERROR_NAME(rc));
	   test_failure("test_ns_replica orte_ns create_process_name failed");
       test_finalize();
	   exit(1);
    } else {
	   fprintf(test_out, "got process name: %lu %lu %lu\n", ORTE_NAME_ARGS(test_name));
	   test_success();
    }
    free(test_name);
    
    /* convert a string to a name */
    tmp = strdup("1234.5678.9AEF");
    if (ORTE_SUCCESS != (rc = ns_module->convert_string_to_process_name(&test_name, tmp))) {  /* got error */
	   fprintf(test_out, "convert string to process name failed with error %s\n",
                ORTE_ERROR_NAME(rc));
	   test_failure("test_ns_replica orte_ns convert_string_to_process_name failed");
       test_finalize();
	   exit(1);
    } else {
        fprintf(test_out, "got process name: %lu  %lu  %lu\n", 
	       ORTE_NAME_ARGS(test_name));
        test_success();
    }
    free(tmp);
    free(test_name);
    
    /* create a cellid */
    if (ORTE_SUCCESS != (rc = ns_module->create_cellid(&cell, "dummy-site", "dummy-resource"))) { /* got error */
       test_failure("test_ns_replica orte_ns test create_cellid failed");
	   fprintf(test_out, "create cellid: error with error %s\n", ORTE_ERROR_NAME(rc));
	   test_finalize();
	   exit(1);
    } else {
        fprintf(test_out, "cellid created: %lu\n", (unsigned long) cell);
        test_success();
    }

    /* get cellid info */
    if (ORTE_SUCCESS != (rc = ns_module->get_cell_info(cell, &site, &resource))) { /* got error */
       test_failure("test_ns_replica orte_ns test get_cell_info failed");
       fprintf(test_out, "get_cell_info: error with error %s\n", ORTE_ERROR_NAME(rc));
    test_finalize();
       exit(1);
    } else {
        fprintf(test_out, "get_cell_info: %lu %s %s\n", (unsigned long) cell, site, resource);
        test_success();
    }

    for (i=0; i<10; i++) { /* loop through */
    	/* create jobid */
    	if (ORTE_SUCCESS != (rc = ns_module->create_jobid(&job))) { /* got error */
    	    fprintf(test_out, "create jobid: error with error %s\n", ORTE_ERROR_NAME(rc));
    	    test_failure("test_ns_replica orte_ns create_jobid failed");
    	    test_finalize();
    	    exit(1);
    	} else {
    	    fprintf(test_out, "jobid created: %lu\n", (unsigned long) job);
    	    test_success();
    	}
    
    	for (j=0; j<5; j++) { /* loop through several vpid ranges */
    	    /* get range of vpids */
    	    if (ORTE_SUCCESS != (rc = ns_module->reserve_range(job, 250, &vpid))) { /* got error */
    		   fprintf(test_out, "get range: error with error %s\n",
                ORTE_ERROR_NAME(rc));
    		   test_failure("test_ns_replica orte_ns reserve_range failed");
    	   	   test_finalize();
    		   exit(1);
    	    } else {
    		   fprintf(test_out, "range reserved: %lu\n", 
                           (unsigned long) vpid);
    		   test_success();
    	    }
    
    	    /* create a name */
    	    if (ORTE_SUCCESS != (rc = ns_module->create_process_name(&test_name, (orte_cellid_t)i, 
    							     job, vpid))) {
               fprintf(test_out, "test_ns_replica: failed to create proc name after vpid range with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to create proc name after vpid range");
               test_finalize();
               exit(1);
            }
    
    	    /* get and print its string values */
    	    if (ORTE_SUCCESS != (rc = ns_module->get_proc_name_string(&tmp, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get proc_name_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get proc_name_string");
               test_finalize();
               exit(1);
            } else {
    	       fprintf(test_out, "(%d) strings: name - %s\n", i, tmp);
            }
            free(tmp);
            if (ORTE_SUCCESS != (rc = ns_module->get_vpid_string(&tmp, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get vpid_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get vpid_string");
               test_finalize();
               exit(1);
            } else {
               fprintf(test_out, "(%d) strings: vpid - %s\n", i, tmp);
            }
            free(tmp);
            if (ORTE_SUCCESS != (rc = ns_module->get_jobid_string(&tmp, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get jobid_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get jobid_string");
               test_finalize();
               exit(1);
            } else {
               fprintf(test_out, "(%d) strings: jobid - %s\n", i, tmp);
            }
            free(tmp);
            if (ORTE_SUCCESS != (rc = ns_module->get_cellid_string(&tmp, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get cellid_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get cellid_string");
               test_finalize();
               exit(1);
            } else {
               fprintf(test_out, "(%d) strings: cellid - %s\n", i, tmp);
            }
            free(tmp);
    
    	    /* get and print its numeric values */
            if (ORTE_SUCCESS != (rc = ns_module->get_vpid(&vpid, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get vpid with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get vpid");
               test_finalize();
               exit(1);
            }
            if (ORTE_SUCCESS != (rc = ns_module->get_jobid(&job, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get jobid with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get jobid");
               test_finalize();
               exit(1);
            }
            if (ORTE_SUCCESS != (rc = ns_module->get_cellid(&cell, test_name))) {
               fprintf(test_out, "test_ns_replica: failed to get cellid with error %s\n",
                ORTE_ERROR_NAME(rc));
               test_failure("test_ns_replica: failed to get cellid");
               test_finalize();
               exit(1);
            }
    	    fprintf(test_out, "(%d) ints cell %lu job %lu vpid %lu\n\n", 
    		    i, (unsigned long) cell, (unsigned long) job, 
                    (unsigned long) vpid);
            free(test_name);
    	}
    }

    /* finalize and see if memory cleared */
    if (NULL != ns_component->ns_finalize) {
        ns_component->ns_finalize();
    }
    test_component_close(&ns_handle);

    orte_proc_info_finalize();
    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();

    fclose( test_out );
    test_finalize();

    return(0);
}
