/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/orte_constants.h"

#include <stdio.h>
#include <string.h>

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"

int main(int argc, char **argv)
{
    orte_process_name_t *test_name;
    orte_jobid_t job;
    orte_vpid_t vpid;
    int i, j, rc;
    char *tmp, *site, *resource;

    if (ORTE_SUCCESS != orte_init(true)) {
        fprintf(stderr, "failed to start ORTE\n");
        exit (1);
    }
    
    /* create a name */
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&test_name, 0, 1, 1))) { /* got error */
	   fprintf(stderr, "create process name failed with error %s\n",
                ORTE_ERROR_NAME(rc));
	   exit(1);
    } else {
	   fprintf(stderr, "got process name: %s\n", ORTE_NAME_PRINT(test_name));
    }
    free(test_name);
    
    /* convert a string to a name */
    tmp = strdup("124.5678.0010");
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&test_name, tmp))) {  /* got error */
	   fprintf(stderr, "convert string to process name failed with error %s\n",
                ORTE_ERROR_NAME(rc));
	   exit(1);
    } else {
        fprintf(stderr, "got process name: %s\n", ORTE_NAME_PRINT(test_name));
    }
    free(tmp);
    free(test_name);
    
    for (i=0; i<10; i++) { /* loop through */
    	/* create jobid */
    	if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&job, NULL))) { /* got error */
    	    fprintf(stderr, "create jobid: error with error %s\n", ORTE_ERROR_NAME(rc));
    	    exit(1);
    	} else {
    	    fprintf(stderr, "jobid created: %lu\n", (unsigned long) job);
    	}
    
    	for (j=0; j<5; j++) { /* loop through several vpid ranges */
    	    /* get range of vpids */
    	    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(job, 250, &vpid))) { /* got error */
    		   fprintf(stderr, "reserve range: error with error %s\n",
                ORTE_ERROR_NAME(rc));
    		   exit(1);
    	    } else {
    		   fprintf(stderr, "range reserved: %lu\n", 
                           (unsigned long) vpid);
    	    }
    
    	    /* create a name */
    	    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&test_name,
    							     job, vpid))) {
               fprintf(stderr, "test_ns_replica: failed to create proc name after vpid range with error %s\n",
                ORTE_ERROR_NAME(rc));
               exit(1);
            }
    
    	    /* get and print its string values */
    	    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&tmp, test_name))) {
               fprintf(stderr, "test_ns_replica: failed to get proc_name_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               exit(1);
            } else {
    	       fprintf(stderr, "(%d) strings: name - %s\n", i, tmp);
            }
            free(tmp);
            if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&tmp, test_name))) {
               fprintf(stderr, "test_ns_replica: failed to get vpid_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               exit(1);
            } else {
               fprintf(stderr, "(%d) strings: vpid - %s\n", i, tmp);
            }
            free(tmp);
            if (ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&tmp, test_name))) {
               fprintf(stderr, "test_ns_replica: failed to get jobid_string with error %s\n",
                ORTE_ERROR_NAME(rc));
               exit(1);
            } else {
               fprintf(stderr, "(%d) strings: jobid - %s\n", i, tmp);
            }
            free(tmp);
     
    	}
    }

    /* finalize and see if memory cleared */
    orte_ns_base_close();

    orte_proc_info_finalize();
    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();

    fclose( stderr );

    return(0);
}
