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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#import "orte_config.h"

#import <stdlib.h>
#import <unistd.h>

#import "orte/orte_constants.h"
#import "opal/util/argv.h"
#import "opal/util/path.h"
#import "opal/util/basename.h"

#import "orte/util/proc_info.h"
#import "orte/mca/pls/pls.h"
#import "orte/mca/pls/base/base.h"
#import "opal/mca/base/mca_base_param.h"

#import "pls_xgrid.h"
#import "pls_xgrid_client.h"


/*
 * Public string showing the pls ompi_xgrid component version number
 */
const char *mca_pls_xgrid_component_version_string =
  "Open MPI xgrid pls MCA component version " ORTE_VERSION;

int orte_pls_xgrid_component_open(void);
int orte_pls_xgrid_component_close(void);
orte_pls_base_module_t * orte_pls_xgrid_component_init(int *priority);



/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_xgrid_component_t mca_pls_xgrid_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_3_0,

        /* Component name and version */

        "xgrid",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_xgrid_component_open,
        orte_pls_xgrid_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_pls_xgrid_component_init
    }
};


int
orte_pls_xgrid_component_open(void)
{
    mca_base_param_reg_string(&mca_pls_xgrid_component.super.pls_version,
			      "orted",
			      "The command name that the component will invoke for the ORTE daemon",
			      false, false, "orted", NULL);

    mca_base_param_reg_int(&mca_pls_xgrid_component.super.pls_version,
			   "priority",
			   "Priority of the xgrid pls component",
			   false, false, 20, NULL);

    mca_base_param_reg_int(&mca_pls_xgrid_component.super.pls_version,
			   "delete_job",
			   "Delete job from XGrid controller's database on job completion",
			   false, false, 1, NULL);

    return ORTE_SUCCESS;
}


int
orte_pls_xgrid_component_close(void)
{
    return ORTE_SUCCESS;
}


orte_pls_base_module_t *
orte_pls_xgrid_component_init(int *priority)
{
    char *string;
    int ret, val, param;
    
    /* if we are NOT an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }

    if (NULL == getenv("XGRID_CONTROLLER_HOSTNAME")) {
	opal_output_verbose(10, orte_pls_base.pls_output,
		    "orte:pls:xgrid: not available: controller info not set");
        return NULL;
    }

    opal_output_verbose(1, orte_pls_base.pls_output,
			"orte:pls:xgrid: initializing PlsXGridClient");
    mca_pls_xgrid_component.pool = [[NSAutoreleasePool alloc] init];
    mca_pls_xgrid_component.client = [[PlsXGridClient alloc] init];

    /* setup daemon name */
    param = mca_base_param_find("pls", "xgrid", "orted");
    mca_base_param_lookup_string(param, &string);
    [mca_pls_xgrid_component.client setOrtedAsCString: string];
    if (NULL != string) free(string);

    /* setup contact information */
    if (NULL != getenv("XGRID_CONTROLLER_PASSWORD")) {
	[mca_pls_xgrid_component.client setControllerPasswordAsCString: 
				    getenv("XGRID_CONTROLLER_PASSWORD")];
    }
    [mca_pls_xgrid_component.client setControllerHostnameAsCString: 
				getenv("XGRID_CONTROLLER_HOSTNAME")];

    /* info we need */
    param = mca_base_param_find("pls", "xgrid", "priority");
    mca_base_param_lookup_int(param, priority);

    param = mca_base_param_find("pls", "xgrid", "delete_job");
    mca_base_param_lookup_int(param, &val);
    [mca_pls_xgrid_component.client setCleanUp: val];

    opal_progress_register(orte_pls_xgrid_progress);

    ret = [mca_pls_xgrid_component.client connect];
    if (ret != ORTE_SUCCESS) {
	opal_output_verbose(10, orte_pls_base.pls_output,
			    "orte:pls:xgrid: not available: connection failed");
	orte_pls_xgrid_finalize();
	return NULL;
    }

    opal_output_verbose(10, orte_pls_base.pls_output,
			"orte:pls:xgrid: initialized");

    return &orte_pls_xgrid_module;
}


int
orte_pls_xgrid_progress(void)
{
    /* tick the event loop */
    [[NSRunLoop currentRunLoop] runUntilDate: 
				    [NSDate dateWithTimeIntervalSinceNow:1]];
}
