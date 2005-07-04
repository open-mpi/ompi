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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#import "ompi_config.h"

#import <stdlib.h>
#import <unistd.h>

#import "include/orte_constants.h"
#import "opal/util/argv.h"
#import "util/path.h"
#import "opal/util/basename.h"
#import "mca/pls/pls.h"
#import "mca/pls/base/base.h"
#import "mca/pls/xgrid/pls-xgrid-version.h"
#import "mca/base/mca_base_param.h"
#import "mca/rml/rml.h"

#import "pls_xgrid.h"
#import "pls_xgrid_client.h"


/*
 * Public string showing the pls ompi_xgrid component version number
 */
const char *mca_pls_xgrid_component_version_string =
  "Open MPI xgrid pls MCA component version " MCA_pls_xgrid_VERSION;

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
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "xgrid",
        MCA_pls_xgrid_MAJOR_VERSION,
        MCA_pls_xgrid_MINOR_VERSION,
        MCA_pls_xgrid_RELEASE_VERSION,

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
    mca_base_param_register_string("pls", "xgrid", "orted", NULL, "orted");
    mca_base_param_register_int("pls", "xgrid", "priority", NULL, 20);
    mca_base_param_register_int("pls", "xgrid", "delete_job", NULL, 1);

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

    if (NULL == getenv("XGRID_CONTROLLER_HOSTNAME") ||
        NULL == getenv("XGRID_CONTROLLER_PASSWORD")) {
	opal_output(orte_pls_base.pls_output,
		    "orte:pls:xgrid: not available: controller info not set");
        return NULL;
    }

    opal_output(orte_pls_base.pls_output,
		"orte:pls:xgrid: initializing PlsXGridClient");
    mca_pls_xgrid_component.pool = [[NSAutoreleasePool alloc] init];
    mca_pls_xgrid_component.client = [[PlsXGridClient alloc] init];

    /* setup daemon name */
    param = mca_base_param_find("pls", "xgrid", "orted");
    mca_base_param_lookup_string(param, &string);
    [mca_pls_xgrid_component.client setOrtedAsCString: string];
    if (NULL != string) free(string);

    /* setup contact information */
    [mca_pls_xgrid_component.client setControllerPasswordAsCString: 
				getenv("XGRID_CONTROLLER_PASSWORD")];
    [mca_pls_xgrid_component.client setControllerHostnameAsCString: 
				getenv("XGRID_CONTROLLER_HOSTNAME")];

    /* info we need */
    param = mca_base_param_find("pls", "xgrid", "priority");
    mca_base_param_lookup_int(param, priority);

    param = mca_base_param_find("pls", "xgrid", "delete_job");
    mca_base_param_lookup_int(param, &val);
    [mca_pls_xgrid_component.client setCleanUp: val];

    opal_progress_register(orte_pls_xgrid_progress);

    opal_output(orte_pls_base.pls_output, "orte:pls:xgrid: initialized");

    ret = [mca_pls_xgrid_component.client connect];
    if (ret != ORTE_SUCCESS) {
	opal_output(orte_pls_base.pls_output,
		    "orte:pls:xgrid: connection failed");
	orte_pls_xgrid_finalize();
    }

    return &orte_pls_xgrid_module;
}


int
orte_pls_xgrid_progress(void)
{
    /* tick the event loop */
    [[NSRunLoop currentRunLoop] runUntilDate: 
				    [NSDate dateWithTimeIntervalSinceNow:1]];
}
