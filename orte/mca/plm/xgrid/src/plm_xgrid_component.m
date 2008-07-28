/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
 */

#import "orte_config.h"

#import <stdlib.h>
#import <unistd.h>

#import "orte/constants.h"
#import "opal/util/argv.h"
#import "opal/util/path.h"
#import "opal/util/basename.h"

#import "orte/util/proc_info.h"
#import "orte/mca/plm/plm.h"
#import "orte/mca/plm/base/base.h"
#import "orte/mca/plm/base/plm_private.h"
#import "opal/mca/base/mca_base_param.h"

#import "plm_xgrid.h"
#import "plm_xgrid_client.h"

int orte_plm_xgrid_component_open(void);
int orte_plm_xgrid_component_close(void);
int orte_plm_xgrid_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_plm_xgrid_component_t mca_plm_xgrid_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        ORTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        "xgrid",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_plm_xgrid_component_open,
        orte_plm_xgrid_component_close,
        orte_plm_xgrid_component_query
    },
    {
        /* This component is not checkpointable */
        MCA_BASE_METADATA_PARAM_NONE
    }
    }
};


int
orte_plm_xgrid_component_open(void)
{
    mca_base_param_reg_string(&mca_plm_xgrid_component.super.base_version,
			      "orted",
			      "The command name that the component will invoke for the ORTE daemon",
			      false, false, "orted", NULL);

    mca_base_param_reg_int(&mca_plm_xgrid_component.super.base_version,
			   "priority",
			   "Priority of the xgrid plm component",
			   false, false, 20, NULL);

    mca_base_param_reg_int(&mca_plm_xgrid_component.super.base_version,
			   "delete_job",
			   "Delete job from XGrid controller's database on job completion",
			   false, false, 1, NULL);

    mca_base_param_reg_int(&mca_plm_xgrid_component.super.base_version,
			   "num_slots",
			   "Number of slots to reserve for job (including future spawned processes).  "
			   "0 will result in number of processes requested in initial launch.",
			   false, false, 0, NULL);

    return ORTE_SUCCESS;
}


int
orte_plm_xgrid_component_close(void)
{
    return ORTE_SUCCESS;
}


int orte_plm_xgrid_component_query(mca_base_module_t **module, int *priority)
{
    char *string;
    int ret, val, param;

    if (NULL == getenv("XGRID_CONTROLLER_HOSTNAME")) {
        opal_output_verbose(10, orte_plm_globals.output,
                            "orte:plm:xgrid: not available: controller info not set");
        *module = NULL;
        return ORTE_ERROR;
    }

    opal_output_verbose(1, orte_plm_globals.output,
			"orte:plm:xgrid: initializing PlmXGridClient");
    mca_plm_xgrid_component.pool = [[NSAutoreleasePool alloc] init];
    mca_plm_xgrid_component.client = [[PlmXGridClient alloc] init];

    /* setup daemon name */
    param = mca_base_param_find("plm", "xgrid", "orted");
    mca_base_param_lookup_string(param, &string);
    [mca_plm_xgrid_component.client setOrtedAsCString: string];
    if (NULL != string) free(string);

    /* setup contact information */
    if (NULL != getenv("XGRID_CONTROLLER_PASSWORD")) {
	[mca_plm_xgrid_component.client setControllerPasswordAsCString: 
				    getenv("XGRID_CONTROLLER_PASSWORD")];
    }
    [mca_plm_xgrid_component.client setControllerHostnameAsCString: 
				getenv("XGRID_CONTROLLER_HOSTNAME")];

    /* info we need */
    param = mca_base_param_find("plm", "xgrid", "priority");
    mca_base_param_lookup_int(param, priority);

    param = mca_base_param_find("plm", "xgrid", "delete_job");
    mca_base_param_lookup_int(param, &val);
    [mca_plm_xgrid_component.client setCleanUp: val];

    opal_progress_register(orte_plm_xgrid_progress);

    ret = [mca_plm_xgrid_component.client connect];
    if (ret != ORTE_SUCCESS) {
        opal_output_verbose(10, orte_plm_globals.output,
                            "orte:plm:xgrid: not available: connection failed");
        orte_plm_xgrid_finalize();
        *module = NULL;
        return ORTE_ERROR;
    }

    opal_output_verbose(10, orte_plm_globals.output,
			"orte:plm:xgrid: initialized");
    *module = (mca_base_module_t *) &orte_plm_xgrid_module;
    return ORTE_SUCCESS;
}


int
orte_plm_xgrid_progress(void)
{
    /* tick the event loop */
    [[NSRunLoop currentRunLoop] runUntilDate: 
				    [NSDate dateWithTimeIntervalSinceNow:1]];
}
