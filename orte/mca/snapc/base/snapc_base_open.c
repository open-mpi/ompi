/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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

#include "orte/orte_constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/snapc/base/static-components.h"

/*
 * Globals
 */
int  orte_snapc_base_output  = -1;
bool orte_snapc_base_is_tool = false;
orte_snapc_base_module_t orte_snapc = {
    NULL, /* snapc_init      */
    NULL, /* snapc_finalize  */
    NULL, /* setup_job       */
    NULL  /* release_job     */
};

opal_list_t orte_snapc_base_components_available;
orte_snapc_base_component_t orte_snapc_base_selected_component;

char * orte_snapc_base_global_snapshot_dir = NULL;
bool orte_snapc_base_store_in_place = true;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_snapc_base_open(void)
{
    int value;
    char * str_value = NULL;
    char * home = NULL;

    /* Debugging/Verbose output */
    mca_base_param_reg_int_name("snapc",
                                "base_verbose",
                                "Verbosity level of the SNAPC framework",
                                false, false,
                                0, &value);
    if(0 != value) {
        orte_snapc_base_output = opal_output_open(NULL);
    } else {
        orte_snapc_base_output = -1;
    }
    opal_output_set_verbosity(orte_snapc_base_output, value);

    /* We may need this later */
#if !defined(__WINDOWS__)
    home = getenv("HOME");
#else
    home = getenv("USERPROFILE");
#endif  /* !defined(__WINDOWS__) */

    /* Global Snapshot directory */
    mca_base_param_reg_string_name("snapc",
                                   "base_global_snapshot_dir",
                                   "The base directory to use when storing global snapshots",
                                   false, false,
                                   home,
                                   &orte_snapc_base_global_snapshot_dir);
    /*
     * Store the checkpoint files in their final location.
     * This assumes that the storage place is on a shared file 
     * system that all nodes can access uniformly.
     * Default = enabled
     */
    mca_base_param_reg_int_name("snapc",
                                "base_store_in_place",
                                "If global_snapshot_dir is on a shared file system all nodes can access, "
                                "then the checkpoint files can be stored in place instead of incurring a "
                                "remote copy. [Default = enabled]",
                                false, false,
                                1,
                                &value);
    if( 0 != value ) { /* Enabled */
        orte_snapc_base_store_in_place = true;
    }
    else { /* Disabled */
        orte_snapc_base_store_in_place = false;
    }

    /* Init the sequence (interval) number */
    orte_snapc_base_snapshot_seq_number = 0;


    /* 
     * Which SnapC component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     * Note: Set the default to NULL here so ompi_info will work correctly,
     *       The 'real' default is set in base_select.c
     */
    mca_base_param_reg_string_name("snapc", NULL,
                                   "Which SNAPC component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("snapc", 
                                 orte_snapc_base_output, 
                                 mca_snapc_base_static_components,
                                 &orte_snapc_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
}
