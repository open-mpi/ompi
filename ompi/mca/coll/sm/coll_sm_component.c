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

#include "ompi_config.h"

#include "ompi/include/constants.h"
#include "mca/coll/coll.h"
#include "coll_sm.h"


/*
 * Public string showing the coll ompi_sm component version number
 */
const char *mca_coll_sm_component_version_string =
    "Open MPI sm collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int sm_open(void);
static int sm_close(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_sm_component_t mca_coll_sm_component = {

    /* First, fill in the super (mca_coll_base_component_1_0_0_t) */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        
        {
            /* Indicate that we are a coll v1.0.0 component (which
               also implies a specific MCA version) */

            MCA_COLL_BASE_VERSION_1_0_0,

            /* Component name and version */

            "sm",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            sm_open,
            sm_close,
        },
        
        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */
            
            true
        },

        /* Initialization / querying functions */
        
        mca_coll_sm_init_query,
        mca_coll_sm_comm_query,
        mca_coll_sm_comm_unquery,
    },

    /* sm-component specifc information */

    /* priority */
    75,

    /* mpool name and instance */
    "sm",
    NULL
};


/*
 * Open the component
 */
static int sm_open(void)
{
    int p, ival;
    char *sval;

    /* If we want to be selected (i.e., all procs on one node), then
       we should have a high priority */

    p = mca_base_param_register_int("coll", "sm", "priority", NULL, 75);
    mca_base_param_lookup_int(p, &ival);
    mca_coll_sm_component.sm_priority = ival;

    p = mca_base_param_register_string("coll", "sm", "mpool", NULL, "sm");
    mca_base_param_lookup_string(p, &sval);
    mca_coll_sm_component.sm_mpool_name = sval;

    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int sm_close(void)
{
    if (NULL != mca_coll_sm_component.sm_mpool_name) {
        free(mca_coll_sm_component.sm_mpool_name);
        mca_coll_sm_component.sm_mpool_name = NULL;
    }

    return OMPI_SUCCESS;
}
