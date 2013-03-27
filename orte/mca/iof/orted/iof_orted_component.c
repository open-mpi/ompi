/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"

#include "iof_orted.h"

/*
 * Local functions
 */
static int orte_iof_orted_open(void);
static int orte_iof_orted_close(void);
static int orte_iof_orted_query(mca_base_module_t **module, int *priority);


/*
 * Public string showing the iof orted component version number
 */
const char *mca_iof_orted_component_version_string =
"Open MPI orted iof MCA component version " ORTE_VERSION;


orte_iof_orted_component_t mca_iof_orted_component = {
    {
        {
            ORTE_IOF_BASE_VERSION_2_0_0,
            
            "orted", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            
            /* Component open, close, and query functions */
            orte_iof_orted_open,
            orte_iof_orted_close,
            orte_iof_orted_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

/**
  * component open/close/init function
  */
static int orte_iof_orted_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}

static int orte_iof_orted_close(void)
{
    return ORTE_SUCCESS;
}


static int orte_iof_orted_query(mca_base_module_t **module, int *priority)
{
    /* if we are not a daemon, then don't use this module */
    if (!ORTE_PROC_IS_DAEMON) {
        *module = NULL;
        *priority = -1;
        return ORTE_ERROR;
    }

    *priority = 100;
    *module = (mca_base_module_t *) &orte_iof_orted_module;
    
    return ORTE_SUCCESS;
}

