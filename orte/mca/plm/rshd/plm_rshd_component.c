/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
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

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rshd/plm_rshd.h"


/*
 * Public string showing the plm ompi_rshd component version number
 */
const char *mca_plm_rshd_component_version_string =
  "Open MPI rshd plm MCA component version " ORTE_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_rshd_component_t mca_plm_rshd_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        ORTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        "rshd",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_plm_rshd_component_open,
        orte_plm_rshd_component_close,
        orte_plm_rshd_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
    }
};



int orte_plm_rshd_component_open(void)
{
    int tmp;
    mca_base_component_t *c = &mca_plm_rshd_component.super.base_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_plm_rshd_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_plm_rshd_component.cond, opal_condition_t);
    mca_plm_rshd_component.num_children = 0;
    OBJ_CONSTRUCT(&mca_plm_rshd_component.children, opal_list_t);

    /* lookup parameters */
    mca_base_param_reg_int(c, "num_concurrent",
                           "How many plm_rsh_agent instances to invoke concurrently (must be > 0)",
                           false, false, 128, &tmp);
    if (tmp <= 0) {
        orte_show_help("help-plm-rsh.txt", "concurrency-less-than-zero",
                       true, tmp);
        tmp = 1;
    }
    mca_plm_rshd_component.num_concurrent = tmp;
    
    mca_base_param_reg_int(c, "force_rsh",
                           "Force the launcher to always use rsh",
                           false, false, false, &tmp);
    mca_plm_rshd_component.force_rsh = OPAL_INT_TO_BOOL(tmp);
    
    return ORTE_SUCCESS;
}


int orte_plm_rshd_component_query(mca_base_module_t **module, int *priority)
{
    if (ORTE_SUCCESS != orte_plm_base_rsh_launch_agent_setup()) {
        /* this isn't an error - we just cannot be selected */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshd: unable to be used: cannot find path "
                             "for launching agent \"%s\"\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_plm_globals.rsh_agent_argv[0]));
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* we are good - make ourselves available, but only if selected */
    *priority = 0;
    *module = (mca_base_module_t *) &orte_plm_rshd_module;
    return ORTE_SUCCESS;
}


int orte_plm_rshd_component_close(void)
{
    /* cleanup state */
    OBJ_DESTRUCT(&mca_plm_rshd_component.lock);
    OBJ_DESTRUCT(&mca_plm_rshd_component.cond);
    OBJ_DESTRUCT(&mca_plm_rshd_component.children);

    return ORTE_SUCCESS;
}
