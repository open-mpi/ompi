/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/fddp/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/fddp/base/static-components.h"

/*
 * Global variables
 */
int orte_fddp_base_output = -1;
orte_fddp_base_module_t orte_fddp;
opal_list_t mca_fddp_base_components_available;
orte_fddp_base_component_t mca_fddp_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_fddp_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_fddp_base_output = opal_output_open(NULL);
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("fddp", orte_fddp_base_output,
                                 mca_fddp_base_static_components,
                                 &mca_fddp_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
