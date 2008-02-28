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

#include "orte/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

#include "orte/mca/filem/base/static-components.h"

/*
 * Globals
 */
ORTE_DECLSPEC int  orte_filem_base_output  = -1;
ORTE_DECLSPEC orte_filem_base_module_t orte_filem = {
    NULL, /* filem_init     */
    NULL, /* filem_finalize */

    NULL, /* put */
    NULL, /* get */
    NULL  /* rm  */
};
opal_list_t orte_filem_base_components_available;
orte_filem_base_component_t orte_filem_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orte_filem_base_open(void)
{
    int value;
    char *str_value = NULL;

    /* Debugging/Verbose output */
    mca_base_param_reg_int_name("filem",
                                "base_verbose",
                                "Verbosity level of the FILEM framework",
                                false, false,
                                0, &value);
    if(0 != value) {
        orte_filem_base_output = opal_output_open(NULL);
    } else {
        orte_filem_base_output = 0;
    }

    /* 
     * Which FileM component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     */
    mca_base_param_reg_string_name("filem", NULL,
                                   "Which Filem component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);
    if( NULL != str_value ) {
        free(str_value);
    }

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("filem", 
                                 orte_filem_base_output, 
                                 mca_filem_base_static_components,
                                 &orte_filem_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
}
