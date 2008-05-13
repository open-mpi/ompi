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
 */

#include "opal_config.h"

#include "opal/constants.h"

#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/filter/filter.h"
#include "filter_xml.h"

/*
 * Public string showing the filter ompi_filter component version number
 */
const char *opal_filter_xml_component_version_string =
    "OPAL filter xml MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int xml_open(void);
static int xml_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_filter_base_component_t mca_filter_xml_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a filter v1.0.0 component (which also
           implies a specific MCA version) */
        
        OPAL_FILTER_BASE_VERSION_1_0_0,

        /* Component name and version */

        "xml",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,
        
        /* Component open and close functions */

        xml_open,
        NULL,
        xml_component_query
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int xml_open(void)
{
    return OPAL_SUCCESS;
}

int xml_component_query(mca_base_module_t **module, int *priority)
{
    int index;
    char *param;
    
    /* we can't currently handle this framework correctly, so we have
     * to "hack" this to ensure this component is only selected
     * when the user specifically requests it
     */
    
    index = mca_base_param_find("filter", NULL, NULL);
    if (0 > index) {
        /* wasn't specified - we can't be selected */
        *module = NULL;
        *priority = -1;
        return OPAL_SUCCESS;
    }
    
    /* see if it was us */
    mca_base_param_lookup_string(index, &param);
    if (NULL == param || 0 != strcmp(param, "xml")) {
        /* not us */
        *module = NULL;
        *priority = -1;
        return OPAL_SUCCESS;
    }
    
    /* was us! */
    *priority = 100;  /* only selectable upon demand */
    *module = (mca_base_module_t *)&opal_filter_xml_module;
    
    return OPAL_SUCCESS;
}
