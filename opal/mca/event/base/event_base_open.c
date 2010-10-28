/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/event/event.h"
#include "opal/mca/event/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/event/base/static-components.h"


/*
 * Globals
 */
int opal_event_base_output = -1;
opal_list_t opal_event_components;
opal_event_base_t *opal_event_base=NULL;

int opal_event_base_open(void)
{
    int value, rc = OPAL_SUCCESS;
        
    /* Debugging / verbose output */
    mca_base_param_reg_int_name("event", "base_verbose", 
                                "Verbosity level of the event framework",
                                false, false,
                                0, &value);
    if (0 != value) {
        opal_event_base_output = opal_output_open(NULL);
    } else {
        opal_event_base_output = -1;
    }

    /* to support tools such as ompi_info, add the components
     * to a list
     */
    OBJ_CONSTRUCT(&opal_event_components, opal_list_t);
    if (OPAL_SUCCESS !=
        mca_base_components_open("event", 0,
                                 mca_event_base_static_components,
                                 &opal_event_components, true)) {
        return OPAL_ERROR;
    }

    /* init the lib */
    if (OPAL_SUCCESS != (rc = opal_event_init())) {
        return rc;
    }

    /* get our event base */
    if (NULL == (opal_event_base = opal_event_base_create())) {
        rc = OPAL_ERROR;
    }

    return rc;
}
