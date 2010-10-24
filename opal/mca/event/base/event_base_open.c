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
opal_event_module_t opal_event = {0};
opal_list_t opal_event_components;

/*
 * Only ONE event component can compile at any time, so
 * just open that one - it will be statically built
 */
int opal_event_base_open(void)
{
    int value, rc = OPAL_SUCCESS;
    mca_base_component_list_item_t *cli;
        
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
    if (NULL != mca_event_base_static_components[0]) {
        opal_event_component_t *component = 
            (opal_event_component_t*) 
            mca_event_base_static_components[0];

        /* Save it in a global list for ompi_info */
        cli = OBJ_NEW(mca_base_component_list_item_t);
        cli->cli_component = mca_event_base_static_components[0];
        opal_list_append(&opal_event_components, 
                         &cli->super);

        if (NULL != component->base_version.mca_open_component) {
            if (OPAL_SUCCESS !=component->base_version.mca_open_component()) {
                return OPAL_ERROR;
            }
        }

        /* component will have done its duty, so close it */
        if (NULL != component->base_version.mca_close_component) {
            component->base_version.mca_close_component();
        }
    }

    /* be sure to init the final module */
    if (NULL != opal_event.init) {
        rc = opal_event.init();
    }

    /* All done */

    return rc;
}

/****    EVENT OBJECT    ****/
static void ev_construct(opal_event_t *ptr)
{
    if (NULL != opal_event.construct) {
        opal_event.construct(ptr);
    }
}
static void ev_destruct(opal_event_t *ptr)
{
    if (NULL != opal_event.destruct) {
        opal_event.destruct(ptr);
    }
}
OBJ_CLASS_INSTANCE(opal_event_t,
                   opal_object_t,
                   ev_construct,
                   ev_destruct);
