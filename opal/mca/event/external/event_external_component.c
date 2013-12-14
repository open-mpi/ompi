/*
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/event/event.h"

/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *opal_event_external_component_version_string =
    "OPAL event_external event MCA component version " OPAL_VERSION;


/*
 * Local function
 */
static int event_external_open(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_event_component_t mca_event_external_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_EVENT_BASE_VERSION_2_0_0,

        /* Component name and version */
        "external",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        event_external_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int event_external_open(void)
{
    /* Must have some code in this file, or the OS X linker may
       eliminate the whole file */
    return OPAL_SUCCESS;
}
