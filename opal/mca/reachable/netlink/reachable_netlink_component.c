/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/proc.h"
#include "opal/mca/reachable/reachable.h"
#include "reachable_netlink.h"

/*
 * Public string showing the reachable netlink component version number
 */
const char *opal_reachable_netlink_component_version_string =
    "OPAL netlink reachable MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int reachable_netlink_open(void);
static int reachable_netlink_close(void);
static int reachable_netlink_component_query(mca_base_module_t **module, int *priority);
static int component_register(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_reachable_base_component_t mca_reachable_netlink_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .base_version = {
        /* Indicate that we are a reachable v1.1.0 component (which also
           implies a specific MCA version) */

        OPAL_REACHABLE_BASE_VERSION_2_0_0,

        /* Component name and version */

        .mca_component_name = "netlink",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                              OPAL_RELEASE_VERSION),

        /* Component open and close functions */

        .mca_open_component = reachable_netlink_open,
        .mca_close_component = reachable_netlink_close,
        .mca_query_component = reachable_netlink_component_query,
        .mca_register_component_params = component_register,
    },
    /* Next the MCA v1.0.0 component meta data */
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int reachable_netlink_open(void)
{
    /* construct the component fields */

    return OPAL_SUCCESS;
}

static int reachable_netlink_close(void)
{
    return OPAL_SUCCESS;
}

static int component_register(void)
{
    return OPAL_SUCCESS;
}

static int
reachable_netlink_component_query(mca_base_module_t **module, int *priority)
{
    *priority = 50;
    *module = (mca_base_module_t *) &opal_reachable_netlink_module;
    return OPAL_SUCCESS;
}
