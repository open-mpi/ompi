/*
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "common_verbs.h"

/***********************************************************************/

static bool registered = false;
static int warn_nonexistent_if_index = -1;

bool ompi_common_verbs_warn_nonexistent_if = true;

static void register_internal(void)
{
    int ival;

    warn_nonexistent_if_index =
        mca_base_param_reg_int_name("ompi_common_verbs",
                                    "warn_nonexistent_if",
                                    "Warn if non-existent devices and/or ports are specified in device include/exclude MCA parameters "
                                    "(0 = do not warn; any other value = warn)",
                                    false, false,
                                    (int) ompi_common_verbs_warn_nonexistent_if, 
                                    &ival);
    ompi_common_verbs_warn_nonexistent_if = (bool) ival;

    registered = true;
}

void ompi_common_verbs_mca_register(mca_base_component_t *component)
{
    int ival;

    if (!registered) {
        register_internal();
    }

    /* Make synonyms for the common_verbs MCA params.  Need to look up
       the value again, because a new/different value may have been
       set by the new synonym name. */
    mca_base_param_reg_syn(warn_nonexistent_if_index, component,
                           "warn_nonexistent_if", false);
    mca_base_param_lookup_int(warn_nonexistent_if_index, &ival);
    ompi_common_verbs_warn_nonexistent_if = (bool) ival;
}
