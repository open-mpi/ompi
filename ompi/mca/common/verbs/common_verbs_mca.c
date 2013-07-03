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

#include "common_verbs.h"

#include "opal/mca/base/mca_base_var.h"

/***********************************************************************/

static bool registered = false;
static int warn_nonexistent_if_index = -1;

bool ompi_common_verbs_warn_nonexistent_if = true;

static void register_internal(void)
{
    ompi_common_verbs_warn_nonexistent_if = true;
    warn_nonexistent_if_index =
      mca_base_var_register("ompi", "ompi_common", "verbs", "warn_nonexistent_if",
                            "Warn if non-existent devices and/or ports are specified in device include/exclude MCA parameters "
                            "(0 = do not warn; any other value = warn)",
                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                            &ompi_common_verbs_warn_nonexistent_if);

    registered = true;
}

void ompi_common_verbs_mca_register(mca_base_component_t *component)
{
    if (!registered) {
        register_internal();
    }

    /* Make synonym for the common_verbs MCA params. */
    mca_base_var_register_synonym(warn_nonexistent_if_index, "ompi", component->mca_type_name,
                                  component->mca_component_name, "warn_nonexistent_if", 0);
}
