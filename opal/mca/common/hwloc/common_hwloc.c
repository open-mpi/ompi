/*
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/common/hwloc/common_hwloc.h"
#include "opal/mca/base/mca_base_param.h"


static bool already_registered = false;


int opal_common_hwloc_register(void)
{
    if (already_registered) {
        return OPAL_SUCCESS;
    }

    /* Register an MCA info param containing the underlying hwloc
       version */
    mca_base_param_reg_string_name("common", "hwloc_version",
                                   "Version of underlying hwloc",
                                   false, true, COMMON_HWLOC_HWLOC_VERSION,
                                   NULL);

    already_registered = true;
    return OPAL_SUCCESS;
}


