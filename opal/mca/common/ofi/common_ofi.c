/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <errno.h>
#include <unistd.h>

#include "common_ofi.h"

int mca_common_ofi_register_mca_variables(void)
{
    if (fi_version() >= FI_VERSION(1,0)) {
        return OPAL_SUCCESS;
    } else {
        return OPAL_ERROR;
    }
}
