/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"
#include "orca/constants.h"

#include "orca/include/rte_orca.h"

static int parameters_registered = false;

int orca_register_params(void)
{
    if( parameters_registered ) {
        return ORCA_SUCCESS;
    }
    parameters_registered = true;

    return ORCA_SUCCESS;
}
