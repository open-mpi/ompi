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
#include "orca/util/error_strings.h"

int orca_err2str(int errnum, const char **errmsg)
{
    const char *retval;
    switch (errnum) {
    /* JJH: TODO Fill in the rest of the error strings */
    default:
        retval = "Unknown error";
        break;
    }

    *errmsg = retval;
    return ORCA_SUCCESS;
}
