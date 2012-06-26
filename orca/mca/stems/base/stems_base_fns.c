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

#include "opal/util/show_help.h"
#include "opal/util/output.h"

#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"


/******************
 * Local Functions
 ******************/

/******************
 * Object stuff
 ******************/

/******************
 * Utility functions
 ******************/
int orca_stems_base_notifier_show_help(const char *filename,
                                            const char *topic, 
                                            bool want_error_header,
                                            va_list arglist)
{
    char *output = NULL;
    
    output = opal_show_help_vstring(filename,
                                    topic,
                                    want_error_header, 
                                    arglist);

    /* If nothing came back, there's nothing to do */
    if (NULL == output) {
        return ORCA_SUCCESS;
    }

    opal_output(0, "%s", output);

    return ORCA_SUCCESS;
}

/******************
 * Local Functions
 ******************/
