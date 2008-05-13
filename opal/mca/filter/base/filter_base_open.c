/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/include/opal/constants.h"
#include "opal/mca/filter/filter.h"
#include "opal/mca/filter/base/base.h"
#include "opal/util/output.h"

#include "opal/mca/filter/base/static-components.h"

/*
 * Globals
 */
int  opal_filter_base_output;
opal_filter_base_module_t opal_filter = {
    NULL,
    NULL,
    opal_filter_base_process
};

opal_list_t opal_filter_base_components_available;
opal_filter_base_component_t opal_filter_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_filter_base_open(void)
{
    opal_filter_base_output = opal_output_open(NULL);

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("filter", 
                                 opal_filter_base_output, 
                                 mca_filter_base_static_components,
                                 &opal_filter_base_components_available,
                                 true)) {
        return OPAL_ERROR;
    }

    
    return OPAL_SUCCESS;
}
