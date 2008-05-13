/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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

#include "unistd.h"

#include "opal/include/opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/filter/filter.h"
#include "opal/mca/filter/base/base.h"

int opal_filter_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    opal_filter_base_component_t *best_component = NULL;
    opal_filter_base_module_t *best_module = NULL;

   /*
    * Select the best component - it is okay if there are NO
    * components in this framework!
    */
    if( OPAL_SUCCESS == mca_base_select("filter", opal_filter_base_output,
                                        &opal_filter_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component)) {
        /* Save the winner - otherwise, we will just use the
         * default module defined in opal_filter_base_open
         */
        opal_filter_base_selected_component = *best_component;
        opal_filter = *best_module;
    }

    /* Initialize the module */
    if (NULL != opal_filter.init) {
        if (OPAL_SUCCESS != opal_filter.init()) {
            exit_status = OPAL_ERROR;
        }
    }

    return exit_status;
}
