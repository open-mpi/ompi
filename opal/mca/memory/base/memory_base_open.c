/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/memory/memory.h"
#include "opal/mca/memory/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/memory/base/static-components.h"


/*
 * Globals
 */
opal_list_t opal_memory_base_components_opened;
opal_memory_base_component_2_0_0_t *opal_memory_active_component = NULL;

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_memory_base_open(void)
{
    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("memory", 0,
                                 mca_memory_base_static_components,
                                 &opal_memory_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }

    /* can only be zero or one */
    if (opal_list_get_size(&opal_memory_base_components_opened) == 1) {
        mca_base_component_list_item_t *item;
        item = (mca_base_component_list_item_t*) 
            opal_list_get_first(&opal_memory_base_components_opened);
        opal_memory_active_component = (opal_memory_base_component_2_0_0_t*)
            item->cli_component;
    }

    /* All done */
    return OPAL_SUCCESS;
}
