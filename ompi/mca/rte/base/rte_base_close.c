/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/mca/rte/rte.h"
#include "ompi/mca/rte/base/base.h"

int ompi_rte_base_close(void)
{
    opal_list_item_t *item;

    ompi_rte_base_inited--;

    /* no need to close the component as it was statically opened */

    /* for support of tools such as ompi_info */
    for (item = opal_list_remove_first(&ompi_rte_components);
         NULL != item; 
         item = opal_list_remove_first(&ompi_rte_components)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ompi_rte_components);

    /* Close the framework output */
    opal_output_close (ompi_rte_base_output);
    ompi_rte_base_output = -1;

    /* All done */
    return OMPI_SUCCESS;
}
