/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

int
mca_llm_base_deallocate(ompi_list_t *nodelist)
{
    ompi_rte_node_allocation_t *node;
    ompi_list_item_t *item;

    while (NULL != (item = ompi_list_remove_first(nodelist))) {
        node = (ompi_rte_node_allocation_t*) item;
        OBJ_RELEASE(node);
    }

    OBJ_RELEASE(nodelist);

    return OMPI_SUCCESS;
}

