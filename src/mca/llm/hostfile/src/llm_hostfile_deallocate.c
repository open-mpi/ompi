/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "llm_hostfile.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

int
mca_llm_hostfile_deallocate_resources(int jobid,
                                      ompi_list_t *nodelist)
{
    ompi_list_item_t *item;

    /* pop off all the ompi_ret_node_allocatoin_t instances and free
     * them.  Their destructors will kill the
     * mca_llm_base_hostfile_data_t, who's destructor will kill the
     * mca_llm_base_hostfile_node_t instances associated with the
     * node_allocation.  In other words, everything goes "bye-bye"
     */
    while (NULL != (item = ompi_list_remove_first(nodelist))) {
        OBJ_RELEASE(item);
    }

    OBJ_RELEASE(nodelist);

    return OMPI_SUCCESS;
}
