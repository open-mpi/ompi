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
    mca_llm_base_deallocate(nodelist);
    return OMPI_SUCCESS;
}
