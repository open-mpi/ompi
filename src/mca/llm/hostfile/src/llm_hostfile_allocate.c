/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "llm_hostfile.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

#include <stdio.h>

extern char *mca_llm_hostfile_filename;

ompi_list_t*
mca_llm_hostfile_allocate_resources(int jobid, int nodes, int procs)
{
    ompi_list_t *nodelist = NULL;
    int ret;

    /* start by getting the full list of available resources */
    nodelist = mca_llm_base_parse_hostfile(mca_llm_hostfile_filename);
    if (NULL == nodelist) {
        return NULL;
    }

    ret = mca_llm_base_collapse_resources(nodelist);
    if (OMPI_SUCCESS != ret) {
        mca_llm_base_deallocate(nodelist);
        return NULL;
    }

    ret = mca_llm_base_map_resources(nodes, procs, nodelist);
    if (OMPI_SUCCESS != ret) {
        mca_llm_base_deallocate(nodelist);
        return NULL;
    }

    return nodelist;
}
