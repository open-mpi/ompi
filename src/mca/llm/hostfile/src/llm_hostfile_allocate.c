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

int 
mca_llm_hostfile_allocate_resources(int jobid,
                                    int nodes,
                                    int procs,
                                    ompi_list_t **nodelist)
{
    ompi_list_t *hostlist = NULL;
    int ret;

    /* start by getting the full list of available resources */
    hostlist = mca_llm_base_parse_hostfile(mca_llm_hostfile_filename);
    if (NULL == hostlist) {
        return OMPI_ERROR;
    }

    ret = mca_llm_base_collapse_resources(hostlist);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_llm_base_map_resources(nodes, procs, hostlist);
    *nodelist = hostlist;
    return ret;
}
