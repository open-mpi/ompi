/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "mca/pcm/pcm.h"

extern mca_pcm_base_module_t mca_pcm;
ompi_list_t*
ompi_rte_allocate_resources(int jobid, int nodes, int procs)
{
    if (NULL == mca_pcm.pcm_allocate_resources) {
        return OMPI_ERROR;
    }

    return mca_pcm.pcm_allocate_resources(jobid, nodes, procs);
}


int
ompi_rte_deallocate_resources(int jobid, ompi_list_t *nodelist)
{
    if (NULL == mca_pcm.pcm_deallocate_resources) {
        return OMPI_ERROR;
    }

    return mca_pcm.pcm_deallocate_resources(jobid, nodelist);
}
