/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "mca/pcm/pcm.h"

extern mca_pcm_base_module_t *mca_pcm;

ompi_list_t*
ompi_rte_allocate_resources(mca_ns_base_jobid_t jobid, int nodes, int procs)
{
    if (NULL == mca_pcm->pcm_allocate_resources) {
        return NULL;
    }

    return mca_pcm->pcm_allocate_resources(mca_pcm, jobid, nodes, procs);
}


int
ompi_rte_deallocate_resources(mca_ns_base_jobid_t jobid, ompi_list_t *nodelist)
{
    if (NULL == mca_pcm->pcm_deallocate_resources) {
        return OMPI_ERROR;
    }

    return mca_pcm->pcm_deallocate_resources(mca_pcm, jobid, nodelist);
}
