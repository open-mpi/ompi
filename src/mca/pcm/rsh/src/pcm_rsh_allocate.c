/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "mca/llm/llm.h"


ompi_list_t *
mca_pcm_rsh_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                               mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
    return mca_pcm_rsh_llm.llm_allocate_resources(jobid, nodes, procs);
}


int
mca_pcm_rsh_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    return mca_pcm_rsh_llm.llm_deallocate_resources(jobid, nodelist);
}
