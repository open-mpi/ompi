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
mca_pcm_rsh_allocate_resources(struct mca_pcm_base_module_1_0_0_t* me_super,
                               mca_ns_base_jobid_t jobid,
                               int nodes, int procs)
{
    mca_pcm_rsh_module_t *me = (mca_pcm_rsh_module_t*) me_super;

    return me->llm.llm_allocate_resources(jobid, nodes, procs);
}


int
mca_pcm_rsh_deallocate_resources(struct mca_pcm_base_module_1_0_0_t* me_super,
                                 mca_ns_base_jobid_t jobid,
                                 ompi_list_t *nodelist)
{
    mca_pcm_rsh_module_t *me = (mca_pcm_rsh_module_t*) me_super;

    return me->llm.llm_deallocate_resources(jobid, nodelist);
}
